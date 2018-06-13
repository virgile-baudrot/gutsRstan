## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 4,
  cache = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----pairsRstan, cache=TRUE----------------------------------------------
## Note: to reduce the size of the vignettes, we do not save previous object from 'stanguts_to_stanfit()' function, so we recall this function in pairs plots.
# ---
pairs(stanguts_to_stanfit(fit_SD_diaz),
      pars = c("hb_log10", "kd_log10", "z_log10", "kk_log10"))
pairs(stanguts_to_stanfit(fit_IT_diaz),
      pars = c("hb_log10", "kd_log10", "alpha_log10", "beta_log10"))
pairs(stanguts_to_stanfit(fit_IT_lN_diaz),
      pars = c("hb_log10", "kd_log10", "alpha_log10", "beta_log10"))
pairs(stanguts_to_stanfit(fit_PROPER_lN_diaz),
      pars = c("hb_log10", "kd_log10", "kk_log10", "alpha_log10", "beta_log10"))
pairs(stanguts_to_stanfit(fit_PROPER_ll_diaz),
      pars = c("hb_log10", "kd_log10", "kk_log10", "alpha_log10", "beta_log10"))

## ----survFit, cache=TRUE, eval=FALSE-------------------------------------
#  survFit_SD_diaz <- stanguts_to_survFit(fit_SD_diaz)
#  survFit_IT_diaz <- stanguts_to_survFit(fit_IT_diaz)
#  survFit_PROPER_lN_diaz <- stanguts_to_survFit(fit_PROPER_lN_diaz)
#  survFit_PROPER_ll_diaz <- stanguts_to_survFit(fit_PROPER_ll_diaz)

## ----survFitsummary, cache=TRUE------------------------------------------
## Note: to reduce the size of the vignettes, we do not save previous object from 'stanguts_to_survFit()' function, so we recall this function
# ---
summary(stanguts_to_survFit(fit_SD_diaz))
summary(stanguts_to_survFit(fit_IT_diaz))
# PROPER models are not include in morse version < 3.X.X
# summary(survFit_PROPER_lN_diaz)
# summary(survFit_PROPER_ll_diaz)

## ----survFitplot, cache=TRUE---------------------------------------------
plot(stanguts_to_survFit(fit_SD_diaz))
plot(stanguts_to_survFit(fit_IT_diaz))

# PROPER models are not include in morse version < 3.X.X
# plot(survFit_PROPER_lN_diaz)
# plot(survFit_PROPER_ll_diaz)

## ---- cache=TRUE---------------------------------------------------------
ppc(stanguts_to_survFit(fit_SD_diaz))
ppc(stanguts_to_survFit(fit_IT_diaz))
# PROPER models are not include in morse version < 3.X.X
# ppc(survFit_PROPER_lN_diaz)
# ppc(survFit_PROPER_ll_diaz)

## ----cstSDLCx, cache=TRUE------------------------------------------------
# LC50 at the maximum time-point:
LC50_SD_diaz <- LCx(stanguts_to_survFit(fit_SD_diaz),
                    X = 50)
plot(LC50_SD_diaz)
# LC30 at the maximum time-4:
LC30t4_SD_diaz <- LCx(stanguts_to_survFit(fit_SD_diaz),
                      X = 30, time_LCx = 4)
plot(LC30t4_SD_diaz)

# IT
LC50_IT_diaz <- LCx(stanguts_to_survFit(fit_IT_diaz), X = 50)
plot(LC50_IT_diaz)

# PROPER models are not include in morse version < 3.X.X
# # PROPER log-Normal
# LC50_PROPER_lN_diaz <- LCx(survFit_PROPER_lN_diaz, X = 50)
# plot(LC50_PROPER_lN_diaz)
# 
# # PROPER log-Logistic
# LC50_PROPER_ll_diaz <- LCx(survFit_PROPER_ll_diaz, X = 50)
# plot(LC50_PROPER_ll_diaz)

## ----cstSDMFx, cache=TRUE------------------------------------------------
# MF50 at the maximum time-point:
MF50_SD_diaz <- MFx(stanguts_to_survFit(fit_SD_diaz),
                    data_predict = data_4predict, X = 50)
plot(MF50_SD_diaz)
# MF30 at the maximum time-4:
MF30t4_SD_diaz <- MFx(stanguts_to_survFit(fit_SD_diaz),
                      data_predict = data_4predict, X = 30, time_MFx = 4)
plot(MF30t4_SD_diaz)

# IT
MF50_IT_diaz <- MFx(stanguts_to_survFit(fit_IT_diaz),
                    data_predict = data_4predict, X = 50)
plot(MF50_IT_diaz)

# PROPER models are not include in morse version < 3.X.X
# # PROPER log-Normal
# MF50_PROPER_lN_diaz <- MFx(survFit_PROPER_lN_diaz, X = 50)
# plot(MF50_PROPER_lN_diaz)
#
# # PROPER log-Logistic
# MF50_PROPER_ll_diaz <- MFx(survFit_PROPER_ll_diaz, X = 50)
# plot(MF50_PROPER_ll_diaz)

