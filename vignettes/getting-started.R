## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 4,
  cache = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----plotRate, cache = TRUE----------------------------------------------
# Rate of survival as a function of time
plot_stanguts(fit_SD_diaz)

## ----plotRate2, eval=FALSE-----------------------------------------------
#  plot_stanguts(fit_IT_diaz)
#  plot_stanguts(fit_IT_lN_diaz)
#  plot_stanguts(fit_PROPER_lN_diaz)
#  plot_stanguts(fit_PROPER_ll_diaz)

## ----plotNumber, cache = TRUE--------------------------------------------
# Number of survivors as a function of time
plot_stanguts(fit_SD_diaz, data_type = "Number")

## ----plotNumber2, cache = TRUE, eval=FALSE-------------------------------
#  plot_stanguts(fit_IT_diaz, data_type = "Number")
#  plot_stanguts(fit_IT_lN_diaz, data_type = "Number")
#  plot_stanguts(fit_PROPER_lN_diaz, data_type = "Number")
#  plot_stanguts(fit_PROPER_ll_diaz, data_type = "Number")

## ----stanfit, cache=TRUE, eval=FALSE-------------------------------------
#  stanfit_SD_diaz <- stanguts_to_stanfit(fit_SD_diaz)
#  stanfit_IT_diaz <- stanguts_to_stanfit(fit_IT_diaz)
#  stanfit_IT_lN_diaz <- stanguts_to_stanfit(fit_IT_lN_diaz)
#  stanfit_PROPER_lN_diaz <- stanguts_to_stanfit(fit_PROPER_lN_diaz)
#  stanfit_PROPER_ll_diaz <- stanguts_to_stanfit(fit_PROPER_ll_diaz)

## ----pairsRstan, cache=TRUE----------------------------------------------
## Note: to reduce the size of the vignettes, we do not save previous object from 'stanguts_to_stanfit()' function, so we recall this function in pairs plots.
# ---
pairs(stanguts_to_stanfit(fit_SD_diaz),
      pars = c("hb_log10", "kd_log10", "z_log10", "kk_log10"))

## ----pairsRstan2, eval=FALSE---------------------------------------------
#  pairs(stanguts_to_stanfit(fit_IT_diaz),
#        pars = c("hb_log10", "kd_log10", "alpha_log10", "beta_log10"))
#  pairs(stanguts_to_stanfit(fit_IT_lN_diaz),
#        pars = c("hb_log10", "kd_log10", "alpha_log10", "beta_log10"))
#  pairs(stanguts_to_stanfit(fit_PROPER_lN_diaz),
#        pars = c("hb_log10", "kd_log10", "kk_log10", "alpha_log10", "beta_log10"))
#  pairs(stanguts_to_stanfit(fit_PROPER_ll_diaz),
#        pars = c("hb_log10", "kd_log10", "kk_log10", "alpha_log10", "beta_log10"))

## ----print, eval=FALSE---------------------------------------------------
#  library(shinystan)
#  launch_shinystan(stanfit_SD_diaz)
#  launch_shinystan(stanfit_IT_diaz)
#  launch_shinystan(stanfit_PROPER_lN_diaz)
#  launch_shinystan(stanfit_PROPER_ll_diaz)

## ----survFit, cache=TRUE, eval=FALSE-------------------------------------
#  survFit_SD_diaz <- stanguts_to_survFit(fit_SD_diaz)
#  survFit_IT_diaz <- stanguts_to_survFit(fit_IT_diaz)
#  survFit_PROPER_lN_diaz <- stanguts_to_survFit(fit_PROPER_lN_diaz)
#  survFit_PROPER_ll_diaz <- stanguts_to_survFit(fit_PROPER_ll_diaz)

