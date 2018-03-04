## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 4,
  cache = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----package, echo=FALSE, results='hide'---------------------------------
library(rstanguts)
library(rstan)
library(morse)

## ----data, cache=TRUE----------------------------------------------------
# (1) load dataset
data("data_Diazinon")

## ---- OPTIONAL - using package 'morse'
# (2) : check structure and integrity of the dataset, with the package morse
survDataCheck(data_Diazinon)

# (3) OPTIONAL - using package 'morse': represent the number of survivors as a function of time
plot(survData(data_Diazinon), pool.replicate = FALSE)

# (4) check information on the experimental design
summary(survData(data_Diazinon))

## ----fit, cache=TRUE, echo=TRUE, eval=FALSE------------------------------
#  # OPTION for the number of cores:
#  options(mc.cores = 3)
#  
#  # (6) fit the TK-TD model SD
#  fit_SD_diaz <- stan_guts(data_Diazinon, model_type = "SD")
#  
#  # (7) fit the TK-TD model IT
#  fit_IT_diaz <- stan_guts(data_Diazinon, model_type = "IT")
#  
#  # (8) fit the TK-TD model PROPER with distribution 'loglogistic'
#  fit_PROPERlogLogistic_diaz <- stan_guts(data_Diazinon, model_type = "PROPER")
#  
#  # (9) fit the TK-TD model IT with distribution 'lognormal'
#  fit_ITlogNormal_diaz <- stan_guts(data_Diazinon, model_type = "IT", distribution = "lognormal")
#  
#  # (10) fit the TK-TD model PROPER with distribution 'lognormal'
#  fit_PROPERlogNormal_diaz <- stan_guts(data_Diazinon, model_type = "PROPER", distribution = "lognormal")

## ---- echo=FALSE---------------------------------------------------------
# save(fit_SD_diaz, file = "vignettes/data_fit/fit_SD_diaz.rda")
# save(fit_IT_diaz, file = "vignettes/data_fit/fit_IT_diaz.rda")
# save(fit_PROPERlogLogistic_diaz, file = "vignettes/data_fit/fit_PROPERlogLogistic_diaz.rda")
# save(fit_PROPERlogNormal_diaz, file = "vignettes/data_fit/fit_PROPERlogNormal_diaz.rda")

load(file = "data_fit/fit_SD_diaz.rda")
load(file = "data_fit/fit_IT_diaz.rda")
load(file = "data_fit/fit_PROPERlogLogistic_diaz.rda")
load(file = "data_fit/fit_PROPERlogNormal_diaz.rda")

## ----rstanGUTS, cache = TRUE---------------------------------------------
plot_stanguts(fit_SD_diaz)
plot_stanguts(fit_IT_diaz)
plot_stanguts(fit_PROPERlogNormal_diaz)
plot_stanguts(fit_PROPERlogLogistic_diaz)

## ----rstanGUTSnumber, cache = TRUE---------------------------------------
plot_stanguts(fit_SD_diaz, data_type = "Number")
plot_stanguts(fit_IT_diaz, data_type = "Number")
plot_stanguts(fit_PROPERlogNormal_diaz, data_type = "Number")
plot_stanguts(fit_PROPERlogLogistic_diaz, data_type = "Number")

## ----stanfit, cache=TRUE-------------------------------------------------
stanfit_SD_diaz <- stanguts_to_stanfit(fit_SD_diaz)
stanfit_IT_diaz <- stanguts_to_stanfit(fit_IT_diaz)
stanfit_PROPERlogNormal_diaz <- stanguts_to_stanfit(fit_PROPERlogNormal_diaz)
stanfit_PROPERlogLogistic_diaz <- stanguts_to_stanfit(fit_PROPERlogLogistic_diaz)

## ----pairs, cache=TRUE---------------------------------------------------
pairs(stanfit_SD_diaz, pars = c("hb_log10", "kd_log10", "z_log10", "kk_log10"))
pairs(stanfit_IT_diaz, pars = c("hb_log10", "kd_log10", "alpha_log10", "beta_log10"))
pairs(stanfit_PROPERlogNormal_diaz, pars = c("hb_log10", "kd_log10", "kk_log10", "alpha_log10", "beta_log10"))
pairs(stanfit_PROPERlogLogistic_diaz, pars = c("hb_log10", "kd_log10", "kk_log10", "alpha_log10", "beta_log10"))

## ----print, eval=FALSE---------------------------------------------------
#  library(shinystan)
#  launch_shinystan(stanfit_SD_diaz)
#  launch_shinystan(stanfit_IT_diaz)
#  launch_shinystan(stanfit_PROPERlogNormal_diaz)
#  launch_shinystan(stanfit_PROPERlogLogistic_diaz)

## ----survFit, cache=TRUE-------------------------------------------------
survFit_SD_diaz <- stanguts_to_survFit(fit_SD_diaz)
survFit_IT_diaz <- stanguts_to_survFit(fit_IT_diaz)
survFit_PROPERlogNormal_diaz <- stanguts_to_survFit(fit_PROPERlogNormal_diaz)
survFit_PROPERlogLogistic_diaz <- stanguts_to_survFit(fit_PROPERlogLogistic_diaz)

## ----survFitsummary, cache=TRUE------------------------------------------
summary(survFit_SD_diaz)
summary(survFit_IT_diaz)
summary(survFit_PROPERlogNormal_diaz)
summary(survFit_PROPERlogLogistic_diaz)

## ----survFitplot, cache=TRUE---------------------------------------------
plot(survFit_SD_diaz)
plot(survFit_IT_diaz)

# PROPER models are not include in morse version < 3.1.0
# plot(survFit_PROPERlogNormal_diaz)
# plot(survFit_PROPERlogLogistic_diaz)

## ---- cache=TRUE---------------------------------------------------------
ppc(survFit_SD_diaz)
ppc(survFit_IT_diaz)

# PROPER models are not include in morse version < 3.1.0
# ppc(survFit_PROPERlogNormal_diaz)
# ppc(survFit_PROPERlogLogistic_diaz)

## ----cstSDLCx, cache=TRUE------------------------------------------------
# LC50 at the maximum time-point:
LC50_SD_diaz <- LCx(survFit_SD_diaz, X = 50)
plot(LC50_SD_diaz)
# LC30 at the maximum time-4:
LC30t4_SD_diaz <- LCx(survFit_SD_diaz, X = 30, time_LCx = 4)
plot(LC30t4_SD_diaz)

# IT
LC50_IT_diaz <- LCx(survFit_IT_diaz, X = 50)
plot(LC50_IT_diaz)

# PROPER models are not include in morse version < 3.1.0
# # PROPER log-Normal
# LC50_PROPERlogNormal_diaz <- LCx(survFit_PROPERlogNormal_diaz, X = 50)
# plot(LC50_PROPERlogNormal_diaz)
# 
# # PROPER log-Logistic
# LC50_PROPERlogLogistic_diaz <- LCx(survFit_PROPERlogLogistic_diaz, X = 50)
# plot(LC50_PROPERlogLogistic_diaz)

