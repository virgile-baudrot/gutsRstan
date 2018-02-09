library(rstanTKTD)
load(file = "data/Car_cst.rda")
load(file = "data/Cyp_cst.rda")
load(file = "data/Mal_cst.rda")
load(file = "data/Dim_cst.rda")
load(file = "data/PRZ_cst.rda")
load(file = "data/Car_var.rda")
load(file = "data/Cyp_var.rda")
load(file = "data/Mal_var.rda")
load(file = "data/Dim_var.rda")
load(file = "data/PRZ_var.rda")



# -----------------------------------------------------------------------------
compare_param <- function(x, y){
  
  # For rstanTKTD model
  df_stanEstim <- extract_MCMCparameters(x)
  
  parameters <- switch(x$model_type,
                       SD =  c("kd", "hb", "z", "kk"),
                       IT =  c("kd", "hb", "alpha", "beta"),
                       PROPER =  c("kd", "hb", "kk", "alpha", "beta"))
  df_x <- data.frame(parameters = parameters,
                     median = as.numeric(10^apply(df_stanEstim, 2, quantile, 0.5)),
                     Q2.5 = as.numeric(10^apply(df_stanEstim, 2, quantile, 0.025)),
                     Q97.5 = as.numeric(10^apply(df_stanEstim, 2, quantile, 0.975)))
  df_x$model = rep("rstanTKTD", nrow(df_x))
  
  # For morse model
  
  df_y <- y$estim.par
  df_y$model = rep("morse", nrow(df_y))
  
  df_xy <- dplyr::bind_rows(df_x,df_y)
  
  plt <- ggplot(data = df_xy,
                aes(x = parameters, y = median, ymin = Q2.5, ymax = Q97.5, color = model)) +
    theme_light() +
    scale_y_log10() +
    geom_pointrange(position = position_dodge(width = 0.5))
  
  return(plt)
}

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# quick test
options(mc.cores = 1)
### IT2
fit_Dim_cstIT2 <- stan_guts(Dim_cst, model_type = "IT", iter = 2000, warmup=500, chains = 3,
                            adapt_delta = 0.95,
                            rel_tol = 1e-8,
                            abs_tol = 1e-8,
                            max_num_steps = 1e2,
                            ode_integrator = "rk45" # other is "bdf"
                            )
save(fit_Car_cstIT2, file = "tests/testthat/tests/testthat/fit_Dim_cstIT2.rda")

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# -------------------------------------- Car
### FIT MODEL
options(mc.cores = 3)
### IT2
fit_Car_cstIT2 <- stan_guts(Car_cst, model_type = "IT", iter = 2000, warmup=500, chains = 3)
save(fit_Car_cstIT2, file = "tests/testthat/tests/testthat/fit_Car_cstIT2.rda")
fit_Car_varIT2 <- stan_guts(Car_var, model_type = "IT", iter = 2000, warmup=500, chains = 3) 
save(fit_Car_varIT2, file = "tests/testthat/tests/testthat/fit_Car_varIT2.rda")

load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Car_cstIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Car_varIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Car_cstIT_MORSE.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Car_varIT_MORSE.rda")

library(morse)
# cst
rstanTKTD::ppc(fit_Car_cstIT)
rstanTKTD::ppc(fit_Car_cstIT2)
morse::ppc(fit_Car_cstIT_MORSE)
compare_param(fit_Car_cstIT, fit_Car_cstIT_MORSE)
compare_param(fit_Car_cstIT2, fit_Car_cstIT_MORSE)
# var
rstanTKTD::ppc(fit_Car_varIT)
rstanTKTD::ppc(fit_Car_varIT2)
morse::ppc(fit_Car_varIT_MORSE)
compare_param(fit_Car_varIT, fit_Car_varIT_MORSE)
compare_param(fit_Car_varIT2, fit_Car_varIT_MORSE)


# -------------------------------------- Cyp
### FIT MODEL
options(mc.cores = 3)
fit_Cyp_cstIT2 <- stan_guts(Cyp_cst, model_type = "IT", iter = 2000, warmup=500, chains = 3) 
save(fit_Cyp_cstIT2, file = "tests/testthat/fit_Cyp_cstIT2.rda")
fit_Cyp_varIT2 <- stan_guts(Cyp_var, model_type = "IT", iter = 2000, warmup=500, chains = 3) 
save(fit_Cyp_varIT2, file = "tests/testthat/fit_Cyp_varIT2.rda")

load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Cyp_cstIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Cyp_varIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Cyp_cstIT_MORSE.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Cyp_varIT_MORSE.rda")
load(file = "tests/testthat/fit_Cyp_cstIT2.rda")
# cst
rstanTKTD::ppc(fit_Cyp_cstIT)
rstanTKTD::ppc(fit_Cyp_cstIT2)
morse::ppc(fit_Cyp_cstIT_MORSE)
compare_param(fit_Cyp_cstIT, fit_Cyp_cstIT_MORSE)
compare_param(fit_Cyp_cstIT2, fit_Cyp_cstIT_MORSE)
# var
rstanTKTD::ppc(fit_Cyp_varIT)
rstanTKTD::ppc(fit_Cyp_varIT2)
morse::ppc(fit_Cyp_varIT_MORSE)
compare_param(fit_Cyp_varIT, fit_Cyp_varIT_MORSE)
compare_param(fit_Cyp_varIT2, fit_Cyp_varIT_MORSE)

# -------------------------------------- Dim
### FIT MODEL
options(mc.cores = 3)

fit_Dim_cstIT2 <- stan_guts(Dim_cst, model_type = "IT", iter = 2000, warmup=500, chains = 3)
save(fit_Dim_cstIT2, file = "tests/testthat/fit_Dim_cstIT2.rda")
fit_Dim_varIT2 <- stan_guts(Dim_var, model_type = "IT", iter = 2000, warmup=500, chains = 3)
save(fit_Dim_varIT2, file = "tests/testthat/fit_Dim_varIT2.rda")

load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Dim_cstIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Dim_varIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Dim_cstIT_MORSE.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Dim_varIT_MORSE.rda")
load(file = "tests/testthat/fit_Dim_cstIT2.rda")
load(file = "tests/testthat/fit_Dim_varIT2.rda")
# cst
rstanTKTD::ppc(fit_Dim_cstIT)
rstanTKTD::ppc(fit_Dim_cstIT2)
morse::ppc(fit_Dim_cstIT_MORSE)
compare_param(fit_Dim_cstIT, fit_Dim_cstIT_MORSE)
compare_param(fit_Dim_cstIT2, fit_Dim_cstIT_MORSE)
# var
rstanTKTD::ppc(fit_Dim_varIT)
rstanTKTD::ppc(fit_Dim_varIT2)
morse::ppc(fit_Dim_varIT_MORSE)
compare_param(fit_Dim_varIT, fit_Dim_varIT_MORSE)
compare_param(fit_Dim_varIT2, fit_Dim_varIT_MORSE)

# -------------------------------------- Mal
### FIT MODEL
options(mc.cores = 3)

fit_Mal_cstIT2 <- stan_guts(Mal_cst, model_type = "IT", iter = 2000, warmup=500, chains = 3)
save(fit_Mal_cstIT2, file = "tests/testthat/fit_Mal_cstIT2.rda")
fit_Mal_varIT2 <- stan_guts(Mal_var, model_type = "IT", iter = 2000, warmup=500, chains = 3) 
save(fit_Mal_varIT2, file = "tests/testthat/fit_Mal_varIT2.rda")

load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Mal_cstIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Mal_varIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Mal_cstIT_MORSE.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_Mal_varIT_MORSE.rda")
load(file = "tests/testthat/fit_Mal_cstIT2.rda")
# cst
rstanTKTD::ppc(fit_Mal_cstIT)
rstanTKTD::ppc(fit_Mal_cstIT2)
morse::ppc(fit_Mal_cstIT_MORSE)
compare_param(fit_Mal_cstIT, fit_Mal_cstIT_MORSE)
compare_param(fit_Mal_cstIT2, fit_Mal_cstIT_MORSE)
# var
rstanTKTD::ppc(fit_Mal_varIT)
rstanTKTD::ppc(fit_Mal_varIT2)
morse::ppc(fit_Mal_varIT_MORSE)
compare_param(fit_Mal_varIT, fit_Mal_varIT_MORSE)
compare_param(fit_Mal_varIT2, fit_Mal_varIT_MORSE)

# -------------------------------------- PRZ
### FIT MODEL
options(mc.cores = 3)

fit_PRZ_cstIT2 <- stan_guts(PRZ_cst, model_type = "IT", iter = 2000, warmup=500, chains = 3)
save(fit_PRZ_cstIT2, file = "tests/testthat/fit_PRZ_cstIT2.rda")
fit_PRZ_varIT2 <- stan_guts(PRZ_var, model_type = "IT", iter = 2000, warmup=500, chains = 3)
save(fit_PRZ_varIT2, file = "tests/testthat/fit_PRZ_varIT2.rda")

load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_PRZ_cstIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_PRZ_varIT.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_PRZ_cstIT_MORSE.rda")
load(file = "../PostDoc-LBBE/Projet_varyingExposureGUTS/DATA_SIMULATION/fit_cluster/fit_PRZ_varIT_MORSE.rda")
load(file = "tests/testthat/fit_PRZ_cstIT2.rda")
load(file = "tests/testthat/fit_PRZ_varIT2.rda")
# cst
rstanTKTD::ppc(fit_PRZ_cstIT)
rstanTKTD::ppc(fit_PRZ_cstIT2)
morse::ppc(fit_PRZ_cstIT_MORSE)
compare_param(fit_PRZ_cstIT, fit_PRZ_cstIT_MORSE)
compare_param(fit_PRZ_cstIT2, fit_PRZ_cstIT_MORSE)
# var
rstanTKTD::ppc(fit_PRZ_varIT)
rstanTKTD::ppc(fit_PRZ_varIT2)
morse::ppc(fit_PRZ_varIT_MORSE)
compare_param(fit_PRZ_varIT, fit_PRZ_varIT_MORSE)
compare_param(fit_PRZ_varIT2, fit_PRZ_varIT_MORSE)

plot(fit_PRZ_varIT2)



# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# ROUGH TEST IT2

library(rstan)
options(mc.cores = 1)

ode_controlVal <- list(rel_tol = 1e-6, abs_tol = 1e-6, max_num_steps = 1e6)
dataStan_withReplicate <- rstanTKTD::modelDataStan(data = Cyp_cst, model_type = "IT", ode_controlVal)
dataStan <- dataStan_withReplicate
dataStan$replicate_conc = NULL
dataStan$replicate_Nsurv = NULL
dataStan$Ninit = NULL

stan(file = "tests/testthat/IT2_test.stan",
     data = dataStan,
     iter = 2000,
     warmup = 500,
     chains = 1)


