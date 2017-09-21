#' PREDICTION
#' 
#' @param x An object of class \code{survFitODE}
#' @param new_data A new data set with concentration time point (a data.frame with 2 column,
#'  time in the first column and concentration in the second)
#'  

predict.survFitODE <- function(x, new_data){
  

    tconcPredict <- new_data[,1]
    concPredict <- new_data[,2]
    # initial time must be less than t0 = 0, so we use a very small close small number -1e-8
    y_predict[1:100,1:2] = solve_TKTD_varSD(y0, -1e-9, tPredict[1:100], param, tconcPredict[1:100], concPredict[1:100])
    
    Psurv_hat[1:100] = exp( - y_predict[1:100, 2])
    
    for(i in 1:100){
      Conditional_Psurv_hat[i] =  ifelse(i == 1 , Psurv_hat[1] , Psurv_hat[i] / Psurv_hat[i-1])
      
      Nsurv_predict[i] = binomial_rng(Nprec[i], Conditional_Psurv_hat[i])
    }
    
    
  
}

# ----------

load(file = "tests/testthat/fit_varIT.rda")
conc <- c(1,5,2,6,3,7,4,8,5,9,6)
tconc <- 0:10 # DOIT COMMENCER A 0
new_data_test = data.frame(tconc, conc)

odes_out <- run_odes(x = fit_varIT, new_data  = new_data_test)

# solve_ODE_for all parmaters
expose_stan_functions("exec/ode_TKTD_varIT.stan")

run_odes <- function(x, new_data){

  # This is usefull to load the function ode_TKTD_varSD which has been implemented  
  if(x$model_type == "SD"){
    stanEstim <- extract(x$stanfit, pars = c("hb_log10", "kd_log10", "z_log10", "kk_log10"))
  }
  if(x$model_type == "IT"){
    stanEstim <- extract(x$stanfit, pars = c("hb_log10", "kd_log10", "alpha_log10", "beta_log10"))
  }
  df_stanEstim <- as.data.frame(stanEstim)

  time_Predict <- seq(0, max(new_data[,1]), length.out = 100)
  
  if(x$model_type == "SD"){
    y0 = c(0, 0) # initial dose
    ls_ode <- lapply(1:nrow(df_stanEstim), function(it) 
      solve_TKTD_varSD(y0, -1e-9, time_Predict, as.numeric(df_stanEstim[it,]), new_data[,1], new_data[,2]))
  }
  if(x$model_type == "IT"){
    y0 = 0
    ls_ode <- lapply(1:nrow(df_stanEstim), function(it) 
      solve_TKTD_varIT(y0, -1e-9, time_Predict, as.numeric(df_stanEstim[it,]), new_data[,1], new_data[,2]))
  }
  
  return(ls_ode)
}

