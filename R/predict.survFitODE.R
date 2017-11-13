#' Prediction
#' 
#' @param x An object of class \code{survFitODE}
#' @param new_data A new data set with concentration time point (a data.frame with 2 column,
#'  time in the first column and concentration in the second)
#' 
#' @import deSolve
#'    
#' @export
#' 
#' 
predict.survFitODE <- function(x, new_data = NULL,
                               interpolate_length = 1e2,
                               interpolate_method = "linear"){

    if(is.null(new_data)){
      # run_odes on profiles used for estimations

      dataStan <-  x$dataStan

      new_data <- data.frame(conc = dataStan$conc,
                             time = dataStan$tconc,
                             replicate = dataStan$replicate_conc)

      new_data_list = split(new_data, new_data$replicate)

    } else{
      new_data_list = list(new_data = new_data)
    }

    # list_ODES <- lapply(1:n_replicate, function(it)
    #                     run_odes(x, new_data_list[[it]]))
    
    n_replicate <- length(unique(new_data$replicate))
  
    list_ODEs <- lapply(1:n_replicate,
                        function(it) {
                          solve_TKTD(x,
                                     new_data_list[[it]],
                                     interpolate_length = interpolate_length,
                                     interpolate_method = interpolate_method)
                        })
    
    names(list_ODEs) <- unique(new_data$replicate)
    return(list_ODEs)

    # if(x$model_type == "SD"){
    #   Nsurv_hat = exp( - odes_out)
    # }
    # if(x$model_type == "IT"){
    #   Nsurv_hat = binomial()
    # }
    #
    # return(list(psurv_hat = psurv_hat,
    #             Nsurv_hat = Nsurv_hat))

}

# -------

#' Run odes with the stan integrator
#'
#' @export
#' 
#' 
run_odes <- function(x, new_data){

  # This is usefull to load the function ode_TKTD_varSD which has been implemented  
  if(x$model_type == "SD"){
    stanEstim <- extract(x$stanfit, pars = c("hb_log10", "kd_log10", "z_log10", "kk_log10"))
  }
  if(x$model_type == "IT"){
    stanEstim <- extract(x$stanfit, pars = c("hb_log10", "kd_log10", "alpha_log10", "beta_log10"))
  }
  df_stanEstim <- as.data.frame(stanEstim)

  time_Predict <- seq(0, max(new_data[,1]), length.out = 1e2)
  
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

#' solver
#' 
#' 


#' Forecasting ith ODE solver


# ----------------------------

model_SD <- function(t, State, parms, input)  {
  with(as.list(c(parms, State)), {
    
    C = rep(input(t), length = n_size)
    
    D = State[1:n_size]
    H = State[(n_size+1):(2*n_size)] 
    
    dD <- kd*(C - D)     # internal concentration
    dH <- kk*pmax(0, D-z) + hb # risk function
    
    res <- c(dD, dH)
    list(res, signal = input(t))
    
  })
}

model_IT <- function(t, State, parms, input) {
  with(as.list(c(parms, State)), {
    
    C = rep(input(t), length = n_size)
    
    D = State[1:n_size]
    
    dD <- kd*(C - D)    # internal concentration
    
    list(dD = dD, signal = input(t))
    
  })
}

solve_TKTD <- function(x, 
                       new_data,
                       interpolate_length = 1e3,
                       interpolate_method = "linear"){
  
  model_type = x$model_type

  MCMC_stanEstim <- extract_MCMCparameters(x)
  n_size <- nrow(MCMC_stanEstim)
  ## external signal with several rectangle impulses
  signal <- data.frame(times = new_data$time, 
                       import = new_data$conc)
  
  sigimp <- approxfun(signal$times, signal$import, method = interpolate_method, rule = 2)
  
  if(!is.null(interpolate_length)){
    times = seq(min(new_data$time),max(new_data$time), length = interpolate_length)
  } else{
    times = signal$times
  }
  
  ## model
  kd = 10^MCMC_stanEstim$kd_log10
  hb = 10^MCMC_stanEstim$hb_log10
    
  if(model_type == "IT"){

      alpha = 10^MCMC_stanEstim$alpha_log10
      beta = 10^MCMC_stanEstim$beta_log10
    
    ## The parameters
    parms  <- list( kd = kd,
                    alpha = alpha,
                    beta = beta,
                    n_size = n_size)
    
    ## Start values for steady state
    xstart <- c(D = rep(0,n_size))
    
    ## Solve model
    out <- ode(y = xstart,
               times = times,
               func = model_IT,
               parms,
               input = sigimp)
    
  }
  if (model_type == "SD"){

      kk = 10^MCMC_stanEstim$kk_log10
      z = 10^MCMC_stanEstim$z_log10
    
    ## The parameters
    parms  <- list( kd = kd,
                    hb = hb,
                    kk = kk,
                    z = z,
                    n_size = n_size)
    
    ## Start values for steady state
    xstart <- c(D = rep(0,n_size),
                H = rep(0,n_size))
    
    ## Solve model
    out <- ode(y = xstart,
               times = times,
               func = model_SD,
               parms,
               input = sigimp)
  }
  
  if(model_type == "IT"){
    D_matrix = as.data.frame(out) %>%
      dplyr::select(-c(time,signal))%>%
      as.matrix()
    S <- 1-plogis(log(t(D_matrix)), location = log(parms$alpha), scale = 1/parms$beta)
    dtheo <- t(S)
    
  }
  if(model_type=="SD"){
    
    H_matrix = as.data.frame(out) %>%
      dplyr::select(contains("H"),-c(time,signal))%>%
      as.matrix()
    S <- exp(-H_matrix)
    dtheo <- S
  }

  # -------- df.theo
  df <- data_frame(time = out[, "time"],
                  conc = out[, "signal"],
                  replicate = rep(unique(new_data$replicate), length(out[, "time"])),
                  q50 = apply(dtheo, 1, quantile, probs = 0.5, na.rm = TRUE),
                  qinf95 = apply(dtheo, 1, quantile, probs = 0.025, na.rm = TRUE),
                  qsup95 = apply(dtheo, 1, quantile, probs = 0.975, na.rm = TRUE))
  ## RETURN
  return(df)
}




