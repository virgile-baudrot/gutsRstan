#' Posterior predictive check plot
#' 
#' Plots posterior predictive check for \code{stanTKTD} objects.
#' 
#' @param x an object used to select a method
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @export
#' 
posterior_predict <- function(x, ...){
  UseMethod("posterior_predict")
}


#' Draw from posterior predictive distribution
#' 
#' The posterior predictive distribution is the distribution of the outcome
#' implied by the model after using the observed data to update our beliefs
#' about the unknown parameters in the model. Simulating data from the posterior
#' predictive distribution using the observed predictors is useful for checking
#' the fit of the model. Drawing from the posterior predictive distribution at
#' interesting values of the predictors also lets us visualize how a
#' manipulation of a predictor affects (a function of) the outcome(s). With new
#' observations of predictor variables we can use the posterior predictive
#' distribution to generate predicted outcomes.
#' #'
#' @aliases posterior_predict
#' @export
#' 
#' @param x An object of class \code{stanTKTD}
#' @param newdata Optionally, a data frame in which to look for variables with
#'   which to predict. This must be a \code{data.frame} with three columns 
#'   \code{time}, \code{conc} and \code{replicate}.
#'   If omitted, the model dataset is used. If \code{newdata}
#'   is provided and any variables were transformed (e.g. rescaled) in the data
#'   used to fit the model, then these variables must also be transformed in
#'   \code{newdata}. This only applies if variables were transformed before
#'   passing the data to one of the modeling functions and \emph{not} if
#'   transformations were specified inside the model formula. Also see the Note
#'   section below for a note about using the \code{newdata} argument with
#'   binomial models.
#' @param draws An integer indicating the number of draws to return. The default
#'   and maximum number of draws is the size of the posterior sample.
#' @param interpolate_length An interger with the length of interpolation. 
#' Default is 100.
#' @param interpolate_method A method for interpolation to be used. See 
#' function \code{\link[stats]{approxfun}}. Choices are "\code{linear}" or
#'  "\code{constant}", default is "\code{linear}".
#' 
#' @import deSolve
#'    
#' @export
#' 
#' 
posterior_predict.stanTKTD <- function(x,
                                       newdata = NULL,
                                       draws = NULL,
                                       interpolate_length = 1e2,
                                       interpolate_method = "linear"){

    if(is.null(newdata)){
      dataStan <-  x$dataStan
      newdata <- data.frame(conc = dataStan$conc,
                            time = dataStan$tconc,
                            replicate = dataStan$replicate_conc)
    }
  
    newdata_list = split(newdata, newdata$replicate)
    n_replicate <- length(unique(newdata$replicate))
  
    pp_list_solveTKTD <- lapply(1:n_replicate,
                                function(it) {
                                  solveTKTD(x,
                                            newdata_list[[it]],
                                            draws = draws,
                                            interpolate_length = interpolate_length,
                                            interpolate_method = interpolate_method)
                                })
    
    names(pp_list_solveTKTD) <- unique(newdata$replicate)
    
    structure(pp_list_solveTKTD, class = c("ppd", class(pp_list_solveTKTD)))
    
    return(pp_list_solveTKTD)
}

# internal --------------------------------------------------------------------

# SD model solver

model_SD <- function(t, State, parms, input)  {
  with(as.list(c(parms, State)), {
    
    C = rep(input(t), length = draws)
    
    D = State[1:draws]
    H = State[(draws+1):(2*draws)] 
    
    dD <- kd*(C - D)     # internal concentration
    dH <- kk*pmax(0, D-z) + hb # risk function
    
    res <- c(dD, dH)
    list(res, signal = input(t))
    
  })
}

# IT model solver

model_IT <- function(t, State, parms, input) {
  with(as.list(c(parms, State)), {
    
    C = rep(input(t), length = draws)
    
    D = State[1:draws]
    
    dD <- kd*(C - D)    # internal concentration
    
    list(dD = dD, signal = input(t))
    
  })
}

# solver TKTD with deSolve
solveTKTD <- function(x,
                      newdata = NULL,
                      draws = NULL,
                      interpolate_length = 1e3,
                      interpolate_method = "linear"){
  
  MCMC_stanEstim <- extract_MCMCparameters(x)
  
  if(is.null(draws)){
    draws <- nrow(MCMC_stanEstim)
  }
  
  ## external signal with several rectangle impulses
  signal <- data.frame(times = newdata$time, 
                       import = newdata$conc)
  
  sigimp <- approxfun(signal$times, signal$import, method = interpolate_method, rule = 2)
  
  if(!is.null(interpolate_length)){
    times = seq(min(newdata$time),max(newdata$time), length = interpolate_length)
  } else{
    times = signal$times
  }
  
  ## model
  kd = 10^MCMC_stanEstim$kd_log10
  hb = 10^MCMC_stanEstim$hb_log10
    
  if(x$model_type == "IT"){

      alpha = 10^MCMC_stanEstim$alpha_log10
      beta = 10^MCMC_stanEstim$beta_log10
    
    ## The parameters
    parms  <- list( kd = kd,
                    alpha = alpha,
                    beta = beta,
                    draws = draws)
    
    ## Start values for steady state
    xstart <- c(D = rep(0,draws))
    
    ## Solve model
    out <- ode(y = xstart,
               times = times,
               func = model_IT,
               parms,
               input = sigimp)
    
  }
  if (x$model_type == "SD"){

      kk = 10^MCMC_stanEstim$kk_log10
      z = 10^MCMC_stanEstim$z_log10
    
    ## The parameters
    parms  <- list( kd = kd,
                    hb = hb,
                    kk = kk,
                    z = z,
                    draws = draws)
    
    ## Start values for steady state
    xstart <- c(D = rep(0,draws),
                H = rep(0,draws))
    
    ## Solve model
    out <- ode(y = xstart,
               times = times,
               func = model_SD,
               parms,
               input = sigimp)
  }
  
  if(x$model_type == "IT"){
    D_matrix = as.data.frame(out) %>%
      dplyr::select(-c(time,signal))%>%
      as.matrix()
    S <- 1-plogis(log(t(D_matrix)), location = log(parms$alpha), scale = 1/parms$beta)
    dtheo <- t(S)
    
  }
  if(x$model_type=="SD"){
    
    H_matrix = as.data.frame(out) %>%
      dplyr::select(contains("H"),-c(time,signal))%>%
      as.matrix()
    S <- exp(-H_matrix)
    dtheo <- S
  }

  # -------- df.theo
  # df <- data_frame(time = out[, "time"],
  #                 conc = out[, "signal"],
  #                 replicate = rep(unique(newdata$replicate), length(out[, "time"])),
  #                 q50 = apply(dtheo, 1, quantile, probs = 0.5, na.rm = TRUE),
  #                 qinf95 = apply(dtheo, 1, quantile, probs = 0.025, na.rm = TRUE),
  #                 qsup95 = apply(dtheo, 1, quantile, probs = 0.975, na.rm = TRUE))
  ## RETURN
  
  pp_solveTKTD <- as.data.frame(dtheo) %>%
    mutate(time = out[, "time"],
           conc = out[, "signal"],
           replicate = rep(unique(newdata$replicate), length(out[, "time"])),
           q50 = apply(dtheo, 1, quantile, probs = 0.5, na.rm = TRUE),
           qinf95 = apply(dtheo, 1, quantile, probs = 0.025, na.rm = TRUE),
           qsup95 = apply(dtheo, 1, quantile, probs = 0.975, na.rm = TRUE))
  
  return(pp_solveTKTD)
}



#' #' Run odes with the stan integrator
#' #'
#' #' @export
#' #' 
#' #' 
#' run_odes <- function(x, newdata){
#' 
#'   # This is usefull to load the function ode_TKTD_varSD which has been implemented  
#'   if(x$model_type == "SD"){
#'     stanEstim <- extract(x$stanfit, pars = c("hb_log10", "kd_log10", "z_log10", "kk_log10"))
#'   }
#'   if(x$model_type == "IT"){
#'     stanEstim <- extract(x$stanfit, pars = c("hb_log10", "kd_log10", "alpha_log10", "beta_log10"))
#'   }
#'   df_stanEstim <- as.data.frame(stanEstim)
#' 
#'   time_Predict <- seq(0, max(newdata[,1]), length.out = 1e2)
#'   
#'   if(x$model_type == "SD"){
#'     y0 = c(0, 0) # initial dose
#'     ls_ode <- lapply(1:nrow(df_stanEstim), function(it)
#'       solveTKTD_varSD(y0, -1e-9, time_Predict, as.numeric(df_stanEstim[it,]), newdata[,1], newdata[,2]))
#'   }
#'   if(x$model_type == "IT"){
#'     y0 = 0
#'     ls_ode <- lapply(1:nrow(df_stanEstim), function(it)
#'       solveTKTD_varIT(y0, -1e-9, time_Predict, as.numeric(df_stanEstim[it,]), newdata[,1], newdata[,2]))
#'   }
#'   
#'   return(ls_ode)
#' }
