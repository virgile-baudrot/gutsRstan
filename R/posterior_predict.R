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
#' 
#' @aliases posterior_predict
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
#' @param posterior_type A method to define the conditional probability for
#'  posterior estimation. Only required when \code{newdata = NULL}. Choices 
#'  are \code{pointwise} and \code{timeserie}. For "\code{pointwise}", the
#'  conditional probability is based on the observation
#'  of the previous point, while for "\code{timeserie}" the conditional probability
#'  is based on the simulation of the previous point. Default is \code{pointwise}.
#' @param mc.cores The number of cores to use, i.e. at most how many child
#' processes will be run simultaneously.  The option is initialized from 
#' environment variable ‘MC_CORES’ if set. Must be at least one, and 
#' parallelization requires at least two cores. Default is 1. It is not 
#' available on Windows unless ‘mc.cores = 1’.
#' @param integrator_method The integrator to use. See 
#' function \code{\link[deSolve]{ode}}. Either a function that
#' performs integration, or a list of class rkMethod, or a string ("lsoda",
#' "lsode", "lsodes","lsodar","vode", "daspk", "euler", "rk4", "ode23", 
#' "ode45", "radau", "bdf", "bdf_d", "adams", "impAdams" or "impAdams_d" ,
#' "iteration"). Options "bdf", "bdf_d", "adams", "impAdams" or "impAdams_d"
#' are the backward differentiation formula, the BDF with diagonal representation
#' of the Jacobian, the (explicit) Adams and the implicit Adams method, and the
#' implicit Adams method with diagonal representation of the Jacobian
#'  respectively (see details). The default integrator used is \code{lsoda}.
#' 
#'   
#' @import deSolve
#' @import parallel
#' @importFrom matrixStats colCummaxs
#'    
#' @export
#' 
#' 
posterior_predict.stanTKTD <- function(x,
                                       newdata = NULL, #other stanTKTD
                                       draws = NULL,
                                       interpolate_length = 1e2,
                                       interpolate_method = "linear",
                                       posterior_type = "pointwise",
                                       mc.cores = 1,
                                       integrator_method = "lsoda"){

  if(is.null(newdata)){
    if(posterior_type == "pointwise"){
      mat_posterior_predict <- extract(x$stanfit, pars = 'Nsurv_ppc')$Nsurv_ppc
    }
    if(posterior_type == "timeserie"){
      mat_posterior_predict <- extract(x$stanfit, pars = 'Nsurv_sim')$Nsurv_sim
    }
    # mat_posterior_predict <- extract(x$stanfit, pars = 'Psurv_hat')
  }
  if(!is.null(newdata)){

    # pSurv ----------------------------
    posteriorPsurv_predict <- psurv_predict(x, newdata, draws, interpolate_length, interpolate_method, mc.cores)
  
    toJoin_posterior_predict <- dplyr::select(posteriorPsurv_predict, -conc)
    
    # select Nsurv != NA
      toJoin_newdata <-  newdata %>%
        dplyr::select(replicate, time, Nsurv) %>%
        dplyr::mutate(replicate = as.character(replicate)) %>%
        dplyr::filter(!is.na(Nsurv))
    
    join_predict <- dplyr::inner_join(x = toJoin_posterior_predict, y = toJoin_newdata, by = c('replicate','time'))
     
    # Nsurv ---------------------------
    
    select_join_predict <- dplyr::select(join_predict, -c(time, Nsurv, replicate))
                  
    mat_posterior_predict <- matrix(NA, ncol = ncol(select_join_predict), nrow = nrow(select_join_predict))
    mat_prob <- matrix(NA, ncol = ncol(select_join_predict), nrow = nrow(select_join_predict))
  
    for(i in 1:nrow(join_predict)){
      if(join_predict$time[i] == 0){
        
        mat_prob[i, ] = rep(1, ncol(mat_posterior_predict))
        
        mat_posterior_predict[i, ] <- rep(join_predict$Nsurv[i], ncol(mat_posterior_predict))
      } else{
        if(posterior_type == "pointwise"){
          
          mat_prob[i, ] = as.numeric(select_join_predict[i, ]/select_join_predict[i-1, ])
          
          mat_posterior_predict[i, ] <- rbinom( n = ncol(mat_posterior_predict),
                                                size = join_predict$Nsurv[i-1],
                                                prob = as.numeric(select_join_predict[i, ]/select_join_predict[i-1, ]))
        }
        if(posterior_type == "timeserie"){
          mat_posterior_predict[i, ] <- rbinom( n = ncol(mat_posterior_predict),
                                                size = mat_posterior_predict[i-1, ],
                                                prob = as.numeric(select_join_predict[i, ]/select_join_predict[i-1, ]))
        }
      } 
    }
    
    mat_posterior_predict <- t(mat_posterior_predict)
  }
    
  return(mat_posterior_predict)
}

# internal --------------------------------------------------------------------

psurv_predict <- function(x,
                          newdata = NULL,
                          draws = NULL,
                          interpolate_length = 1e2,
                          interpolate_method = "linear",
                          mc.cores = 1){
  
  if(is.null(newdata)){
    newdata <- x$data
  }

  filter_newdata <- newdata %>%
      dplyr::filter(!is.na(conc))
    
  filter_newdata_list <- split(filter_newdata, filter_newdata$replicate)
    
  newdata_list <- split(newdata, newdata$replicate)

  if(length(newdata_list) != length(filter_newdata_list)){
    stop("A replicate has all 'conc' with 'NA'")
  }
  
  if(mc.cores == 1){
    cat('progress:', 0, '% \n')
  } else{
    cat('For information: No progress indicator is available when mc.cores > 1. \n')
  }
  
  n_replicate <- length(newdata_list)
  
  pp_list_solveTKTD <- parallel::mclapply(1:n_replicate,
                                function(it) {
                                  ode_TKTD = solveTKTD(x,
                                            time_conc = filter_newdata_list[[it]]$time,
                                            conc = filter_newdata_list[[it]]$conc,
                                            draws = draws,
                                            interpolate_time = newdata_list[[it]]$time,
                                            interpolate_length = interpolate_length,
                                            interpolate_method = interpolate_method)
                                  ## progress
                                  percent_draws = it/n_replicate * 100
                                  cat('progress:', percent_draws, '% \n')
                                  ##
                                  return(ode_TKTD)                      
                                },
                                mc.cores = mc.cores)

  names(pp_list_solveTKTD) <- names(newdata_list)
  
  out_posterior_predict <- dplyr::bind_rows(pp_list_solveTKTD, .id = "replicate")

  return(out_posterior_predict)
}

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
                      time_conc = NULL,
                      conc = NULL,
                      draws = NULL,
                      interpolate_time = NULL,
                      interpolate_length = 1e2,
                      interpolate_method = "linear"){
  
  MCMC_stanEstim <- extract_MCMCparameters(x)
  
  if(is.null(draws)){
    draws <- nrow(MCMC_stanEstim)
  } else{
    if(draws > nrow(MCMC_stanEstim)) { stop('draws > size of MCMC.')}
    seq_MCMC <- round(seq(from = 1, to = nrow(MCMC_stanEstim), length.out = draws)) # with draws < size MCMC, elements of seq_MCMC are unique ! 
    MCMC_stanEstim <- MCMC_stanEstim[ seq_MCMC , ]
  }
  
  ## external signal with several rectangle impulses
  sigimp <- approxfun(time_conc, conc, method = interpolate_method, rule = 2)
  
  if(is.null(interpolate_time)){
    times <- seq(min(time_conc), max(time_conc), length.out = interpolate_length)
  } else{
    times <-  sort(unique(c(seq(min(interpolate_time), max(interpolate_time), length.out = interpolate_length), interpolate_time)))
  }
  
  ## model
  kd <- 10^MCMC_stanEstim$kd_log10
  hb <- 10^MCMC_stanEstim$hb_log10
    
  if(x$model_type == "IT"){

    alpha <- 10^MCMC_stanEstim$alpha_log10
    beta <- 10^MCMC_stanEstim$beta_log10
    
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
      dplyr::select(-c(time, signal))%>%
      as.matrix() %>%
      matrixStats::colCummaxs()
    #S <- exp( - hb %*% t(times)) * ( 1-plogis(log(t(D_matrix)), location = log(parms$alpha), scale = 1/parms$beta))
    #S <- exp( - hb %*% t(times)) * ( 1-1 / (1+ (t(D_matrix)/parms$alpha)^(-parms$beta)))
    #S <- exp( - hb %*% t(times)) * ( 1-1 / (1+ exp((t(D_matrix) - parms$alpha)/(parms$beta))))
    #S <- exp( - hb %*% t(times)) * ( 1- t(D_matrix)^(parms$beta) / ( (parms$alpha)^(parms$beta) + t(D_matrix)^(parms$beta) ))
    S <- exp( - hb %*% t(times)) * ( 1 - 1 / (1+ exp( -parms$beta * (log(t(D_matrix)) - log(parms$alpha) )))) 
    dtheo <- t(S)

  }
  if(x$model_type=="SD"){
    
    H_matrix = as.data.frame(out) %>%
      dplyr::select(contains("H"),-c(time, signal))%>%
      as.matrix()
    S <- exp(-H_matrix)
    dtheo <- S
  }

  # OUTPUT --------------------------------------------------------------------

  pp_solveTKTD <- as_data_frame(dtheo) %>%
    mutate(time = out[, "time"], 
           conc = out[, "signal"])
  
  return(pp_solveTKTD)
}


#' #' Run odes with the stan integrator
#' #'
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