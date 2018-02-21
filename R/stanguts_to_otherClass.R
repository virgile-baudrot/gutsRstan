#' Create a stanfit from a stanguts object
#' 
#' Create a stanfit (\code{rstan}) from a stanguts object.
#'
#' @param stanguts An object of class \code{stanguts}.
#' 
#' @return An object of class \code{stanfit}.
#'   
#' @export
#' 

stanguts_to_stanfit.stanguts <- function(stanguts){
  return( stanguts$stanfit )
}

#' Create a survFit from a stanguts object
#' 
#' Create a survFit (\code{morse}) from a stanguts object.
#'
#' @param stanguts An object of class \code{stanguts}.
#' 
#' @return An object of class \code{survFit}.
#'   
#' @export
#'

stanguts_to_survFit.stanguts <- function(stanguts,
                                       extend_time = 100){
  
  ##
  ## Define model
  ##
  
  ### Model
  model <- stanguts$stanfit@stanmodel
  
  ### Model type
  model_type <- stanguts$model_type
  
  ### Parameters
  pars_general <- c("kd_log10", "hb_mean", "psurv")
  pars_SD <- c(pars_general,c("kk_mean", "z_log10"))
  pars_IT <- c(pars_general, c("alpha_log10", "beta_log10"))
  pars_PROPER <- c(pars_general, c("kk_log10", "alpha_log10", "beta_log10"))
  
  parameters <- switch(model_type,
                       IT = pars_IT,
                       SD = pars_SD,
                       PROPER = pars_PROPER)
  
  ### MCMC info
  mcmcInfo = data.frame(n.iter = stanguts$mcmcInfo$n.iter,
                        n.chains = stanguts$mcmcInfo$n.chains,
                        n.adapt = "Include in n.warmup",
                        thin.interval = stanguts$mcmcInfo$thin.interval,
                        n.warmup = stanguts$mcmcInfo$n.warmup)
  
  
  # jags.data
  
  survData_local = morse::survData(stanguts$data)
  
  globalData <- morse:::modelData.survDataVarExp(x = survData_local, model_type = model_type, extend_time = extend_time)
  jags.data <- globalData$modelData

  priorsData = globalData$priorsMinMax
  # warnings
  ##
  ## Cheking posterior range with data from experimental design:
  ##
  
  mcmc <- rstan::As.mcmc.list(stanguts$stanfit)

  estim.par <- morse:::survFit_TKTD_params(mcmc = mcmc, model_type = model_type)
  
  warnings <- morse:::msgTableCreate()
  
  if (dplyr::filter(estim.par, parameters == "kd")$Q97.5 > priorsData$kd_max){
    ## store warning in warnings table
    msg <- "The estimation of the dominant rate constant (model parameter kd) lies 
    outside the range used to define its prior distribution which indicates that this
    rate is very high and difficult to estimate from this experiment !"
    warnings <- msgTableAdd(warnings, "kd_outRange", msg)
    ## print the message
    warning(msg, call. = FALSE)
  }
  
  if (dplyr::filter(estim.par, parameters == "hb")$Q2.5 < priorsData$hb_min){
    ## store warning in warnings table
    msg <- "The estimation of the natural instantaneous mortality rate (model 
    parameter hb) lies outside the range used to define its prior distribution 
    which indicates that this rate is very low and so difficult to estimate 
    from this experiment !"
    warnings <- msgTableAdd(warnings, "hb_outRange", msg)
    ## print the message
    warning(msg, call. = FALSE)
  }
  
  ### for SD model
  if(model_type == "SD"){
    if (dplyr::filter(estim.par, parameters == "kk")$Q97.5 > priorsData$kk_max){
      ## store warning in warnings table
      msg <- "The estimation of the killing rate (model parameter k) lies
      outside the range used to define its prior distribution which indicates
      that this rate is very high and difficult to estimate from this experiment !"
      warnings <- msgTableAdd(warnings, "kk_outRange", msg)
      ## print the message
      warning(msg, call. = FALSE)
    }
    
    if (dplyr::filter(estim.par, parameters == "z")$Q2.5 < priorsData$conc_min ||
        dplyr::filter(estim.par, parameters == "z")$Q97.5 > priorsData$conc_max){
      ## store warning in warnings table
      msg <- "The estimation of Non Effect Concentration threshold (NEC) 
      (model parameter z) lies outside the range of tested concentration 
      and may be unreliable as the prior distribution on this parameter is
      defined from this range !"
      warnings <- msgTableAdd(warnings, "z_outRange", msg)
      ## print the message
      warning(msg, call. = FALSE)
    }
  }
  
  ### for IT model
  if(model_type == "IT"){
    
    if (dplyr::filter(estim.par, parameters == "alpha")$Q2.5 < priorsData$conc_min ||
        dplyr::filter(estim.par, parameters == "alpha")$Q97.5 > priorsData$conc_max){
      ## store warning in warnings table
      msg <- "The estimation of log-logistic median (model parameter alpha) 
      lies outside the range of tested concentration and may be unreliable as 
      the prior distribution on this parameter is defined from this range !"
      warnings <- msgTableAdd(warnings, "alpha_outRange", msg)
      ## print the message
      warning(msg, call. = FALSE)
    }
  }
  
  ### for PROPER model
  if(model_type == "PROPER"){
    if (dplyr::filter(estim.par, parameters == "kk")$Q97.5 > priorsData$kk_max){
      ## store warning in warnings table
      msg <- "The estimation of the killing rate (model parameter k) lies
      outside the range used to define its prior distribution which indicates
      that this rate is very high and difficult to estimate from this experiment !"
      warnings <- msgTableAdd(warnings, "kk_outRange", msg)
      ## print the message
      warning(msg, call. = FALSE)
    }
    
    if (dplyr::filter(estim.par, parameters == "alpha")$Q2.5 < priorsData$conc_min ||
        dplyr::filter(estim.par, parameters == "alpha")$Q97.5 > priorsData$conc_max){
      ## store warning in warnings table
      msg <- "The estimation of log-logistic median (model parameter alpha) 
      lies outside the range of tested concentration and may be unreliable as 
      the prior distribution on this parameter is defined from this range !"
      warnings <- msgTableAdd(warnings, "alpha_outRange", msg)
      ## print the message
      warning(msg, call. = FALSE)
    }
  }
  
  ##
  ## MCMC information
  ## 
  mcmcInfo = data.frame(n.iter = n.iter,
                        n.chains = n.chains,
                        n.adapt = n.adapt,
                        thin.interval = thin.interval,
                        n.warmup = n.warmup)
  
  
  ##
  transformed.data <- data.frame(
    replicate = jags.data$replicate,
    time = jags.data$time,
    conc = jags.data$conc,
    Nsurv = jags.data$Nsurv
  )
  transformed.data <- dplyr::group_by(transformed.data, replicate)
  transformed.data <- dplyr::mutate(transformed.data, Ninit = max(Nsurv, na.rm = TRUE))
  
  
  
  OUT <- list(estim.par = estim.par,
              mcmc = mcmc,
              model = model,
              dic = NULL,
              parameters = parameters,
              mcmcInfo = mcmcInfo,
              jags.data = jags.data,
              warnings = warnings,
              model_type = model_type,
              transformed.data = transformed.data,
              original.data = data)
  
  class(OUT) <- c("survFitVarExp","survFit")
  
  return(OUT)
  
}