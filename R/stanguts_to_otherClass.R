###############################################################################
# stanfit
##############################################################################
#' @title Convert a \code{stanguts} object into a \code{stanfit} object
#' 
#' @description Convert a \code{stanguts} object returned by the function
#' \link[gutsRstan]{stan_guts} as provided by Stan package \link{rstan}
#' 
#' @param stanguts An object of class \code{stanguts}
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @return An object of class \code{stanfit}
#' 
#' @export
stanguts_to_stanfit <- function(stanguts, ...){
  UseMethod("stanguts_to_stanfit")
}

#' @rdname stanguts_to_stanfit
#' @export 
stanguts_to_stanfit.stanguts <- function(stanguts, ...){
  return( stanguts$stanfit )
}

###############################################################################
# survFit
###############################################################################

#' @title Convert a \code{stanguts} object into a \code{survFit} object
#' 
#' @description Convert a \code{stanguts} object returned by the function
#' \link[gutsRstan]{stan_guts} as provided by the function \link[morse]{survFit}
#' 
#' @param stanguts An object of class \code{stanguts}
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @return An object of class \code{survFit}
#' 
#' @import morse
#' 
#' @export
stanguts_to_survFit <- function(stanguts, ...){
  UseMethod("stanguts_to_survFit")
}
#' @param extend_time Number of time points for linear interpolation of external concentration
#'  
#' @rdname stanguts_to_survFit
#' @export 
#'
stanguts_to_survFit.stanguts <- function(stanguts,
                                         extend_time = 100,
                                         ...){
  
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
  
  survData_local <-  morse::survData(stanguts$data)
  
  globalData <-  morse::modelData.survDataVarExp(x = survData_local, model_type = model_type, extend_time = extend_time)
  jags.data <- globalData$modelData

  priorsData <- globalData$priorsMinMax
  # warnings
  ##
  ## Cheking posterior range with data from experimental design:
  ##
  
  mcmc <- rstan::As.mcmc.list(stanguts$stanfit)

  estim.par <- survFit_TKTD_params(mcmc = mcmc, model_type = model_type)
  
  warnings <- msgTableCreate()
  
  if (dplyr::filter(estim.par, parameters == "kd")$Q97.5 > priorsData$kd_max){
    ## store warning in warnings table
    msg <- "The estimation of the dominant rate constant (model parameter kd) lies 
    outside the range used to define its prior distribution which indicates that this
    rate is very high and difficult to estimate from this experiment !"
    warnings <-  msgTableAdd(warnings, "kd_outRange", msg)
    ## print the message
    warning(msg, call. = FALSE)
  }
  
  if (dplyr::filter(estim.par, parameters == "hb")$Q2.5 < priorsData$hb_min){
    ## store warning in warnings table
    msg <- "The estimation of the natural instantaneous mortality rate (model 
    parameter hb) lies outside the range used to define its prior distribution 
    which indicates that this rate is very low and so difficult to estimate 
    from this experiment !"
    warnings <-  msgTableAdd(warnings, "hb_outRange", msg)
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
      warnings <-  msgTableAdd(warnings, "kk_outRange", msg)
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
      warnings <-  msgTableAdd(warnings, "z_outRange", msg)
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
      warnings <-  msgTableAdd(warnings, "alpha_outRange", msg)
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
      warnings <-  msgTableAdd(warnings, "kk_outRange", msg)
      ## print the message
      warning(msg, call. = FALSE)
    }
    
    if (dplyr::filter(estim.par, parameters == "alpha")$Q2.5 < priorsData$conc_min ||
        dplyr::filter(estim.par, parameters == "alpha")$Q97.5 > priorsData$conc_max){
      ## store warning in warnings table
      msg <- "The estimation of log-logistic median (model parameter alpha) 
      lies outside the range of tested concentration and may be unreliable as 
      the prior distribution on this parameter is defined from this range !"
      warnings <-  msgTableAdd(warnings, "alpha_outRange", msg)
      ## print the message
      warning(msg, call. = FALSE)
    }
  }
  
  ## transformed.data
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
              original.data = stanguts$data)
  
  class(OUT) <- c("survFitVarExp","survFit")
  
  return(OUT)
  
}


#####################
# To convert msg in morse class
####################

msgTableCreate <- function() {
  t <- data.frame(id = character(0), msg = character(0), stringsAsFactors = FALSE)
  class(t) <- c("msgTable", "data.frame")
  t
}

msgTableAppend <- function(...) {
  u <- rbind(...)
  class(u) <- c("msgTable", "data.frame")
  u
}

msgTableAdd <- function(t, id, msg) {
  newlines <- data.frame(id = id, msg = msg, stringsAsFactors = FALSE)
  msgTableAppend(t, newlines)
}

msgTableSingleton <- function(id, msg) {
  msgTableAdd(msgTableCreate(), id, msg)
}

msgTableIsEmpty <- function(x)
  dim(x)[1] == 0

#' @export
print.msgTable <- function(x, ...) {
  if (msgTableIsEmpty(x)) {
    cat("No message\n")
  }
  else {
    cat("Message(s):\n")
    for (m in x$msg) {
      cat(paste("\t",m,"\n",sep=""))
    }
  }
}

survFit_TKTD_params <- function(mcmc, model_type) {
  # create the table of posterior estimated parameters
  # for the survival analyses
  # INPUT:
  # - mcmc:  list of estimated parameters for the model with each item representing
  # a chain
  # OUTPUT:
  # - data frame with 3 columns (values, CIinf, CIsup) and 3-4rows (the estimated
  # parameters)
  
  # Retrieving parameters of the model
  res.M <- summary(mcmc)
  
  kd <- 10^res.M$quantiles["kd_log10", "50%"]
  kd_inf95 <- 10^res.M$quantiles["kd_log10", "2.5%"]
  kd_sup95 <- 10^res.M$quantiles["kd_log10", "97.5%"]
  
  hb <- 10^res.M$quantiles["hb_log10", "50%"]
  hb_inf95 <- 10^res.M$quantiles["hb_log10", "2.5%"]
  hb_sup95 <- 10^res.M$quantiles["hb_log10", "97.5%"]
  
  if(model_type == "SD"){
    kk <- 10^res.M$quantiles["kk_log10", "50%"]
    kk_inf95 <- 10^res.M$quantiles["kk_log10", "2.5%"]
    kk_sup95 <- 10^res.M$quantiles["kk_log10", "97.5%"]
    
    z <- 10^res.M$quantiles["z_log10", "50%"]
    z_inf95 <- 10^res.M$quantiles["z_log10", "2.5%"]
    z_sup95 <- 10^res.M$quantiles["z_log10", "97.5%"]
    
    res <- data.frame(parameters = c("kd", "hb", "z", "kk"),
                      median = c(kd, hb, z, kk),
                      Q2.5 = c(kd_inf95, hb_inf95, z_inf95, kk_inf95),
                      Q97.5 = c(kd_sup95, hb_sup95, z_sup95, kk_sup95))
    
  } else if (model_type == "IT"){
    alpha <- 10^res.M$quantiles["alpha_log10", "50%"]
    alpha_inf95 <- 10^res.M$quantiles["alpha_log10", "2.5%"]
    alpha_sup95 <- 10^res.M$quantiles["alpha_log10", "97.5%"]
    
    beta <- 10^res.M$quantiles["beta_log10", "50%"]
    beta_inf95 <- 10^res.M$quantiles["beta_log10", "2.5%"]
    beta_sup95 <- 10^res.M$quantiles["beta_log10", "97.5%"]
    
    res <- data.frame(parameters = c("kd", "hb", "alpha", "beta"),
                      median = c(kd, hb, alpha, beta),
                      Q2.5 = c(kd_inf95, hb_inf95, alpha_inf95, beta_inf95),
                      Q97.5 = c(kd_sup95, hb_sup95, alpha_sup95, beta_sup95))
  } else if (model_type == "PROPER"){
    kk <- 10^res.M$quantiles["kk_log10", "50%"]
    kk_inf95 <- 10^res.M$quantiles["kk_log10", "2.5%"]
    kk_sup95 <- 10^res.M$quantiles["kk_log10", "97.5%"]
    
    alpha <- 10^res.M$quantiles["alpha_log10", "50%"]
    alpha_inf95 <- 10^res.M$quantiles["alpha_log10", "2.5%"]
    alpha_sup95 <- 10^res.M$quantiles["alpha_log10", "97.5%"]
    
    beta <- 10^res.M$quantiles["beta_log10", "50%"]
    beta_inf95 <- 10^res.M$quantiles["beta_log10", "2.5%"]
    beta_sup95 <- 10^res.M$quantiles["beta_log10", "97.5%"]
    
    res <- data.frame(parameters = c("kd", "hb", "kk" , "alpha", "beta"),
                      median = c(kd, hb, kk, alpha, beta),
                      Q2.5 = c(kd_inf95, hb_inf95, kk_inf95, alpha_inf95, beta_inf95),
                      Q97.5 = c(kd_sup95, hb_sup95, kk_sup95, alpha_sup95, beta_sup95))
  } else {
    stop("please, provide the 'model_type': 'IT', 'SD' or 'PROPER'")
  }
  
  return(res)
}
