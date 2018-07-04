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
#' from package \code{morse}.
#' 
#' @param stanguts An object of class \code{stanguts}
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @return An object of class \code{survFit}
#' 
#' 
#' @export
stanguts_to_survFit <- function(stanguts, ...){
  UseMethod("stanguts_to_survFit")
}
#' @param extend_time Number of time points for linear interpolation of
#' exposure concentration
#'  
#' @rdname stanguts_to_survFit
#' @importFrom morse survData priors_survData
#' @importFrom zoo na.approx na.locf
#' 
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
  
  globalData <-  modelData_survDataVarExp(x = survData_local, model_type = model_type, extend_time = extend_time)
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

## ---------------- modelData

modelData_survDataVarExp <- function(x,
                                     model_type = NULL,
                                     extend_time = 100, ...){
  
  
  ## 0. Creation of additional variable
  ## - tprec: previous time
  ## - Nprec: previous number of survivors
  ## - time_ID_red: identification of row number inside a group
  ## - i_row: identification of row number (for every group)
  ## - i_prec: identification of previous row number (for every group) exept when time_ID_red (in group) is 1
  
  x_interpolate <- survData_interpolate(x,  extend_time = extend_time) %>%
    dplyr::arrange(replicate, time)
  
  x_reduce <- x_interpolate %>%
    dplyr::filter(!is.na(Nsurv)) %>%
    # Group by replicate to replicate an indice of replicate:
    dplyr::mutate(replicate_ID = group_indices_(., .dots="replicate")) %>%
    dplyr::group_by(replicate) %>%
    dplyr::arrange(replicate, time) %>%
    dplyr::mutate( tprec = ifelse( time == 0, time, dplyr::lag(time) ) ) %>%
    dplyr::mutate( Nprec = ifelse( time == 0, Nsurv, dplyr::lag(Nsurv) ) ) %>%
    dplyr::mutate(time_ID_red = row_number()) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(i_row = row_number()) %>%
    dplyr::mutate(i_prec = ifelse(time_ID_red == 1, i_row, dplyr::lag(i_row))) %>%
    dplyr::arrange(replicate, time)
  
  ##
  ## ============================= Construction of modelData
  ##
  
  ### return priors for model
  priorsData <- morse::priors_survData(x = x, model_type = model_type)
  
  modelData <- priorsData$priorsList
  
  ### reduce dataset: To remove NA in Nsurv column
  modelData$time <-  x_reduce$time
  modelData$conc <-  x_reduce$conc
  modelData$replicate <-  x_reduce$replicate
  modelData$Nsurv <-  x_reduce$Nsurv
  modelData$Nprec <-  x_reduce$Nprec
  
  ###  parameters
  
  modelData$replicate_ID <- x_reduce$replicate_ID
  modelData$time_ID_red <- x_reduce$time_ID_red
  
  modelData$n_data_red <- nrow(x_reduce)
  
  ## Interpolation
  modelData$time_ID_long_red <- x_reduce$time_ID_long
  modelData$i_prec <- x_reduce$i_prec
  
  ## Interpolation
  
  modelData$n_data_long <- nrow(x_interpolate)
  
  ### Integration
  modelData$replicate_ID_long  <- x_interpolate$replicate_ID_long
  modelData$time_ID_long <- x_interpolate$time_ID_long
  
  modelData$conc_long <- x_interpolate$conc
  modelData$time_long <- x_interpolate$time
  modelData$replicate_long <- x_interpolate$replicate
  
  modelData$tprec_long <- x_interpolate$tprec_long
  modelData$concprec_long <- x_interpolate$concprec_long
  
  
  ##
  ## other parameters specific to model SD vs. IT
  ##
  
  if(model_type == "SD"){
    
    modelData$tprec_ID_long <- x_interpolate$tprec_ID_long
    
  }
  if (model_type == "IT"){
    
    modelData$time <- x_reduce$time
    
  }
  if(model_type == "PROPER"){
    
    modelData$tprec_ID_long <- x_interpolate$tprec_ID_long
    
  }
  
  ##
  ## =========== Object to return
  ##
  
  
  ### OUT ================
  OUT_modelDATA <- list(modelData = modelData,
                        priorsList = priorsData$priorsList,
                        priorsMinMax = priorsData$priorsMinMax)
  
  return(OUT_modelDATA)
  
}

# Create a dataset for survival analysis when the replicate of concentration is variable
#
# @param x An object of class \code{survData}
#
# @return A dataframe
#
survData_interpolate <- function(x, extend_time = 100){
  
  ## data.frame with time
  
  df_MinMax <- x %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarise(min_time = min(time, na.rm = TRUE),
                     max_time = max(time, na.rm = TRUE)) %>%
    dplyr::group_by(replicate) %>%
    # dplyr::do(data.frame(replicate = .$replicate, time = seq(.$min_time, .$max_time, length = extend_time)))
    dplyr::do(data_frame(replicate = .$replicate, time = seq(.$min_time, .$max_time, length = extend_time)))
  
  x_interpolate <- dplyr::full_join(df_MinMax, x,
                                    by = c("replicate", "time")) %>%
    dplyr::group_by(replicate) %>%
    dplyr::arrange(replicate, time) %>% # organize in replicate and time
    dplyr::mutate(conc = zoo::na.approx(conc, time, na.rm = FALSE)) %>%
    # from package zoo : 'na.locf()' carry the last observation forward to replace your NA values.
    dplyr::mutate(conc = ifelse(is.na(conc),zoo::na.locf(conc),conc),
                  # identification of time point index for Nsurv
                  id_conc_interp = ifelse(is.na(Nsurv), NA, row_number()),
                  # 'lag' function copy values lagged by 1 (see 'dplyr' package)
                  tprec_long = ifelse( time == 0, time, dplyr::lag(time) ),
                  concprec_long = ifelse( time == 0, conc, dplyr::lag(conc) ) ) %>%
    dplyr::group_by(replicate) %>%
    dplyr::mutate(time_ID_long = row_number(),
                  tprec_ID_long = ifelse(time_ID_long==1, time_ID_long,  dplyr::lag(time_ID_long))) %>%
    dplyr::ungroup() %>%
    # Group by replicate to replicate an indice of replicate:
    dplyr::mutate(replicate_ID_long = group_indices_(., .dots="replicate"))
  
  return(x_interpolate)
}
