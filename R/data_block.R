#' @import morse
#' 
#' @export
#' 
modelDataStan <- function(data, model_type = NULL, ode_control = NULL, priors_list = NULL){
  
  #------------ pool replicate when data is constant:
  if("survDataCstExp" %in% class(data)){ # data is a survData object
    ## pool replicate when there is the same constante concentration
    data <- pool_survData(data)
  }
  
  # --------------------------------------------------
  
  ls_OUT <- observation_survData(data)

  # PRIORS
  if(is.null(priors_list)){
    priors <- priors_survData(data, model_type = model_type)$priorsList
  } else{
    priors <- priors_list
  }
  
    
  ls_OUT_2 <- unlist(list(ls_OUT, priors, ode_control), recursive = FALSE)
    
  return(ls_OUT_2)
}

# INTERNAL --------------------------------------------------------------------

#------------------------------------------------------------------------------
#
#                                      OBSERVATION
#
#------------------------------------------------------------------------------

observation_survData <- function(data){
  
  ## REMOVE time = 0:
  #  data <- dplyr::filter(data, time != 0)
  
  ls_OUT <- list()
  
  ls_OUT$n_group <- length(unique(data$replicate)) # number of group
  
  # Concentration
  data_Conc <- data %>% 
    dplyr::filter(!is.na(conc)) %>%
    dplyr::arrange(replicate, time) %>%
    dplyr::mutate(id_all = row_number() )
  
  data_Conc_id = data_Conc %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarise(idC_lw = min(id_all),
                     idC_up = max(id_all))
  
  ls_OUT$n_data_conc <- nrow(data_Conc)
  
  ls_OUT$conc <- data_Conc$conc
  ls_OUT$tconc <- data_Conc$time
  
  ls_OUT$replicate_conc <- data_Conc$replicate
  
  ls_OUT$idC_lw <- data_Conc_id$idC_lw
  ls_OUT$idC_up <- data_Conc_id$idC_up
  
  # Survival
  data_Nsurv <- data %>%
    dplyr::filter(!is.na(Nsurv)) %>%
    dplyr::arrange(replicate, time) %>%
    dplyr::mutate(id_all = row_number() ) %>%
    dplyr::group_by(replicate) %>%
    dplyr::mutate(Nprec = ifelse( time == min(time), Nsurv, dplyr::lag(Nsurv) ),
                  Ninit = max(Nsurv)) %>%  # since it is grouped by replicate
    dplyr::ungroup()
  
  data_Nsurv_id = data_Nsurv %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarise(idS_lw = min(id_all),
                     idS_up = max(id_all))
  
  
  ls_OUT$n_data_Nsurv <- nrow(data_Nsurv)
  
  ls_OUT$Nsurv <- data_Nsurv$Nsurv
  ls_OUT$Nprec <- data_Nsurv$Nprec
  ls_OUT$Ninit <- data_Nsurv$Ninit
  
  ls_OUT$tNsurv <- data_Nsurv$time
  ls_OUT$replicate_Nsurv <- data_Nsurv$replicate
  
  ls_OUT$idS_lw <- data_Nsurv_id$idS_lw
  ls_OUT$idS_up <- data_Nsurv_id$idS_up
  
  return(ls_OUT)
  
}


#------------------------------------------------------------------------------
#
#                                      PRIORS
#
#------------------------------------------------------------------------------

priors_survData <- function(x, model_type = NULL){
  
  data <- filter(x, time != 0)
  
  # Parameter calculation of concentration min and max
  conc_min <- min(data$conc[data$conc != 0], na.rm = TRUE) # to remove 0 and NA
  conc_max <- max(data$conc, na.rm = TRUE)
  
  time_min <- min(data$time)
  time_max <- max(data$time)
  
  conc_unic <- sort(unique(data$conc))
  conc_unicPrec <- dplyr::lag(conc_unic)
  conc_minDelta <- min(conc_unic - conc_unicPrec, na.rm = TRUE)
  
  ##
  ## dominant rate constant: kd
  ##
  
  kd_max <- -log(0.001) / time_min
  kd_min <- -log(0.999) / time_max
  
  ##
  ## background hazard rate
  ##
  
  hb_max <- -log(0.5) / time_min
  hb_min <- -log(0.999) / time_max
  
  ##
  ## killing rate parameter: kk
  ##
  
  kk_max <- -log(0.001) / (time_min * conc_minDelta)
  kk_min <- -log(0.999) / (time_max * (conc_max - conc_min))
  
  ##
  ## beta
  ##
  
  beta_minlog10 <- -2
  beta_maxlog10 <- 2
  
  priorsMinMax <- list(
    conc_min = conc_min,
    conc_max = conc_max,
    kd_min = kd_min,
    kd_max = kd_max,
    hb_min = hb_min,
    hb_max = hb_max,
    kk_min = kk_min,
    kk_max = kk_max,
    z_min = conc_min,
    z_max = conc_max,
    alpha_min = conc_min,
    alpha_max = conc_max,
    beta_min = beta_minlog10,
    beta_max = beta_maxlog10)
  
  elMinMax_general <- c("kd_min", "kd_max", "hb_min", "hb_max")
  elMinMax_SD <- c(elMinMax_general, c("kk_min", "kk_max", "z_min", "z_max"))
  elMinMaxt_IT <- c(elMinMax_general, c("alpha_min", "alpha_max", "beta_min", "beta_max"))
  elMinMax_PROPER <- c(elMinMax_general, c("kk_min", "kk_max", "alpha_min", "alpha_max", "beta_min", "beta_max"))
  
  priorsMinMax <- switch(model_type,
                         IT = priorsMinMax[elMinMaxt_IT],
                         SD = priorsMinMax[elMinMax_SD],
                         PROPER = priorsMinMax[elMinMax_PROPER])
  
  ##
  ## Construction of the list of priors
  ##
  priorsList <-  list(
    ## dominant rate constant: kd
    kd_meanlog10 = .priorMean(kd_min, kd_max),
    kd_sdlog10 = .priorSD(kd_min, kd_max),
    ## background hazard rate
    hb_meanlog10 =  .priorMean(hb_min, hb_max),
    hb_sdlog10 = .priorSD(hb_min, hb_max),
    ## killing rate parameter: kk
    kk_meanlog10 = .priorMean(kk_min, kk_max),
    kk_sdlog10 = .priorSD(kk_min, kk_max),
    ## non effect threshold: z
    z_meanlog10 = .priorMean(conc_min, conc_max),
    z_sdlog10 = .priorSD(conc_min, conc_max),
    ## non effect threshold: scale parameter & median of a log-logistic distribution
    alpha_meanlog10 = .priorMean(conc_min, conc_max),
    alpha_sdlog10 = .priorSD(conc_min, conc_max),
    ## shape parameter of a log-logistic distribution
    beta_minlog10 = beta_minlog10,
    beta_maxlog10 = beta_maxlog10
  )
  
  elList_general <- c("kd_meanlog10", "kd_sdlog10", "hb_meanlog10", "hb_sdlog10")
  elList_SD <- c(elList_general, c("kk_meanlog10", "kk_sdlog10", "z_meanlog10", "z_sdlog10"))
  elList_IT <- c(elList_general, c("alpha_meanlog10", "alpha_sdlog10", "beta_minlog10", "beta_maxlog10"))
  elList_PROPER <- c(elList_general, c("kk_meanlog10", "kk_sdlog10", "alpha_meanlog10", "alpha_sdlog10", "beta_minlog10", "beta_maxlog10"))
  
  
  priorsList <- switch(model_type,
    IT = priorsList[elList_IT],
    SD = priorsList[elList_SD],
    PROPER = priorsList[elList_PROPER]
  )
  
  return(list(priorsList = priorsList,
              priorsMinMax = priorsMinMax))
}


# internal --------------------------------------------------------------------

# Compute priors Mean and SD for lognormal distribution

.priorMean <- function(x_min, x_max){
  (log10(x_max) + log10(x_min)) / 2
} 

.priorSD <- function(x_min, x_max){
  (log10(x_max) - log10(x_min)) / 4
}

# pool replicates with the same concentration
#
# @param x An object of class \code{survData}
# @return A dataframe
#

pool_survData <- function(x){
  
  bool_checkTimeReplicate <- checkTimeReplicate(x)
  
  if( bool_checkTimeReplicate ){
    ### Sum Nsurv data for each (conc, time) couple
    x_dev <- x %>%
      dplyr::group_by(conc, time) %>%
      dplyr::summarise(Nsurv = sum(Nsurv)) %>%
      # concate replicate in the same replicate using factor (conc)
      dplyr::mutate(replicate = as.character(conc)) %>%
      as.data.frame()
  } else{
    x_dev <- x
  }
  return(x_dev)
}


# Check the same number of (time, replicate) for a common concentration
# 
# @param x 

checkTimeReplicate <- function(x){
  df_checkTimeReplicate <- x %>%
    dplyr::group_by(conc, time) %>%
    dplyr::summarise(nbrReplicate_ConcTime = n_distinct(replicate)) %>%
    dplyr::group_by(conc) %>%
    dplyr::summarise(nbrReplicate_uniqueConc = length(unique(nbrReplicate_ConcTime)))
  
  return(all(df_checkTimeReplicate$nbrReplicate_uniqueConc == 1))
  
}


