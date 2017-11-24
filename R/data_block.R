#' @import morse
#' 
#' @export
#' 
modelDataStan <- function(data, model_type = NULL){
  
  #------------ gather replicate when data is constant:
  if("survDataCstExp" %in% class(data)){ # data is a survData object
    ## 1. Gather replicate when there is the same constante concentration
    data <- morse:::gather_survDataCstExp(data)
  }
  
  # --------------------------------------------------
  
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
    dplyr::mutate(Nprec = ifelse( time == 0, Nsurv, dplyr::lag(Nsurv) ),
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

  # PRIORS
    
  priors <- priors_survData(data, model_type = model_type)$priorsList
    
  ls_OUT_2 <- unlist(list(ls_OUT, priors), recursive = FALSE)
    
  return(ls_OUT_2)
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
    hb_max = hb_max )
  
  ##
  ## Construction of the list of priors
  ##
  
  priorsList <-  list(
    ##
    ## dominant rate constant: kd
    ##
    kd_meanlog10 = (log10(kd_max) + log10(kd_min)) / 2 ,
    kd_sdlog10 = (log10(kd_max) - log10(kd_min)) / 4 ,
    ##
    ## background hazard rate
    ##
    hb_meanlog10 = (log10(hb_max) + log10(hb_min)) / 2 ,
    hb_sdlog10 = (log10(hb_max) - log10(hb_min)) / 4
  )
  
  if(model_type == "IT"){
    
    ## priorsMinMax
    priorsMinMax$beta_min <- beta_minlog10
    priorsMinMax$beta_max <- beta_maxlog10
    
    ## priorsList
    ### non effect threshold: scale parameter & median of a log-logistic distribution
    priorsList$alpha_meanlog10 <- (log10(conc_max) + log10(conc_min)) / 2
    priorsList$alpha_sdlog10 <- (log10(conc_max) - log10(conc_min)) / 4
    
    ### shape parameter of a log-logistic distribution
    priorsList$beta_minlog10 <- beta_minlog10
    priorsList$beta_maxlog10 <- beta_maxlog10
    
  } else if (model_type == "SD"){
    
    ## priorsMinMax
    priorsMinMax$kk_min <- kk_min
    priorsMinMax$kk_max <- kk_max
    
    ## priorsList
    ### killing rate parameter: kk
    priorsList$kk_meanlog10 <- (log10(kk_max) + log10(kk_min)) / 2
    priorsList$kk_sdlog10 <- (log10(kk_max) - log10(kk_min)) / 4
    ### non effect threshold: z
    priorsList$z_meanlog10 <- (log10(conc_max) + log10(conc_min)) / 2
    priorsList$z_sdlog10 <- (log10(conc_max) - log10(conc_min)) / 4
  } else if (model_type == "PROPER"){
    
    ## priorsMinMax
    priorsMinMax$kk_min <- kk_min
    priorsMinMax$kk_max <- kk_max
    
    ## priorsList
    ### killing rate parameter: kk
    priorsList$kk_meanlog10 <- (log10(kk_max) + log10(kk_min)) / 2
    priorsList$kk_sdlog10 <- (log10(kk_max) - log10(kk_min)) / 4

    ## priorsMinMax
    priorsMinMax$beta_min <- beta_minlog10
    priorsMinMax$beta_max <- beta_maxlog10
    
    ## priorsList
    ### non effect threshold: scale parameter & median of a log-logistic distribution
    priorsList$alpha_meanlog10 <- (log10(conc_max) + log10(conc_min)) / 2
    priorsList$alpha_sdlog10 <- (log10(conc_max) - log10(conc_min)) / 4
    
    ### shape parameter of a log-logistic distribution
    priorsList$beta_minlog10 <- beta_minlog10
    priorsList$beta_maxlog10 <- beta_maxlog10
    
  } else stop("please, provide the 'model_type': 'SD', 'IT' or 'PROPER'.")
  
  
  return(list(priorsList = priorsList,
              priorsMinMax = priorsMinMax))
}


