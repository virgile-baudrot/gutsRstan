plot.predict.stanTKTD <- function(x,
                                  one.plot = FALSE,
                                  ncol= NULL,
                                  newdata = NULL){
  
  prediction <- rstanTKTD::posterior_predict(x, newdata = newdata)

  df_prediction <- do.call("rbind", prediction)
  
  if(is.null(newdata)){
    x_data <- data.frame(Nsurv = x$dataStan$Nsurv,
                         Ninit = x$dataStan$Ninit,
                         time = x$dataStan$tNsurv,
                         replicate = x$dataStan$replicate_Nsurv)
  } else{
    x_data <- newdata 
    if("Nsurv" %in% colnames(x_data)){
      x_data <- newdata %>%
        dplyr::group_by(replicate) %>%
        dplyr::mutate(Ninit = max(Nsurv, na.rm =TRUE))
    } 
  }

  plt <- ggplot() + theme_light() +
            scale_y_continuous(limits = c(0,1)) +
            geom_ribbon(data = df_prediction,
                        aes(x = time, ymin = qinf95, ymax = qsup95), fill = "grey70") +
            geom_line(data = df_prediction,
                      aes(x = time, y = q50)) 
  
  if("Nsurv" %in% colnames(x_data)){
    
    plt <- plt +
      geom_point(data = x_data, aes(x = time, y = Nsurv/Ninit, group = replicate))
  }
  
  if(one.plot == FALSE){
    plt = plt + facet_wrap(~ replicate, ncol = ncol)
  }
  
  return(plt)
}

plotQuality.predict.stanTKTD <- function(x, newdata = NULL){
  
  prediction <- rstanTKTD::posterior_predict(x, newdata = newdata)
  
  df_prediction <- do.call("rbind", prediction)
  
  x_stanfit <- x$stanfit
  x_data <- x$dataStan
  
  Psurv_sim <- rstan::extract(x_stanfit, pars = 'Psurv_hat')
  
  df_Nsurv <- data.frame(Nsurv = x_data$Nsurv/x_data$Ninit,
                         time = x_data$tNsurv,
                         replicate = x_data$replicate_Nsurv,
                         q50 = apply(Psurv_sim[[1]], 2, quantile, 0.5),
                         qinf95 = apply(Psurv_sim[[1]], 2, quantile, 0.025),
                         qsup95 = apply(Psurv_sim[[1]], 2, quantile, 0.975))
  y_limits = c(0,1)
  
  
plt <-  ggplot(data = df_Nsurv) + theme_light() +
    scale_y_continuous(limits = y_limits) +
    geom_ribbon(data = df_prediction,
                aes(x = time, ymin = qinf95, ymax = qsup95), fill = "grey70") +
    geom_line(data = df_prediction,
              aes(x = time, y = q50)) +
    
    geom_pointrange( aes(x = time, y = q50, ymin = qinf95, ymax = qsup95, group = replicate), color = "red", size = 0.2) +
    geom_line(aes(x = time, y = q50,  group = replicate), color = "red") +
    geom_ribbon(aes(x= time, ymin = qinf95, ymax = qsup95, group = replicate), fill = "pink", alpha = 0.2)+
    geom_point( aes(x = time, y = Nsurv, group = replicate) ) 
  
  if(one.plot == FALSE){
    plt = plt + facet_wrap(~ replicate)
  }
  
  
}
