plot.predict.stanTKTD <- function(x, newdata = NULL){
  
  prediction <- predict(x, new_data = newdata)
  
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
  
  
  ggplot(data = df_Nsurv) + theme_light() +
    scale_y_continuous(limits = y_limits) +
    geom_ribbon(data = df_prediction,
                aes(x = time, ymin = qinf95, ymax = qsup95), fill = "grey70") +
    geom_line(data = df_prediction,
              aes(x = time, y = q50)) +
    
    geom_pointrange( aes(x = time, y = q50, ymin = qinf95, ymax = qsup95, group = replicate), color = "red", size = 0.2) +
    geom_line(aes(x = time, y = q50,  group = replicate), color = "red") +
    geom_ribbon(aes(x= time, ymin = qinf95, ymax = qsup95, group = replicate), fill = "pink", alpha = 0.2)+
    geom_point( aes(x = time, y = Nsurv, group = replicate) ) +
    facet_wrap(~ replicate)
  
}