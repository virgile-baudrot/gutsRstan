#' Plot
#' 
#' @export
#' 
plot.survFitODE <- function(x, data_type = NULL){

  x_stanfit <- x$stanfit
  x_data <- x$dataStan
  
  if(data_type == 'Number'){
    
    Nsurv_sim <- extract(x_stanfit, pars = 'Nsurv_sim')
    
    df_Nsurv <- data.frame(Nsurv = x_data$Nsurv,
                           time = x_data$tNsurv,
                           replicate = x_data$replicate_Nsurv,
                           q50 = apply(Nsurv_sim[[1]], 2, quantile, 0.5),
                           qinf95 = apply(Nsurv_sim[[1]], 2, quantile, 0.025),
                           qsup95 = apply(Nsurv_sim[[1]], 2, quantile, 0.975))
    
  } else if(data_type == 'Rate'){
    
    Psurv_sim <- extract(x_stanfit, pars = 'Psurv_hat')
    
    df_Nsurv <- data.frame(Nsurv = x_data$Nsurv/x_data$Ninit,
                           time = x_data$tNsurv,
                           replicate = x_data$replicate_Nsurv,
                           q50 = apply(Psurv_sim[[1]], 2, quantile, 0.5),
                           qinf95 = apply(Psurv_sim[[1]], 2, quantile, 0.025),
                           qsup95 = apply(Psurv_sim[[1]], 2, quantile, 0.975))
    
  } else stop("'data_type' must be 'Rate' for the survival rate, or 'Number' for the number of survivors")
  
  plot <- ggplot(data = df_Nsurv) + theme_bw() +
      geom_pointrange( aes(x = time, y = q50, ymin = qinf95, ymax = qsup95, group = replicate), color = "red", size = 0.2) +
      geom_line(aes(x = time, y = q50,  group = replicate), color = "red") + 
      geom_ribbon(aes(x= time, ymin = qinf95, ymax = qsup95, group = replicate), fill = "pink", alpha = 0.2)+
      geom_point( aes(x = time, y = Nsurv, group = replicate) ) +
      #geom_errorbar( aes(x = time, ymin = qinf95, ymax = qsup95, group = replicate), color = "pink", width = 0.5) +
      facet_wrap(~ replicate)
  
  return(plot)
}

