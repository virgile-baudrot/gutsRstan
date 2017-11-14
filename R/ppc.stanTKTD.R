#' Posterior predictive check plot
#' 
#' Plots posterior predictive check for \code{stanTKTD} objects.
#' 
#' @param x an object used to select a method
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @export
#' 
ppc <- function(x, ...){
  UseMethod("ppc")
}


#' PPC
#' 
#' @param x an object of class \code{stanTKTD}
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @export
#' 
#' 
ppc.stanTKTD <- function(x, facetting = FALSE, ...){
  
  x_stanfit <- x$stanfit
  x_data <- x$dataStan
  
  Nsurv_ppc <- extract(x_stanfit, pars = 'Nsurv_ppc')
  
  df_Nsurv <- data.frame(Nsurv = x_data$Nsurv,
                         time = x_data$tNsurv,
                         replicate = x_data$replicate_Nsurv,
                         q50 = apply(Nsurv_ppc[[1]], 2, quantile, 0.5),
                         qinf95 = apply(Nsurv_ppc[[1]], 2, quantile, 0.025),
                         qsup95 = apply(Nsurv_ppc[[1]], 2, quantile, 0.975)) %>%
    mutate(color = ifelse(Nsurv < qinf95 | Nsurv > qsup95, "out", "in"))
  
  ppc_plt <- ggplot(data = df_Nsurv) + theme_bw() +
    geom_abline() +
    scale_colour_manual(values = c("darkgreen", "red")) +
    geom_pointrange( aes(x = Nsurv, y = q50, ymin = qinf95, ymax = qsup95, group = replicate, color = color),
                     size = 0.4, alpha = 0.5,position = position_dodge(width = 0.5))
    
  if(facetting == TRUE){
    ppc_plt <- ppc_plt + facet_wrap(~ replicate, scale = "free")
  }  
    
  return(ppc_plt)
  
}
