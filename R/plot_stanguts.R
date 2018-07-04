#' @title Plotting method for \code{stanguts} objects
#'
#' @description This is a \code{plot} method for the
#' \code{stanguts} object. It plots the fit obtained for each
#' profile of chemical compound in the original dataset.
#'
#' @details The fitted curves represent the \strong{estimated survival rate} as a function
#' of time for each profile of concentration.
#' The black dots depict the \strong{observed survival
#' rate} at each time point.
#' The function plots thz 95\% credible intervals for the estimated survival
#' rate (by default the grey area around the fitted curve).
#' 
#' @param stanguts An object of class \code{stanguts}
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return An object of class \code{("gg","ggplot")}. See package \link{ggplot2} 
#'   for further information.
#' 
#' @export
plot_stanguts <- function(stanguts, ...){
  UseMethod("plot_stanguts")
}
#' @param data_type The type of data to plot: either \code{"Rate"} for the
#'   survival rate, or \code{"Number"} for the number of survivors. The
#'   default is the survival rate.
#' @param x_lab Label of the x-axis
#' @param y_lab Label of the y-axis
#' @param title Title of the graph
#' 
#' @examples
#'
#' # (1) Load the survival data
#' data("data_Diazinon")
#'
#' \dontrun{
#' # (2) Run the stan_guts function 
#' fit_SD_diaz <- stan_guts(data_Diazinon, model_type = "SD")
#'
#' # (3) Plot the fitted curve
#' plot_stanguts(fit_SD_diaz)
#' }
#' 
#' @rdname plot_stanguts
#' @export
#' 
#' @import ggplot2
#'
plot_stanguts.stanguts <- function(stanguts,
                                   data_type = "Rate",
                                   x_lab = "Time",
                                   y_lab = NULL,
                                   title = NULL,
                                   ...){
  
  x_stanfit <- stanguts$stanfit
  x_data <- stanguts$dataStan
  
  if(data_type == 'Number'){
    if(is.null(y_lab)){ y_lab <-"Number of survivors"}
    
    Nsurv_sim <- extract(x_stanfit, pars = 'Nsurv_sim')
    
    df_Nsurv <- data.frame(Nsurv = x_data$Nsurv,
                           time = x_data$tNsurv,
                           replicate = x_data$replicate_Nsurv,
                           q50 = apply(Nsurv_sim[[1]], 2, quantile, 0.5),
                           qinf95 = apply(Nsurv_sim[[1]], 2, quantile, 0.025),
                           qsup95 = apply(Nsurv_sim[[1]], 2, quantile, 0.975))
    
    y_limits = c(0, max(df_Nsurv$Nsurv, df_Nsurv$qsup95))
    
  } else if(data_type == 'Rate'){
    if(is.null(y_lab)){ y_lab <- "Survival rate"}
    
    Psurv_sim <- extract(x_stanfit, pars = 'Psurv_hat')
    
    df_Nsurv <- data.frame(Nsurv = x_data$Nsurv/x_data$Ninit,
                           time = x_data$tNsurv,
                           replicate = x_data$replicate_Nsurv,
                           q50 = apply(Psurv_sim[[1]], 2, quantile, 0.5),
                           qinf95 = apply(Psurv_sim[[1]], 2, quantile, 0.025),
                           qsup95 = apply(Psurv_sim[[1]], 2, quantile, 0.975))
    
    y_limits = c(0,1)
    
  } else stop("'data_type' must be 'Rate' for the survival rate, or 'Number' for the number of survivors")
  
  plot <- ggplot(data = df_Nsurv) +
    theme_minimal() +
    labs(x = x_lab, y = y_lab, title = title) +
    scale_y_continuous(limits = y_limits) +
    geom_pointrange( aes(x = time, y = q50, ymin = qinf95, ymax = qsup95, group = replicate), color = "orange", size = 0.2) +
    geom_line( aes(x = time, y = q50,  group = replicate), color = "orange") +
    geom_ribbon( aes(x= time, ymin = qinf95, ymax = qsup95, group = replicate), fill = "lightgrey", alpha = 0.2)+
    geom_point( aes(x = time, y = Nsurv, group = replicate) ) +
    #geom_errorbar( aes(x = time, ymin = qinf95, ymax = qsup95, group = replicate), color = "lightgrey", width = 0.5) +
    facet_wrap(~ replicate)
  
  return(plot)
}
