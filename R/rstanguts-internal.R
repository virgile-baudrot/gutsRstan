#' Extract parameters
#' 
#' @importfrom dplyr as_data_frame
#' 
#' @export
#' 
extract_MCMCparameters <- function(x){
  # stanEstim <- switch(x$model_type,
  #                     SD =  rstan::extract(x$stanfit, pars = c("kd_log10", "hb_log10", "z_log10", "kk_log10")),
  #                     IT =  rstan::extract(x$stanfit, pars = c("kd_log10", "hb_log10", "alpha_log10", "beta_log10")),
  #                     PROPER =  rstan::extract(x$stanfit, pars = c("kd_log10", "hb_log10", "kk_log10", "alpha_log10", "beta_log10")))
  
  stanEstim <- rstan::extract(x$stanfit, pars = extract_MCMCparameters_name(x))
  
  df_stanEstim <- as_data_frame(stanEstim)
  return(df_stanEstim)
}

#' Extract parameters name for a \code{stanguts} object
#' 
#' @export
#'  
extract_MCMCparameters_name <- function(x){
  
  stanParam <- switch(x$model_type,
                      SD =  c("kd_log10", "hb_log10", "z_log10", "kk_log10"),
                      IT =  c("kd_log10", "hb_log10", "alpha_log10", "beta_log10"),
                      PROPER =  c("kd_log10", "hb_log10", "kk_log10", "alpha_log10", "beta_log10"))
  
  return(stanParam)
}

#' Extract prediction
#' 
#' @importfrom dplyr as_data_frame
#' 
#' @export
#' 
extract_MCMCppc <- function(x){
  stanPredict <- rstan::extract(x$stanfit, pars = c("Nsurv_ppc"))
  mat_stanPredict <- stanPredict$Nsurv_ppc
  return(mat_stanPredict)
}

#' Extract the \code{stanfit} object of a \code{stanguts} object
#' 
#' Extract the \code{stanfit} object of a \code{stanguts} object.
#' 
#' @param x an object used to select a method
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @export
#' 
extract_stanfit <- function(x, ...){
  UseMethod("extract_stanfit")
}

#' Extract the \code{stanfit} object of a \code{stanguts} object
#' 
#' @param x a,n object of class \code{stanguts}
#' 
#' @export
#'  
extract_stanfit.stanguts <- function(x){
  return(x$stanfit)
}
