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

#' Extract parameters name for a \code{stanTKTD} object
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

#' Extract the \code{stanfit} object of a \code{stanTKTD} object
#' 
#' Extract the \code{stanfit} object of a \code{stanTKTD} object.
#' 
#' @param x an object used to select a method
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @export
#' 
extract_stanfit <- function(x, ...){
  UseMethod("extract_stanfit")
}

#' Extract the \code{stanfit} object of a \code{stanTKTD} object
#' 
#' @param x a,n object of class \code{stanTKTD}
#' 
#' @export
#'  
extract_stanfit.stanTKTD <- function(x){
  return(x$stanfit)
}

#' Extract the \code{stanfit} object of a \code{stanTKTD} object
#' 
#' Extract the \code{stanfit} object of a \code{stanTKTD} object.
#' 
#' @param x an object used to select a method
#' @param \dots Further arguments to be passed to generic methods
#' 
#' @export
#' 
extract_log_lik <- function(x, ...){
  UseMethod("extract_log_lik")
}

#' Compute the likelihoo
#' 
#' @export
#' 
extract_log_lik.stanTKTD <- function(x){
  
  # Observation
  df_obs <- data.frame(Nsurv = x$dataStan$Nsurv,
                       tNsurv = x$dataStan$tNsurv,
                       replicate = x$dataStan$replicate_Nsurv) %>%
    group_by(replicate) %>%
    dplyr::mutate(Nsurv_post = ifelse( tNsurv == max(tNsurv), 0, dplyr::lead(Nsurv)),
                  Nsurv_diff = Nsurv - Nsurv_post,
                  nrow_label = row_number(),
                  max_tNsurv = max(tNsurv))

  # Estimated probabiblity of survival
  
  mat_psurv <- t(rstan::extract(x$stanfit, pars = c("Psurv_hat"))$Psurv_hat)
  
  mat_psurv_post <- matrix(nrow = nrow(mat_psurv), ncol = ncol(mat_psurv))
  
  for(i in 1:nrow(mat_psurv)){
    if(df_obs$tNsurv[i] == df_obs$max_tNsurv[i]){
      mat_psurv_post[i, 1:ncol(mat_psurv)] = rep(0, ncol(mat_psurv))
    } else{
      mat_psurv_post[i, 1:ncol(mat_psurv)] = mat_psurv[(i+1), 1:ncol(mat_psurv)]
    }
  }
  mat_psurv_diff <- mat_psurv - mat_psurv_post

  # Computing of the likelihood
  mat_log_lik <- df_obs$Nsurv_diff * log(mat_psurv_diff)
  vec_log_lik <- apply(mat_log_lik, 2, sum)
  
  ### For the package loo, the matrix as to be An S by N matrix, where
  ### S is the size of the posterior sample (the number of simulations) and
  ### N is the number of data points.
  transpose_mat_log_lik <-  t(mat_log_lik)
  
  ls_log_lik <- list(mat_log_lik = transpose_mat_log_lik,
                     vec_log_lik = vec_log_lik)
  
  return(ls_log_lik)
}

