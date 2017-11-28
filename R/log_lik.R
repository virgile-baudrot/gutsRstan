#' Generic function for pointwise log-likelihood
#'
#' We define a new function \code{log_lik} rather than a
#' \code{\link[stats]{logLik}} method because (in addition to the conceptual
#' difference) the documentation for \code{logLik} states that the return value
#' will be a single number, whereas \code{log_lik} returns a matrix. See this 
#' package \pkg{rstanTKTD} the function \code{\link[rstanTKTD]{log_lik.stanTKTD}}
#' or \code{\link[rstanarm]{log_lik.stanreg}} in the \pkg{rstanarm} package
#' for an example.
#'
#' @export
#'
#' @return \code{log_lik} methods should return a \eqn{S} by \eqn{N} matrix,
#'   where \eqn{S} is the size of the posterior sample (the number of draws from
#'   the posterior distribution) and \eqn{N} is the number of data points.
#'
#'
#' @examples
#' # See help("log_lik", package = "rstanTKTD")
#'
log_lik <- function(x, ...) {
  UseMethod("log_lik")
}

#' Pointwise log-likelihood matrix
#'
#' For models fit using MCMC only, the \code{log_lik} method returns the
#' \eqn{S} by \eqn{N} pointwise log-likelihood matrix, where \eqn{S} is the size
#' of the posterior sample and \eqn{N} is the number of data points.
#'
#' @aliases log_lik
#' @export
#'
#' @param newdata An optional data frame of new data (e.g. holdout data) to use
#'   when evaluating the log-likelihood. See the description of \code{newdata}
#'   for \code{\link{posterior_predict}}.
#'   
#' @return For the \code{stanTKTD} method an \eqn{S} by 
#'   \eqn{N} matrix, where \eqn{S} is the size of the posterior sample and 
#'   \eqn{N} is the number of data points.
#'   
#'   
#' @examples 
#' \donttest{
#' 
#' }
#'
#'
log_lik.stanTKTD <- function(x,
                             newdata = NULL,
                             draws = NULL,
                             interpolate_length = 1e2,
                             interpolate_method = "linear",
                             mc.cores = 1,
                             ...){
  
  # Observation
  if(is.null(newdata)){
    x_data =  x$data
  } else{
    x_data = newdata
  }
    
  filter_obs <- x_data %>%
    dplyr::select(replicate, time, Nsurv) %>%
    dplyr::mutate(replicate = as.character(replicate)) %>%
    dplyr::filter(!is.na(Nsurv)) %>%
    group_by(replicate) %>%
    dplyr::mutate(Nsurv_post = ifelse( time == max(time), 0, dplyr::lead(Nsurv)),
                  Nsurv_diff = Nsurv - Nsurv_post,
                  nrow_label = row_number(),
                  max_time = max(time))

  # Estimation of probabiblity of survival
  if(is.null(newdata)){
    
    mat_psurv <- t(rstan::extract(x$stanfit, pars = c("Psurv_hat"))$Psurv_hat)
      
  } else {
    
    mat_psurvPredict <- psurv_predict(x, newdata, draws, interpolate_length, interpolate_method, mc.cores)
    
    # inner_join
    toJoin_posterior_predict <- dplyr::select(mat_psurvPredict, -conc)
    
    toJoin_newdata <- filter_obs %>%
      dplyr::select(-c(Nsurv_post, Nsurv_diff, nrow_label, max_time))
    
    join_predict <- dplyr::inner_join(x = toJoin_posterior_predict, y = toJoin_newdata, by = c('replicate','time'))
    
    mat_psurv <- dplyr::select(join_predict, -c(time, Nsurv, replicate)) %>%
      as.matrix()
    
  }
  
  mat_psurv_post <- matrix(NA, nrow = nrow(mat_psurv), ncol = ncol(mat_psurv))
  
  for(i in 1:nrow(mat_psurv)){
    if(filter_obs$time[i] == filter_obs$max_time[i]){
      mat_psurv_post[i, 1:ncol(mat_psurv)] <- rep(0, ncol(mat_psurv))
    } else{
      mat_psurv_post[i, 1:ncol(mat_psurv)] <- mat_psurv[(i+1), 1:ncol(mat_psurv)]
    }
  }
  
  mat_psurv_diff <- mat_psurv - mat_psurv_post
  
  # Computing of the likelihood
  mat_log_lik <- filter_obs$Nsurv_diff * log(mat_psurv_diff)
  
  ## For the package loo, the matrix as to be An S by N matrix, where
  ## S is the size of the posterior sample (the number of simulations) and
  ## N is the number of data points.
  transpose_mat_log_lik <-  t(mat_log_lik)

  return(transpose_mat_log_lik)
}


# Vectorized log_likelihood

matTovec_loglik <- function(mat_log_lik){
  
  vec_log_lik <- apply(mat_log_lik, 2, sum)
  
}
