#' Summary for \code{stanTKTD} objects
#'
#' This is the generic \code{summary} S3 methode for the \code{stanTKTD} class.
#' It shows the quantiles of priors and posteriors on parameters.
#'
#' @param object An object of class \code{stanTKTD}
#'
#' @return The function returns a list with the following fields:
#' \item{Qpriors}{quantiles for the model's prior}
#' \item{Qposteriors}{quantiles for the model's posteriors}
#'
#' @examples
#' # (1) Load the data
#' data(propiconazole)
#'
#' # (2) Create a survData object
#' dat <- survData(propiconazole)
#'
#' \dontrun{
#' # (3) Run the 'stan_guts' function
#' out <- stan_guts(dat, model_type = "SD")
#'
#' # (4) summarize the stanTKTD object
#' summary(out)
#' }
#'
#' @keywords summary
#'
#' @importFrom stats qnorm qunif
#' 
#' @export
#' 

summary.stanTKTD <- function(x, quiet = FALSE, ...) {
  
  param <- x$dataStan
  
  quantiles <- c(0.5, 0.025, 0.975)
  
  # kd
  kd <- 10^qnorm(p = quantiles,
                 mean = param$kd_meanlog10,
                 sd = param$kd_sdlog10)

  
  
  # hb
  hb <- 10^qnorm(p = quantiles,
                    mean = param$hb_meanlog10,
                    sd = param$hb_sdlog10)

  
  if(x$model_type == "SD"){
    
    # kk
    kk <- 10^qnorm(p = quantiles,
                  mean = param$kk_meanlog10,
                  sd = param$kk_sdlog10)

    
    ## z
    z <- 10^qnorm(p = quantiles,
                     mean = param$z_meanlog10,
                     sd = param$z_sdlog10)
    
    res <- data.frame(parameters = c("kd", "hb", "z", "kk"),
                      median = c(kd[1], hb[1], z[1], kk[1]),
                      Q2.5 = c(kd[2], hb[2], z[2], kk[2]),
                      Q97.5 = c(kd[3], hb[3], z[3], kk[3]))
    
  }
  if(x$model_type == "IT"){
    
    # alpha
    alpha <- 10^qnorm(p = quantiles,
                         mean = param$alpha_meanlog10,
                         sd = param$alpha_sdlog10)

    # beta
    beta <- 10^qunif(p = quantiles,
                        min = param$beta_minlog10,
                        max = param$beta_maxlog10)
    
    res <- data.frame(parameters = c("kd", "hb", "alpha", "beta"),
                      median = c(kd[1], hb[1], alpha[1], beta[1]),
                      Q2.5 = c(kd[2], hb[2], alpha[2], beta[2]),
                      Q97.5 = c(kd[3], hb[3], alpha[3], beta[3]))

  }
  if(x$model_type == "PROPER"){
    # kk
    kk <- 10^qnorm(p = quantiles,
                   mean = param$kk_meanlog10,
                   sd = param$kk_sdlog10)
    
    # alpha
    alpha <- 10^qnorm(p = quantiles,
                      mean = param$alpha_meanlog10,
                      sd = param$alpha_sdlog10)
    
    # beta
    beta <- 10^qunif(p = quantiles,
                     min = param$beta_minlog10,
                     max = param$beta_maxlog10)
    
    res <- data.frame(parameters = c("kd", "hb", "kk", "alpha", "beta"),
                      median = c(kd[1], hb[1], kk[1], alpha[1], beta[1]),
                      Q2.5 = c(kd[2], hb[2], kk[2], alpha[2], beta[2]),
                      Q97.5 = c(kd[3], hb[3], kk[3], alpha[3], beta[3]))
    
  }
  
  ans1 <- format(res, scientific = TRUE, digits = 4)
  
  # 
  # quantiles of estimated model parameters
  #

  df_stanEstim <- extract_MCMCparameters(x)
  
  parameters <- switch(x$model_type,
                       SD =  c("kd", "hb", "z", "kk"),
                       IT =  c("kd", "hb", "alpha", "beta"),
                       PROPER =  c("kd", "hb", "kk", "alpha", "beta"))
  
  res2 <- data.frame(parameters = parameters,
                     median = as.numeric(10^apply(df_stanEstim, 2, quantile, 0.5)),
                     Q2.5 = as.numeric(10^apply(df_stanEstim, 2, quantile, 0.025)),
                     Q97.5 = as.numeric(10^apply(df_stanEstim, 2, quantile, 0.975)))

  ans2 <- format(res2, scientific = TRUE, digits = 4)
  
  # print
  if(! quiet){
    cat("Summary: \n\n")
    cat("Priors of the parameters (quantiles) (select with '$Qpriors'):\n\n")
    print(ans1)
    cat("\n\nPosterior of the parameters (quantiles) (select with '$Qposteriors'):\n\n")
    print(ans2)
  }
  
  invisible(list(Qpriors = ans1,
                 Qposteriors = ans2))
}
