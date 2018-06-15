# Ugly hack to get rid of spurious notes in package check, caused by uses
# of dplyr package. R is such a sad language.
utils::globalVariables(c(
  "time", "Nsurv", "conc", "q50", "qinf95", "qsup95",
  "id_all", "quantile", "survFit_TKTD_params",
  "nbrReplicate_ConcTime", "time_ID_red", "i_row",
  "time_ID_long", "time_ID_long", "time_ID_red", "."))

# Extract parameters
#
# 
extract_MCMCparameters <- function(x){
  # stanEstim <- switch(x$model_type,
  #                     SD =  rstan::extract(x$stanfit, pars = c("kd_log10", "hb_log10", "z_log10", "kk_log10")),
  #                     IT =  rstan::extract(x$stanfit, pars = c("kd_log10", "hb_log10", "alpha_log10", "beta_log10")),
  #                     PROPER =  rstan::extract(x$stanfit, pars = c("kd_log10", "hb_log10", "kk_log10", "alpha_log10", "beta_log10")))
  
  stanEstim <- rstan::extract(x$stanfit, pars = extract_MCMCparameters_name(x))
  
  df_stanEstim <- as_data_frame(stanEstim)
  return(df_stanEstim)
}

# Extract parameters name for a \code{stanguts} object
# 
# @export
#  
extract_MCMCparameters_name <- function(x){
  
  stanParam <- switch(x$model_type,
                      SD =  c("kd_log10", "hb_log10", "z_log10", "kk_log10"),
                      IT =  c("kd_log10", "hb_log10", "alpha_log10", "beta_log10"),
                      PROPER =  c("kd_log10", "hb_log10", "kk_log10", "alpha_log10", "beta_log10"))
  
  return(stanParam)
}

# Extract prediction
# 
# @importfrom dplyr as_data_frame
# 
# @export
# 
extract_MCMCppc <- function(x){
  stanPredict <- rstan::extract(x$stanfit, pars = c("Nsurv_ppc"))
  mat_stanPredict <- stanPredict$Nsurv_ppc
  return(mat_stanPredict)
}