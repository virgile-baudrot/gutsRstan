#' survFitODE
#' 
#' @param data A dataset to fit
#' @param model_type The type of the model: 'SD', 'IT' or 'PROPER'
#' 
#' 
#' @export
#' 
survFitODE <- function(data, model_type = NULL, under_type = NULL, distribution = NULL, ...){
  
  ### ensures model_type is one of "SD" and "IT"
  if(is.null(model_type) || ! (model_type %in% c("SD","IT", "PROPER"))) {
    stop("You need to specify a 'model_type' among 'SD', 'IT' or 'PROPER'.")
  }
  
  dataStan_withReplicate <- modelDataStan(data, model_type)
  dataStan <- dataStan_withReplicate
  dataStan$replicate_conc = NULL
  dataStan$replicate_Nsurv = NULL
  dataStan$Ninit = NULL
  
  if(model_type == "SD"){
    model_object <- stanmodels$ode_TKTD_varSD
  } else if(model_type == "IT"){
    model_object <- stanmodels$ode_TKTD_varIT
  } else if(model_type == "IT" && under_type == "2"){
    model_object <- stanmodels$ode_TKTD_varIT_2
  } else if(model_type == "PROPER" && distribution == "loglogistic"){
    model_object <- stanmodels$ode_TKTD_varPROPER_loglogistic
  } else if(model_type == "PROPER" && distribution == "lognormal"){
    model_object <- stanmodels$ode_TKTD_varPROPER_lognormal
  } else stop("'model_type' must be 'SD', 'IT' or 'PROPER'. For 'PROPER' models, please add the distribution 'loglogistic' or 'lognormal'.")
  
  fit <- rstan::sampling(
    object = model_object,
    data = dataStan,
    ...)
  
  ls_out <- list(stanfit = fit,
                 data = data,
                 dataStan = dataStan_withReplicate,
                 model_type = model_type,
                 distibution = distribution)
  
  class(ls_out) <- "survFitODE"
  
  ## ------ WARNINGS
  
  out_rhat <- summary(ls_out$stanfit)$summary[, "Rhat"]
  
  if (!all(out_rhat < 1.1, na.rm = TRUE)){
    ##store warning in warnings table
    msg <- "*** Markov chains did not converge! Do not analyze results! ***. You may increase iter number."
    # warnings <- msgTableAdd(warnings, "rhat", msg)
    ## print the message
    warning(msg, call. = FALSE)
  }
  
  return(ls_out)
}
