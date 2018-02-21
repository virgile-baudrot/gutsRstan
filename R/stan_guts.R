#' stan_guts
#' 
#' @param data A dataset to fit
#' @param model_type The type of the model: 'SD', 'IT' or 'PROPER'
#' 
#' 
#' @export
#' 
stan_guts <- function(data,
                      # specific to stan_guts
                      model_type = NULL,
                      distribution = NULL,
                      rel_tol = 1e-8,
                      abs_tol = 1e-8,
                      max_num_steps = 1e3,
                      ode_integrator = "rk45", # other is "bdf"
                      priors_list = NULL,
                      # rsta::sampling
                      adapt_delta = 0.95,
                      chains = 3,
                      iter = 2000,
                      warmup = 1000,
                      thin = 1,
                      ...){
  
  ### ensures model_type is one of "SD" and "IT"
  if(is.null(model_type) || ! (model_type %in% c("SD","IT", "PROPER"))) {
    stop("You need to specify a 'model_type' among 'SD', 'IT' or 'PROPER'.")
  }
  if(!(ode_integrator %in% c("rk45", "rk45_2", "bdf"))){
    stop("You need to specify an 'ode_integrator' among 'rk45' and 'bdf'.")
  }
  
  ode_control <- list(rel_tol = rel_tol, abs_tol = abs_tol, max_num_steps = max_num_steps)

  dataStan_withReplicate <- modelDataStan(data, model_type, ode_control, priors_list)
  dataStan <- dataStan_withReplicate
  dataStan$replicate_conc = NULL
  dataStan$replicate_Nsurv = NULL
  dataStan$Ninit = NULL
  
  if(model_type == "SD"){
    model_object <- stanmodels$ode_TKTD_varSD
  } else if(model_type == "IT"){
    if(ode_integrator == "rk45"){
      model_object <- stanmodels$ode_TKTD_varIT
    }
    if(ode_integrator == "bdf"){
      model_object <- stanmodels$ode_TKTD_varIT_bdf
    }
  } else if(model_type == "PROPER" && distribution == "loglogistic"){
    dataStan$proper_distribution = 1
    model_object <- stanmodels$ode_TKTD_varPROPER
  } else if(model_type == "PROPER" && distribution == "lognormal"){
    dataStan$proper_distribution = 2
    model_object <- stanmodels$ode_TKTD_varPROPER
  } else stop("'model_type' must be 'SD', 'IT' or 'PROPER'. For 'PROPER' models,
              please add the distribution 'loglogistic' or 'lognormal'.")
  
  fit <- rstan::sampling(
    object = model_object,
    data = dataStan,
    control = list(adapt_delta = adapt_delta),
    chains = chains,
    iter = iter,
    warmup = warmup,
    thin = thin,
    ...)
  
  ##
  ## MCMC information
  ## 
  mcmcInfo <- data.frame(n.iter = iter,
                        n.chains = chains,
                        thin.interval = thin,
                        n.warmup = warmup)
  
  ## OBJECT TO RETURN
  
  ls_out <- list(stanfit = fit,
                 data = data,
                 dataStan = dataStan_withReplicate,
                 mcmcInfo = mcmcInfo,
                 model_type = model_type,
                 distribution = distribution)
  
  class(ls_out) <- "stanguts"
  
  ## ------ WARNINGS
  
  out_rhat <- summary(ls_out$stanfit)$summary[, "Rhat"]
  
  if (!all(out_rhat < 1.1, na.rm = TRUE)){
    ##possibility to store warning in a 'warnings table'
    msg <- "*** Markov chains did not converge! Do not analyze results! ***. 
Plot MCMC chains and try the following options:
 (1) if one or more chain are a simple stable line, increase 'adapt_delta' (default is 0.95).
 (2) if the variabbility between chain is great, you can increase the number of iteration (default is 2000 iteration).
 (3) if 'Conditional_Psurv_hat' is greater than 1, the ODE integration is wrong. So you can reduce the tolerance of the ODE integrator."
    # warnings <- msgTableAdd(warnings, "rhat", msg)
    ## print the message
    warning(msg, call. = FALSE)
  }
  
  return(ls_out)
}
