#' survFitODE
#' 
#' @param data A dataset to fit
#' @param model_type The type of the model: 'SD' or 'IT'
#' 
#' 
#' @export
#' 
survFitODE <- function(data, model_type = NULL, ...){
  
  dataStan_withReplicate <- modelDataStan(data, model_type)
  dataStan <- dataStan_withReplicate
  dataStan$replicate_conc = NULL
  dataStan$replicate_Nsurv = NULL
  dataStan$Ninit = NULL
  
  if(model_type == "SD"){
    model_object <- stanmodels$ode_TKTD_varSD
  } else if(model_type == "IT"){
    model_object <- stanmodels$ode_TKTD_varIT
  } else stop("'model_type' must be 'SD' or 'IT'.")
  
  fit <- rstan::sampling(
    object = model_object,
    data = dataStan,
    ...)
  
  ls_out <- list(stanfit = fit,
                 data = data,
                 dataStan = dataStan_withReplicate,
                 model_type = model_type)
  
  class(ls_out) <- "survFitODE"
  
  return(ls_out)
}