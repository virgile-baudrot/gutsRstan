#' survFitODE
#' 
#' @param data A dataset to fit
#' @param model_type The type of the model: 'SD' or 'IT'
#' 
#' 
#' @export
#' 
survFitODE <- function(data, model_type = NULL, ...){
  
  if(model_type == "SD"){
    model_object <- stanmodels$ode_TKTD_varSD
  } else if(model_type == "IT"){
    model_object <- stanmodels$ode_TKTD_varIT
  } else stop("'model_type' must be 'SD' or 'IT'.")
  
  fit <- rstan::sampling(
    object = model_object,
    data = data,
    ...)
  
  return(fit)
}