#' percentage overlap for posteriors
#' 
#' 
#'    
#' @export
#' 
#' 
ppc_percent_overlap <- function(y, yrep, prob = 0.95){
  
  prob_min <- (1-prob)/2
  prob_max <- 1-(1-prob)/2
  
  qinf95 = apply(yrep, 2 , quantile, probs = prob_min, na.rm = TRUE)
  qsup95 = apply(yrep, 2 , quantile, probs = prob_max, na.rm = TRUE)
  
  index_matching = ifelse(y > qsup95 | y < qinf95, 0, 1)
  
  percent_overlap <- sum(index_matching) / length(y) * 100
  
  return(percent_overlap)
  
}
