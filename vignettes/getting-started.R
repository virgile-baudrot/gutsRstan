## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 4,
  cache = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----fit_PROPER_ln, cache=TRUE, echo=TRUE, eval=FALSE--------------------
#  # (6-PROPER_ln) fit the TK-TD model PROPER with distribution 'lognormal'
#  fit_PROPER_lN_diaz <- stan_guts(data_Diazinon, model_type = "PROPER", distribution = "lognormal")

