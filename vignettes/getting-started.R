## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7,
  fig.height = 4,
  cache = TRUE,
  collapse = TRUE,
  comment = "#>"
)

## ----survFitsummary, cache=TRUE------------------------------------------
## Note: to reduce the size of the vignettes, we do not save previous object from 'stanguts_to_survFit()' function, so we recall this function
# ---
summary(stanguts_to_survFit(fit_SD_diaz))
# summary(stanguts_to_survFit(fit_IT_diaz))
# PROPER models are not include in morse version < 3.X.X
# summary(survFit_PROPER_lN_diaz)
# summary(survFit_PROPER_ll_diaz)

## ----survFitplot, cache=TRUE---------------------------------------------
plot(stanguts_to_survFit(fit_SD_diaz))
# plot(stanguts_to_survFit(fit_IT_diaz))

# PROPER models are not include in morse version < 3.X.X
# plot(survFit_PROPER_lN_diaz)
# plot(survFit_PROPER_ll_diaz)

## ---- cache=TRUE---------------------------------------------------------
ppc(stanguts_to_survFit(fit_SD_diaz))
# ppc(stanguts_to_survFit(fit_IT_diaz))
# PROPER models are not include in morse version < 3.X.X
# ppc(survFit_PROPER_lN_diaz)
# ppc(survFit_PROPER_ll_diaz)

