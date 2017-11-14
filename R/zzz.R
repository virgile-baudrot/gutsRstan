#' @useDynLib rstanTKTD, .registration = TRUE
#' @import Rcpp
#' @import rstan
#' @import dplyr
#' @importFrom Rcpp evalCpp

.onLoad <- function(libname, pkgname) {
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) loadModule(m, what = TRUE)
}

.onAttach <- function(...) {
  rstanarmLib <- dirname(system.file(package = "rstanTKTD"))
  pkgdesc <- suppressWarnings(utils::packageDescription("rstanTKTD", lib.loc = rstanarmLib))
  if (length(pkgdesc) > 1) {
    builddate <- gsub(';.*$', '', pkgdesc$Packaged)
    packageStartupMessage(paste("rstanTKTD (Version ", pkgdesc$Version, ", packaged: ", builddate, ")", sep = ""))
  }
  packageStartupMessage("- For execution on a local, multicore CPU with excess RAM we recommend calling")
  packageStartupMessage("options(mc.cores = parallel::detectCores()-1)")
  packageStartupMessage("- In addition to functions provided by rstanTKTD, we recommend using packages:")
  packageStartupMessage("   - 'bayesplot' for posterior analysis, model checking, and MCMC diagnostics.")
  packageStartupMessage("   - 'loo' for leave-one-out cross-validation (LOO) using Pareto smoothed")
  packageStartupMessage("importance sampling (PSIS), comparison of predictive errors between models and")
  packageStartupMessage("widely applicable information criterion (WAIC).")
}