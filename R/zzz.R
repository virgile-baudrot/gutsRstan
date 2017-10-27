#' @useDynLib morseStan, .registration = TRUE
#' @import Rcpp
#' @import rstan
#' @import dplyr
#' @importFrom Rcpp evalCpp

.onLoad <- function(libname, pkgname) {
  modules <- paste0("stan_fit4", names(stanmodels), "_mod")
  for (m in modules) loadModule(m, what = TRUE)
}

.onAttach <- function(...) {
  rstanarmLib <- dirname(system.file(package = "morseStan"))
  pkgdesc <- suppressWarnings(utils::packageDescription("morseStan", lib.loc = rstanarmLib))
  if (length(pkgdesc) > 1) {
    builddate <- gsub(';.*$', '', pkgdesc$Packaged)
    packageStartupMessage(paste("morseStan (Version ", pkgdesc$Version, ", packaged: ", builddate, ")", sep = ""))
  }
  packageStartupMessage("- For execution on a local, multicore CPU with excess RAM we recommend calling")
  packageStartupMessage("options(mc.cores = parallel::detectCores()-1)")
  packageStartupMessage("- In addition to functions provided by morseStan, we recommend using packages:")
  packageStartupMessage("   - 'bayesplot' for posterior analysis, model checking, and MCMC diagnostics.")
  packageStartupMessage("   - 'loo' for leave-one-out cross-validation (LOO) using Pareto smoothed")
  packageStartupMessage("importance sampling (PSIS), comparison of predictive errors between models and")
  packageStartupMessage("widely applicable information criterion (WAIC).")
}