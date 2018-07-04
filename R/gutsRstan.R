#' Modelling Tools for Survival Data in Ecotoxicology using the
#' General Unified Threshold model for Survival (GUTS)
#'
#' Provides tools for GUTS modelling under a Bayesian framework.
#'
#' The package currently handles survival data. 
#'
#' \strong{Getting started:} the package uses the \code{rstan} package
#' [Stan Development Team, 2018](rstan), an R interface to the Stan library for
#' Bayesian model estimation.
#'
#'
#' @name gutsrstan-package
#' @aliases gutsRstan-package gutsRstan
#' @docType package
#' @author
#' Virgile Baudrot  <virgile.baudrot@@posteo.net>,
#' 
#' Sandrine Charles <sandrine.charles@@univ-lyon1.fr>
#' 
#' 
#' Maintainer: Virgile Baudrot  <virgile.baudrot@@posteo.net>,
#' 
NULL

#' Survival datasets for \emph{Gammarus pulex} exposed to
#' time-varying concentration of Diazinon during 21 days.
#'
#' Three profiles of concentration were tested. Each replicate contained 70
#' organisms. Survival was monitored at different time points.
#'
#' @name data_Diazinon
#' @docType data
#' @usage data(data_Diazinon)
#' @format A data frame with 88 observations of the following five variables:
#' \describe{
#' \item{\code{replicate}}{A vector of class \code{factor}.}
#' \item{\code{conc}}{A vector of
#' class \code{numeric} with the Diazinon concentrations.}
#' \item{\code{time}}{A vector of class \code{numeric} with the time points
#' (in days from the beginning of the experiment \eqn{t = 0}).}
#' \item{\code{Nsurv}}{A vector of class \code{integer} with the number of
#' alive individuals.}
#' }
#' @references Ashauer, R., Hintermeister, A., Caravatti, I., Kretschmann, A.,
#'  and Escher, B.I. (2010) Toxicokinetic and toxicodynamic modeling explains
#'  carry-over toxicity from exposure to diazinon by slow organism recovery,
#'  \emph{Environmental Science & Technology}, 10, 3963-3971.
#' @keywords dataset
NULL