#' High-dimensional propensity score algorithm
#' 
#' The high-dimensional propensity score (HDPS) algorithm is a method for
#' high-dimensional proxy adjustment in claims data. This package implements
#' the variable transformation and variable selection parts of the
#' algorithm.
#' 
#' \tabular{ll}{
#' Package: \tab hdps\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1.6\cr
#' Date: \tab 2017-08-16\cr
#' License: \tab MIT \cr 
#' } 
#' 
#' This package implements part of step 2 (\code{\link{identify_covariates}}), 
#' steps 3 (\code{\link{assess_recurrence}}) and 4 (\code{\link{prioritize_covariates}})
#' of the HDPS algorithm (Schneeweiss et al., 2009).
#' 
#' The \code{\link{hdps_screen}} function is a wrapper function for \code{\link{identify_covariates}},
#' \code{\link{assess_recurrence}},  and \code{\link{prioritize_covariates}}.
#' 
#' @name hdps-package
#' @aliases hdps-package hdps
#' @docType package
#' @author Sam Lendle
#' 
#' Maintainer: Sam Lendle <sam.lendle@@gmail.com>
#' 
#' @references Schneeweiss, S., Rassen, J. A., Glynn, R. J., Avorn, J., Mogun,
#' H., & Brookhart, M. A. (2009). High-dimensional propensity score adjustment
#' in studies of treatment effects using health care claims data. \emph{Epidemiology
#' (Cambridge, Mass.)}, 20(4), 512.
#' @keywords package
#' @examples
#' 
#' #~~ simple examples of the most important functions ~~
#' 
#' @useDynLib hdps
#' @importFrom Rcpp evalCpp
NULL



