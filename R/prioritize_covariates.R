#' Returns the indexes of columns \code{covars} ordered by the possible amount of counfounding each column could adjust for.
#'
#' For each covariate in \code{covars}, the potential multiplicitive bias is calculated as described in Schneeweiss et al. (2009).
#' The column indexes are then put in descending order of the absolute value of the log of the multiplicitive bias.
#' 
#' If \code{return_bias==TRUE}, the returned vector of indexes includes the multiplicitive biases sorted in the same order and stored in an attribute called \code{"bias_m"}.
#' 
#' If \code{outcome} has no variation for a particular value of a covariate, then the multiplicitive bias is calculated as \code{NaN}.
#' If \code{keep_NaNs==FALSE}, then the column indexes of such covariates are not included in the returned vector.
#' 
#' @title prioritize_covariates
#' @param outcome binary vector of outcomes
#' @param treatment binary vector of treatments
#' @param covars \code{matrix} or \code{data.frame} of binary covariates. 
#' @param return_bias Should the calculated multiplitive biases be returned with the ordered indexes? Defaults to \code{FALSE}
#' @param keep_NaNs If the calculated multiplicitive bias for a covariate is \code{NaN}, should its index be included in the returned vector? Defaults to \code{FALSE}
#' @return Vector of column indexes of \code{covars}.
#' @author Sam Lendle
#' @references Schneeweiss, S., Rassen, J. A., Glynn, R. J., Avorn, J., Mogun,
#' H., & Brookhart, M. A. (2009). High-dimensional propensity score adjustment
#' in studies of treatment effects using health care claims data. \emph{Epidemiology
#' (Cambridge, Mass.)}, 20(4), 512.
#' @export
prioritize_covariates <- function(outcome, treatment, covars, return_bias=FALSE, keep_NaNs=FALSE) {
  if (!all(outcome %in% c(0,1)))
    stop("outcome should be binary")
  if (!all(treatment %in% c(0,1)))
    stop("treatment should be binary")
  if (!all(covars %in% c(0,1)))
    stop("covars should be binary")
  
  treatment <- factor(treatment)
  
  covar_prev <- by(covars, treatment, colMeans)
  p_c1 <- covar_prev[[levels(treatment)[1]]]
  p_c0 <- covar_prev[[levels(treatment)[2]]]
  
  #a vector of rr_cd if rr_cd > 1, 1/rr_cd otherwise
  rr_cds <- apply(covars, 2, calc_rr_cd, outcome=outcome)
  
  #Infs in rr_cds will result in NaN for some covariates
  bias_mult <- (p_c1 * (rr_cds - 1) + 1) / (p_c0 * (rr_cds - 1) + 1)
  
  abs_log_bias_mult <- abs(log(bias_mult))
  
  na.last <- if (keep_NaNs) TRUE else NA
  ordered_idxs <- order(abs_log_bias_mult, decreasing=TRUE, na.last=na.last)
  if (return_bias) attr(ordered_idxs, "bias_m") <- bias_mult[ordered_idxs]
  ordered_idxs
}
