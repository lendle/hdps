#' Given a matrix of covarites, \code{identify_covariates} returns the top \code{keep_n_covars} or the indexes of those columns.
#'
#' Columns are sorted in descending order of \code{min(prevalence, 1-prevalence)}  where \code{prevalence} is the the proportion of
#' non-zero values in a given column.
#' 
#' If \code{indexes==TRUE}, a vector of the top \code{keep_n_covars} column indexes is returned.
#' 
#' If \code{indexes==FALSE}, a matrix of covariates is returned whos columns are the top \code{keep_n_covars} colums of
#' \code{covars}. Columns are in their original order.
#' If also \code{keep_n_covars >= ncol(covar)}, then the function returns immediately without ranking columns in terms of
#' prevalence as it is unecessary.
#' 
#' \strong{Differences from Schneeweiss et al. (2009):} 
#' \itemize{
#'   \item{Covariates that have fewer than 100 non-zero values are not automatically dropped. 
#'   If typical covariates tend to have more than 100 non-zero values will typically be ranked higher than those with fewer than 100
#'   automatically.}
#' }
#' 
#' @title identify_covariates
#' @param covars a matrix or something that can be coerced with \code{\link[base]{as.matrix}} of covariates
#' @param keep_n_covars number of covariates to keep
#' @param indexes Should indexes be returned? Or a subset of \code{covars}. Defaults to \code{FALSE}.
#' @return Indexes of identified columns or a subset of \code{covars}
#' @references Schneeweiss, S., Rassen, J. A., Glynn, R. J., Avorn, J., Mogun,
#' H., & Brookhart, M. A. (2009). High-dimensional propensity score adjustment
#' in studies of treatment effects using health care claims data. \emph{Epidemiology
#' (Cambridge, Mass.)}, 20(4), 512.
#' @author Sam Lendle
#' @export
identify_covariates <- function(covars, keep_n_covars=200, indexes=FALSE) {
  
  if (ncol(covars) <= keep_n_covars && !indexes) return(covars)
  
  vars <- colPrevScores(covars>0)
  var_ords <- order(vars, decreasing=TRUE)[1:min(keep_n_covars, ncol(covars))] 
  
  if (indexes) {
    return(var_ords)
  } else {
  covars[, sort(var_ords)]  
  }
}
