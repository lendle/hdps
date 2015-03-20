#' Expands covarites to up to three binary columns for at least one, sporadic, or frequent occrence of each covariate.
#'
#' Each column \code{x} of \code{covars} is expanded to three binary columns. 
#' The first column indicates that the value of \code{x} is non-zero. 
#' The second indicates that the value of \code{x} \eqn{\ge} the median of non-zero values of \code{x}.
#' The third indicates that the value of \code{x} \eqn{\ge} the 75th percentile of non-zero values of \code{x}. 
#' Non-unique columns per covariate are dropped.
#' 
#' Groups of columns of the returned matrix are in the same order of columns in \code{covars}. 
#' 
#' If \code{covars} has column names, the returned matrix will have the same column names with suffexes \code{"_once"},
#' \code{"_sporadic"}, and \code{"_frequent"} for the first, second, and third expanded columns, respectively.
#' 
#' @title assess_recurrence
#' @param covars a matrix or something that can be coerced with \code{\link[base]{as.matrix}} of covariates
#' @return Expanded \code{covars} matrix.
#' @author Sam Lendle
#' @references Schneeweiss, S., Rassen, J. A., Glynn, R. J., Avorn, J., Mogun,
#' H., & Brookhart, M. A. (2009). High-dimensional propensity score adjustment
#' in studies of treatment effects using health care claims data. \emph{Epidemiology
#' (Cambridge, Mass.)}, 20(4), 512.
#' @export
assess_recurrence <- function(covars) {
  #expands a matrix by replacing it's columns with as.numeric(x > 0), 
  # as.numeric(x > median(x)), as.numeric(x > quantile(x, prob=0.75))
  #only unique columns (per original column) are kept
  
  covars <- as.matrix(covars)
  
  mats_quants <- lapply(1:ncol(covars), function(i) {
    column <- covars[,i]
    quants <- get_quantiles(column)
    column_recurrence(column, quants, warndup=TRUE)
    })
  
  mats <- lapply(mats_quants, `[[`, "mat")#function(mq) mq[["mat"]])
  quants <- lapply(mats_quants, `[[`, "quants")#function(mq) mq[["quants"]])
  
  cnams <- colnames(covars)  
  if (!is.null(cnams)) {
    for (i in seq_along(mats)) {
      colnames(mats[[i]]) <- paste(cnams[i], colnames(mats[[i]]), sep="")    
      quants[[i]] <- lapply(quants[[i]], function(q) c(varname=cnams[i], q))
    }
  }

  mat <- do.call(cbind, mats)
  quants <- do.call(c, quants)
  list(mat=mat, quants=quants)
}
