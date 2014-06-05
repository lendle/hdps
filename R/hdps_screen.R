#' The \code{hdps_screen} function performs part of step 2 (\code{\link{identify_covariates}}), 
#' steps 3 (\code{\link{assess_recurrence}}) and 4 (\code{\link{prioritize_covariates}}),
#' and part of step 5 of the HDPS algorithm (Schneeweiss et al., 2009).
#'
#' The \code{hdps_screen} function performs part of step 2 (\code{\link{identify_covariates}}), 
#' steps 3 (\code{\link{assess_recurrence}}) and 4 (\code{\link{prioritize_covariates}}),
#' and part of step 5 of the HDPS algorithm (Schneeweiss et al., 2009).
#' 
#' \emph{Step 2.} Columns of \code{covars} are split by data dimension (as defined in Schneeweiss et al. (2009)) and
#' filtered by \code{\link{identify_covariates}}.
#'
#' Dimensions can be specified in two ways.
#' If \code{dimension_names} is used, the \code{colnames(covars)} is \code{\link[base]{grep}}ed for each value of 
#' \code{dimension_names}.
#' If some column names match more than one pattern, an error is thrown.
#' If some column names are not matched by any pattern, a warning is issued and those columns are ignored.
#' For example, suppose the column names of \code{covars} are \code{c("drug_1", "drug_2", "proc_1", "proc_2")}.
#' \code{dimension_names <- c("drug", "proc")} would split \code{covars} into two dimensions,
#' one for \code{drug}s and one for \code{proc}s.
#' 
#' Dimensions can also be specified by \code{dimension_indexes} which should contain a list of either column
#' indexes or column names for each dimension.
#' 
#' If neither \code{dimension_names} nor \code{dimension_indexes} is specified, all covariates are treated as one dimension.
#' 
#' \emph{Step 3.} After filtering, remaining covariates are expanded by \code{\link{assess_recurrence}}.
#' 
#' If at this point, the number of expanded covariates is less than \code{keep_k_total}, all expanded covariates are returned.
#' 
#' \emph{Step 4.} Expanded covariates are ordered with \code{\link{prioritize_covariates}}.
#' 
#' \emph{Step 5.} A matrix of the top \code{keep_k_total} expanded covariates is returned. 
#' 
#' @title hdps_screen
#' @param outcome binary vector of outcomes
#' @param treatment binary vector of treatments
#' @param covars \code{matrix} or \code{data.frame} of binary covariates. 
#' @param dimension_names A character vector of patterns to match against the column names of \code{covars} to split columns into dimension groups. See details.
#' @param dimension_indexes A list of vectors of column indexes corresponding to dimension groups. See details. Cannot be specified with \code{dimension_names}.
#' @param keep_n_per_dimension The maximum number of covariates to be kept per dimension by \code{\link{identify_covariates}}.
#' @param keep_k_total Total number of covariates to keep after expanding by \code{\link{assess_recurrence}} and ordering by \code{link{prioritize_covariates}}.
#' @return Matrix of filtered and expanded covariates. 
#' @references Schneeweiss, S., Rassen, J. A., Glynn, R. J., Avorn, J., Mogun,
#' H., & Brookhart, M. A. (2009). High-dimensional propensity score adjustment
#' in studies of treatment effects using health care claims data. \emph{Epidemiology
#' (Cambridge, Mass.)}, 20(4), 512.
#' @author Sam Lendle
#' @export
hdps_screen <- function(outcome, treatment, covars,
                        dimension_names=NULL, dimension_indexes=NULL,
                        keep_n_per_dimension=200, keep_k_total=500) {
  #outcome and treatment are binary vectors of length n
  #covars is a matrix of binary variabesl of size n x p, 
  # where p is the number of covariates for in a paritular data dimension
  #keep: number of covariates to keep
  # indexes: return indexes or subest of covars
  #
  #returns indexes of kept covariates or matrix of kept covars
  #indexes are sorted by abs(log(multiplicitive bias))
  #if keep is 0 or greatner than the number of covars for which multiplicitive bias
  # could be calculated, all indexes are returned(sorted)
  # matrix of kept covars are in the original column order
  
  #p_c1 - prev of each covar among exposed
  #p_c0 - '' among unexposed
  #rr_cd - "independent association between the [covar] and study outcome"
  
  if (!is.null(dimension_names) && !is.null(dimension_indexes)) {
    stop("At most, one of dimension_names and dimension_indexes should be specified")
  }
  
  if (!is.null(dimension_names)) {
    dimension_indexes <- lapply(dimension_names, grep, x = colnames(covars))
    all_idx <- do.call(c, dimension_names)
    if (anyDuplicated(all_idx)) {
      stop("Some column names of covars are matched by more than one pattern in dimension_names")
    }
    if (!all(all_idx %in% 1:ncol(covars))) {
      warning("Some column names of covars are not matched by any of the patterns in dimension_names")
    } 
  }
  
  # Step 2. Identify empirical candidate covariates
  if (is.null(dimension_indexes)) {
    filtered_covars <- identify_covariates(covars, keep_n_covars=keep_n_per_dimension, indexes=FALSE)
  } else {
    filtered_covars <- lapply(dimension_indexes, identify_covariates, covars,
                                  keep_n_covars=keep_n_per_dimension, indexes=FALSE)
    filtered_covars <- do.call(rbind, filtered_covars)
  }
  
  #Step 3. Assess recurrence
  expanded_covars <- assess_recurrence(filtered_covars)
  
  if (ncol(expanded_covars) <= keep_k_total) {
    return(expanded_covars)
  }
  
  #Step 4. Prioritize covariates
  ordered_indexes <- prioritize_covariates(expanded_covars, keep_NaNs=TRUE)
  
  #(Part of) Step 5. Select covariates
  selected_indexes <- ordered_indexes[1:min(keep_k_total, length(ordered_indexes))]
  selected_covars <- expanded_covars[, sort(selected_indexes)]
  
  return(selected_covars)
}
  
  
