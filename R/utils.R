get_quantiles <- function(x) {
  xx0 <- x[x>0]
  quants <- quantile(xx0, probs=c(0.5, 0.75), names=FALSE, type=2)
  quants <- list(list(q="_once", count=1),
                 list(q="_sporadic", count=quants[1]),
                 list(q="_frequent", count=quants[2]))
  
  counts <- sapply(quants, `[[`, "count")
  ux <- unique(xx0)
  cutoffs <- sapply(counts, function(count) min(ux[ux >= count]))  
  dups <- duplicated(cutoffs)
  quants[!dups]
}

column_recurrence <- function(x, quants, warndup=FALSE) {
  mat <- matrix(0, length(x), length(quants))    
  colnames(mat) <- sapply(quants, `[[`, "q")
  
  for (i in 1:length(quants)) {
    mat[, i] <- x >= quants[[i]]$count
  }
  
  if (warndup) {
    dups <- duplicated(mat, MARGIN=2)
    if (any(dups)) {
      warning("Duplicate columns in mat. This should not happen when hdps_screen is called, but could when predict is called.")
    }
  }
  
  
  list(mat=mat, quants=quants)
}

check_inputs <- function(outcome, treatment, covars, covars_bin=FALSE) {
  n = nrow(covars)
  
  if(!is.vector(outcome)) stop("outcome should be a vector")
  if(!is.vector(treatment)) stop("treatment should be a vector")
  
  if (!length(outcome) == n || !length(treatment) == n)
    stop("outcome and treatment should be the same length, which should be equal to nrow(covars)")
  
  if (!all(outcome %in% c(0,1)))
    stop("outcome should be binary")
  if (!all(treatment %in% c(0,1)))
    stop("treatment should be binary")
  if (covars_bin && !all(covars %in% c(0,1)))
    stop("covars should be binary")
}
