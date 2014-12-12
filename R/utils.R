
get_quantiles <- function(x) {
  quants <- quantile(x[x>0], probs=c(0.5, 0.75), names=FALSE)
  quants <- list(list(q="_sporadic", count=quants[1]),
                 list(q="_frequent", count=quants[2]))
  if (quants[[1]]$count==quants[[2]]$count) quants <- quants[1]
  
  quants <- Filter(function(q) q$count != 1, quants)
  quants <- c(list(list(q="_once", count=1)), quants)
  quants
}

column_recurrence <- function(x, quants) {
  mat <- matrix(0, length(x), length(quants))    
  colnames(mat) <- sapply(quants, `[[`, "q")
  
  mat[, 1] <- x > 0
  if (length(quants) > 0) {
    for (i in 1:length(quants)) {
      mat[, i] <- x >= quants[[i]]$count
    }
  }
  
  dups <- duplicated(mat, MARGIN=2)
  if (any(dups)) {
    list(mat=mat[, !dups], quants=quants[dups])
  } else {
    list(mat=mat, quants=quants)
  }
}

calc_rr_cd <- function(outcome, covar) {
  #returns rr_cd if rr_cd > 1, else returns 1/rr_cd
  #might return Inf if there are 0 outcomes at a particular level of the covar
  prevs <- by(outcome, covar, mean)
  rr_cd <- (prevs[1])/(prevs[2])
  max(rr_cd, 1/rr_cd)
}

check_inputs <- function(outcome, treatment, covars, covars_bin=FALSE) {
  n = nrow(covars)
  
  if (!length(outcome) == n || !length(treatment) == n)
    stop("outcome and treatment should be the same length, which should be equal to nrow(covars)")
  
  if (!all(outcome %in% c(0,1)))
    stop("outcome should be binary")
  if (!all(treatment %in% c(0,1)))
    stop("treatment should be binary")
  if (covars_bin && !all(covars %in% c(0,1)))
    stop("covars should be binary")
}
