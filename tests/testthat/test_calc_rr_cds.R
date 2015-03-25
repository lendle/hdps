
context("calc_rr_cds")

test_that("calc_rr_cds c++ function works the same way as the all R version", {
  calc_rr_cd <- function(outcome, covar) {
    #returns rr_cd if rr_cd > 1, else returns 1/rr_cd
    #might return Inf if there are 0 outcomes at a particular level of the covar
    prevs <- by(outcome, covar, mean)
    rr_cd <- (prevs[1])/(prevs[2])
    max(rr_cd, 1/rr_cd)
  }
  
  set.seed(123)
  n <- 100
  p <- 1000
  out <- rbinom(n, 1, 0.2)
  covars <- matrix(rbinom(n*p, 1, 0.05), n)
  colnames(covars) <- c(paste("drug", 1:(p/2), sep="_"),
                        paste("proc", 1:(p/2), sep="_"))
  
  #make outcome 0 for all obs with covar 1 = 1, so first RR = Inf
  out[covars[, 1]==1] <- 0 
  
  rr_cds <-  calc_rr_cds(out, covars)
  
  expect_equal(rr_cds, apply(covars, 2, calc_rr_cd, outcome=out))
  expect_equal(rr_cds[1], Inf, check.names=FALSE)
})
