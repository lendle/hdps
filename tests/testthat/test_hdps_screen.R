context("hdps_screen")

test_that("works on data.frames", {
  set.seed(123)
  n <- 100
  p <- 1000
  out <- rbinom(n, 1, 0.2)
  trt <- rbinom(n, 1, 0.5)
  covars <- matrix(rbinom(n*p, 3, 0.05), n)
  colnames(covars) <- c(paste("drug", 1:(p/2), sep="_"),
                        paste("proc", 1:(p/2), sep="_"))
  
  dimension_names <- c("drug", "proc")
  
  screened_covars_fit <- hdps_screen(out, trt, as.data.frame(covars), 
                                     dimension_names = dimension_names,
                                     keep_n_per_dimension = 200,
                                     keep_k_total = 100,
                                     verbose=FALSE)
  
  screened_covars <- predict(screened_covars_fit)
  expect_equal(screened_covars, predict(screened_covars_fit, newdata=covars))
})
