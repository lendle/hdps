# well I tried testing this, but seems to not work because of the way SL searches for library functions and scoping. 
# you can run the code inside the test_that block, and it works, but i'm commenting out for now because it won't run inside
# that block.

# context("SL.hdps")
# 
# test_that("SL.hdps.generator works with glmnet and cv.glmnet", {
#   set.seed(123)
#   n <- 50
#   p <- 500
#   out <- rbinom(n, 1, 0.2)
#   trt <- rbinom(n, 1, 0.5)
#   covars <- matrix(rbinom(n*p, 3, 0.05), n)
#   colnames(covars) <- c(paste("drug", 1:(p/2), sep="_"),
#                         paste("proc", 1:(p/2), sep="_"))
#   
#   dimension_names <- c("drug", "proc")
#   
# 
#   SL.library <- c("SL.hdps.50", "SL.hdps.50.cv")
#   
#   library(SuperLearner)
# 
#   slfit <- SuperLearner(trt, X = data.frame(OUTCOME=out, covars), family=binomial, SL.library=c("SL.hdps.50", "SL.hdps.50.cv"), cvControl = list(V=3))
#   
#   expect_is(slfit, "SuperLearner")
#   expect_is(slfit$fitLibrary$SL.hdps.50_All$glmnet_fit, "glmnet")
#   expect_is(slfit$fitLibrary$SL.hdps.50.cv_All$glmnet_fit, "cv.glmnet")
# })
