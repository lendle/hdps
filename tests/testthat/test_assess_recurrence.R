context("assess_recurrence")

test_that("get_quantiles and column_recurrence work when x is binary", {
  x <- c(0,0,0,0,1,1,1)
  mat <- matrix(x, length(x), 1)
  colnames(mat) = c("_once")
  cr <- column_recurrence(x, get_quantiles(x))
  expect_equal(cr$mat, mat)
  expect_equal(colnames(cr$mat), sapply(cr$quants, `[[`, "q"))
})

test_that("get_quantiles and column_recurrence work when 50th and 75th %ile are the same", {
  x <- c(0,0,0,0,1,1,1,3,3,3,3,3)
  mat <- cbind(as.numeric(x>0), as.numeric(x >=3))
  colnames(mat) = c("_once", "_sporadic")
  cr <- column_recurrence(x, get_quantiles(x))
  expect_equal(cr$mat, mat)
  expect_equal(dim(cr$mat)[2], length(cr$quants))
  expect_equal(colnames(cr$mat), sapply(cr$quants, `[[`, "q"))
})

test_that("get_quantiles and column_recurrence work when 50th %ile is 1", {
  x <- c(0,0,0,0,1,1,1,1,1,3,3,3)
  mat <- cbind(as.numeric(x>0), as.numeric(x >=3))
  colnames(mat) = c("_once", "_frequent")
  cr <- column_recurrence(x, get_quantiles(x))
  expect_equal(cr$mat, mat)
  expect_equal(dim(cr$mat)[2], length(cr$quants))
  expect_equal(colnames(cr$mat), sapply(cr$quants, `[[`, "q"))
})

test_that("get_quantiles and column_recurrence work when 50th %ile is 1.5 and 75th is 2", {
  x <- c(0,0,0,0,1,1,1,1,1,2,2,2,2,2)
  mat <- cbind(as.numeric(x>0), as.numeric(x >1))
  colnames(mat) = c("_once", "_sporadic")
  cr <- column_recurrence(x, get_quantiles(x))
  expect_equal(cr$mat, mat)
  expect_equal(dim(cr$mat)[2], length(cr$quants))
  expect_equal(colnames(cr$mat), sapply(cr$quants, `[[`, "q"))
})

test_that("assess_recurrence works", {
  dat <- cbind(a=c(0,0,0,0,1,1,1,3,3,3,3,3),
               b=c(0,0,0,0,1,1,1,1,1,3,3,3))
  mat <- cbind(as.numeric(dat[ ,1]>0), as.numeric(dat[ ,1] >=3),
               as.numeric(dat[ ,2]>0), as.numeric(dat[ ,2] >=3))
  colnames(mat) <- c("a_once", "a_sporadic", "b_once", "b_frequent")
  #quants <- list(a=c("_once"=1, "_sporadic"=3), b=c("_once"=1, "_frequent"=3))
  quants <- list(list(varname="a", q="_once", count=1),
                 list(varname="a", q="_sporadic", count=3),
                 list(varname="b", q="_once", count=1),
                 list(varname="b", q="_frequent", count=3))
  ar <- assess_recurrence(dat)
  expect_equal(ar[["mat"]], mat)
  expect_equal(ar[["quants"]], quants)
})

