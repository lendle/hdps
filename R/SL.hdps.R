##' Generates a wrapper for SuperLearner using HDPS
##' 
##' A HDPS candidate will generate covariates using \code{hdps_screen} from
##' codes, and estimate the propensity score with logistic regression on
##' generated covariates and predefined covariates.
##' 
##' To use HDPS in SuperLearner to estimate a propensity score, you need to
##' include the outcome variable as a covariate where here outcome means the
##' outcome of interest in the causal problem as opposed to the \code{Y}
##' variable in SuperLearner. For non-HDPS candidates in SuperLearner, it's
##' important to exclude the outcome variable via \code{\link[=screen]{screen.named}} or
##' some other screening algorithm in order to avoid adjusting for something
##' downstream on the causal pathway.
##' @title SL.hdps.generator
##' @param out_name Name of the outcome variable.
##' @param dimension_names Dimension names of HDPS dimensions. See
##'   \code{\link{hdps_screen}}.
##' @param predef_covar_names Names of predefined covariates to be included in
##'   logistic regression model.
##' @param keep_k_total See \code{\link{hdps_screen}}.
##' @param ... Other arguments passed to \code{\link{hdps_screen}}.
##' @return A SuperLearner wrapper function
##' @author Sam Lendle
##' @importFrom Matrix sparse.model.matrix 
##' @importFrom glmnet glmnet
##' @export
SL.hdps.generator <- function(out_name, dimension_names, predef_covar_names=c(), keep_k_total, ...) {
  function(Y, X, newX, family, obsWeights, id) {
    if (missing(newX)) {
      newX <- X
    }
    if(family$family == 'gaussian') {
      stop("SL.hdps only for binomial")
    }
    
    hdps_fit <- hdps_screen(X[, out_name], Y, X, dimension_names, keep_k_total=keep_k_total, ...)
    
    predef_covars <- X[, predef_covar_names]
    if (keep_k_total > 0) {
      hdps_covars <- predict(hdps_fit)
      hdps_keep <- colnames(hdps_covars)[abs(cor(Y, hdps_covars)) <= 0.95]
      hdps_covars <- hdps_covars[, hdps_keep]
      df = as.data.frame(cbind(predef_covars, hdps_covars))
    } else {
      hdps_keep <- NULL
      df = as.data.frame(predef_covars)
    }
    
    smm <- sparse.model.matrix(~.-1, df)
    
    glmnet_fit <- glmnet(smm, Y, family="binomial", lambda=0)
    
    if (identical(X, newX)) {
      smmnew <- smm
    } else {
    
      new_predef_covars <- newX[, predef_covar_names]
      if (keep_k_total > 0) {
        new_hdps_covars <- predict(hdps_fit, newdata=newX)
        new_hdps_covars <- new_hdps_covars[, hdps_keep]
        new_df = as.data.frame(cbind(new_predef_covars, new_hdps_covars))
      } else {
        p
        new_df = as.data.frame(new_predef_covars)
      }
      
      smmnew <- sparse.model.matrix(~.-1, new_df)
    }
    
    pred <- predict(glmnet_fit, smmnew, type="response")
    
    
    # fit returns all objects needed for predict.SL.template
    fit <- list(glmnet_fit = glmnet_fit, hdps_fit = hdps_fit, 
                predef_covar_names=predef_covar_names, hdps_keep=hdps_keep, keep_k_total=keep_k_total)
    # declare class of fit for predict.SL.template
    class(fit) <- 'SL_hdps'
    # return a list with pred and fit
    out <- list(pred = pred, fit = fit)
    return(out)
  }
}

predict.SL_hdps <- function(object, newdata, ...){
  new_predef_covars <- newdata[, object$predef_covar_names]
  new_hdps_covars <- predict(object$hdps_fit, newdata=newdata, keep_k_total=object$keep_k_total)
  new_hdps_covars <- new_hdps_covars[, object$hdps_keep]
  new_df <- cbind(new_predef_covars, new_hdps_covars)
  smmnew <- sparse.model.matrix(~.-1, new_df)
  pred <- predict(object$glmnet_fit, smmnew, type = "response")
  pred
}

##' @name screen
##' @rdname screen
##' 
##' @title SuperLearner screening wrappers
##'   
##' @param names Names to be included or excluded
##' 
##' These functions generate simple screening wrappers for SuperLearner to 
##' include or exclude variables based on \code{names}. This is is helpful 
##' because in order to use HDPS as a candidate in SuperLearner, you need to 
##' include the study outcome variable as a covariate. But to use a non-HDPS 
##' algorithm, (say a random forest on some specified set of covariates,) as a
##' candidate as well, you want to make sure you're not adjusting for the 
##' outcome which is downstream from treatment on the causal pathway.
##' 
##' See documentation for the SuperLearner package for more about screening algorithms.
##'   
##' @examples
##' 
##' screen.predefined <- screen.names(c("names", "of", "predefined",
##' "covariates", "that", "definitely", "dont", "include", "the", "outcome"))
##' 
##' screen.notoutcome <- screen.excludenames(c("outcome_variable_name",
##' "and", "other", "covariates", "to", "exclude"))
##' 
NULL

##' @rdname screen
##' @export
screen.names <- function (names) {
  function (Y, X, family, obsWeights, id, ...) {
    colnames(X) %in% names
  }
}

##' @rdname screen
##' @export
screen.excludenames <- function (names) {
  function (Y, X, family, obsWeights, id, ...) {
    !(colnames(X) %in% names)
  }
}
