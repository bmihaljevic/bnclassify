#' Convert to \code{mlr}.
#' 
#' Convert a \code{\link{bnc_dag_object}} to a \code{\link[mlr]{Learner}}
#' object. 
#'   
#' @inheritParams cv
#' @param x A \code{\link{bnc_bn_object}} object.
#' @param id A character.
#' @export
#' @examples 
#' data(car)
#' nb <- bnc('nb', 'class', car, smooth = 1)
#' nb_mlr <- as_mlr(nb, dag = FALSE, id = "ode_cl_aic") 
#' nb_mlr
as_mlr <- function(x, dag, id = "1") {
  if (!requireNamespace("mlr", quietly = TRUE)) {
    stop("Package mlr required for this functionality.")
  }
  check_bnc_bn(x)
  args <- bnc_get_update_args(x, dag)
  # Call make learner with the arguments
  mlr::makeLearner("bnc", id = id, par.vals = list(args=args))
}
#' makeRLearner. Auxiliary mlr function. 
#' @export
#' @keywords internal
makeRLearner.bnc <- function() {
  if (!requireNamespace("mlr", quietly = TRUE)) {
    stop("Package mlr required for this functionality.")
  }
  mlr::makeRLearnerClassif(
    cl = "bnc",
    package = "bnclassify",
    par.set = ParamHelpers::makeParamSet(
      ParamHelpers::makeUntypedLearnerParam(id = "args", default = NULL)
    ),
    properties = retrieve_bnc_properties()
  )
}
#' trainLearner. Auxiliary mlr function. 
#' @export 
#' @keywords internal
#' @param .learner,.task,.subset,.weights Internal.
#' @param ... Internal.
trainLearner.bnc = function(.learner, .task, .subset, .weights, ...) {
  if (!requireNamespace("mlr", quietly = TRUE)) {
    stop("Package mlr required for this functionality.")
  }
  # Check args contain struct, struct_call and params_call
  args <-  .learner$par.vals$args
  dataset <- mlr::getTaskData(.task, .subset)
  bnc_update(args, dataset)
}
#' predictLearner. Auxiliary mlr function. 
#' @export 
#' @keywords internal
#' @param .learner,.model,.newdata Internal.
#' @param ... Internal.
predictLearner.bnc = function(.learner, .model, .newdata, ...) {
  if (!requireNamespace("mlr", quietly = TRUE)) {
    stop("Package mlr required for this functionality.")
  }
  prob = TRUE
  if(.learner$predict.type == "response") prob = FALSE
  predict(.model$learner.model, newdata = .newdata, prob = prob)
}

retrieve_bnc_properties <- function() {
  c("oneclass", "twoclass", "multiclass", "factors", "prob", "numerics", 
    "missings")
}