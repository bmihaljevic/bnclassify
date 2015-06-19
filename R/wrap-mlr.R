#' makeRLearner
#' @export
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
#' To mlr 
#' @export
as_mlr <- function(x, dag, id = "1") {
  if (!requireNamespace("mlr", quietly = TRUE)) {
    stop("Package mlr required for this functionality.")
  }
  check_bnc_bn(x)
  args <- bnc_get_update_args(x, dag)
  # Call make learner with the arguments
  mlr::makeLearner("bnc", id = id, par.vals = list(args=args))
}
#' Train.
#' @export 
trainLearner.bnc = function(.learner, .task, .subset, .weights, ...) {
  if (!requireNamespace("mlr", quietly = TRUE)) {
    stop("Package mlr required for this functionality.")
  }
  # Check args contain struct, struct_call and params_call
  args <-  .learner$par.vals$args
  dataset <- mlr::getTaskData(.task, .subset)
  bnc_update(args, dataset)
}
#' Predict.
#' @export 
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