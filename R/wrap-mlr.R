#' makeRLearner
#' @export
makeRLearner.bnc <- function() {
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
# #' CV
# #' @export 
# bnc_mlr_cv <- function(x, dataset, folds, struct) {
#   # Check dataset is a dataframe
#   stopifnot(is.data.frame(dataset))
#   # Make learner from x. Get name of class variable.
#   learner <- NULL
#   class <- NULL
#   if (inherits(x, what = "bnc_dag")) {  
#     learner <- as_mlr(x, struct = struct)
#     class <- class_var(x)
#   } else {   
#     stopifnot(is.list(x))
#     # Assign a unique ID to each learner
#     ids <- as.character(seq_along(x))
#     learner <- mapply(as_mlr, x, ids, MoreArgs = list(struct = struct), 
#                       SIMPLIFY = FALSE)
#     # Get it just from first element
#     class <- class_var(x[[1]])
#   }
#   # Make resampling description 
#   rdesc <- mlr::makeResampleDesc(method = "CV", stratify = TRUE, iters = folds)
#   # Make resampling task 
#   t <- mlr::makeClassifTask(id = "compare", data = dataset, 
#                             target = class, fixup.data = "no", 
#                             check.data = FALSE) 
#   # Do resampling
#   b <- mlr::benchmark(learner, task = t, resampling = rdesc, measures = mlr::acc,
#                       show.info = FALSE)
#   mlr::getBMRAggrPerformances(b, as.df = TRUE)[, 'acc.test.mean']
# }