# Accessors 
#' @export
#' @describeIn grain_and_graph Convert to a grain.
as_grain <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  if (is.null(x$.grain))  {
    compile_grain(params(x))  
  }
  else {
    x$.grain  
  }  
}
# Computes the log joint probability of the observed features for each of the classes
compute_grain_log_joint <- function(grain, dataset, class) {
  if (!requireNamespace("gRain", quietly = TRUE)) {
    stop("gRain needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Check gRain compiled
  stopifnot(is_grain_compiled(grain))
  # Check data set
  check_dataset(dataset)
  # Check N > 0 
  stopifnot(nrow(dataset) > 0)
  # If class in dataset, remove it 
  if (class %in% colnames(dataset)) {
    dataset <- dataset[, setdiff(colnames(dataset), class), drop = FALSE]
  }
  cp <- t(apply(dataset, 1, compute_grain_log_joint_instance, grain, class))
  # Remove any rownames if they may have left over from dataset
  rownames(cp) <- NULL
  cp
}
# Computes the log joint probability of the observed features for each of the classes
compute_grain_log_joint_instance <- function(instance, grain, class) { 
  stop("gRain currently incompatible")
#   if (!requireNamespace("gRain", quietly = TRUE)) {
#     stop("gRain needed for this function to work. Please install it.",
#          call. = FALSE)
#   }
#   # Instance is character
#   stopifnot(is.character(instance))    
#   # Get non-NA nodes in instance 
#   instance <- instance[!is.na(instance)]
#   vars <- intersect(names(instance), grain_nodes(grain))
#   instance <- instance[vars]
#   # Check class is character and not in instance  
#   check_class(class) 
#   stopifnot(!(class %in% vars))
#   # Set them as evidence if they are more than 0
#   if (length(instance) > 0) {  
# 	  grain <- gRain::setEvidence(grain, nodes = vars, states = instance)
#   }
#   # gRain has a bug and cannot return unnormalized class. Therefore, using the workaround: P(C | x_evidence) * P(x_evidence)
#   cp <- gRain::querygrain.grain(grain, nodes = class, normalize = TRUE)[[class]]  
#   cp <- log(cp)
#   if (length(vars) > 0) {
#     cp <- cp + log(gRain::pEvidence(grain))
#   }
#   cp
}
# Compiles a grain from a a list of CPTs
compile_grain <- function(cpts) {
  if (!requireNamespace("gRain", quietly = TRUE)) {
    stop("gRain needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("gRbase", quietly = TRUE)) {
    stop("gRbase needed for this function to work. Please install it.",
         call. = FALSE)
  }
  stop("gRain currently incompatible")
  # # Check cpts is a list 
  # stopifnot(is.list(cpts))
  # # TODO: Check each cpt. 
  # # Convert each cpt to parray
  # pcpts <- lapply(cpts, gRbase::as.parray, normalize = "none", smooth = 0)
  # # Check the probabilities are left unchanged 
  # all_eq <- mapply(equivalent_num, cpts, pcpts, SIMPLIFY = TRUE)
  # stopifnot(vapply(all_eq, isTRUE, FUN.VALUE = logical(1)))
  # # Assemble the grain    
  # grain <- gRain::grain(gRain::compileCPT(pcpts))
  # grain  
}
is_grain_compiled <- function(g) {
  if (!requireNamespace("gRain", quietly = TRUE)) {
    stop("gRain needed for this function to work. Please install it.",
         call. = FALSE)
  }
  inherits(g, "grain") && g$isCompiled
}
grain_nodes <- function(g) { 
  # gRain::nodeNames(g)
  stop("gRain currently incompatible")
}