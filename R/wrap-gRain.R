# Accessors 
#' To grain
#' @export
to_grain <- function(x) {
  stopifnot(inherits(x, "bnc_bn"))  
  if (is.null(x$.grain))  {
    compile_grain(params(x))  
  }
  else {
    x$.grain  
  }  
}
compute_grain_luccpx <- function(grain, dataset, class) {
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
    dataset <- dataset[, setdiff(colnames(dataset), class)]
  }
  # TODO: Faster to convert to character matrix before apply? 
  # TODO: Do I make a copy of gRain or retract evidence each time? Its probably
  # faster to retract evidence. 
  # For each instance, get gRain class_posterior
  cp <- t(apply(dataset, 1, compute_grain_uccpx_instance, grain, class))
  # Remove any rownames if they may have left over from dataset
  rownames(cp) <- NULL
  log(cp)
}
# incomplete instance Unnormalized class-posterior
compute_grain_uccpx_instance <- function(instance, grain, class) {
  # Instance is character
  stopifnot(is.character(instance))    
  # Get non-NA nodes in instance 
  instance <- instance[!is.na(instance)]
  vars <- names(instance)
  # Check class is character and not in instance names 
  check_class(class) 
  stopifnot(!(class %in% vars))
  # Set them as evidence if they are more than 0
  if (length(vars) > 0) {  
	grain <- gRain::setEvidence(grain, nodes = vars, states = instance)
  }
  # Compute unnormalized class posterior probability 
  gRain::querygrain.grain(grain, nodes=class, normalize=FALSE)[[class]]  
}
# 
# Set, if I retract evidence, the question is whether it is making copies or not. Might be faster by making copies.
# Compiles a grain from a a list of CPTs
compile_grain <- function(cpts) {
  if (!requireNamespace("gRain", quietly = TRUE)) {
    stop("gRain needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("gRbase", quietly = TRUE)) {
    stop("gRain needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Check cpts is a list 
  stopifnot(is.list(cpts))
  # TODO: Check each cpt. 
  # Convert each cpt to parray
  pcpts <- lapply(cpts, gRbase::as.parray, normalize = "none", smooth = 0)
  # Check the probabilities are left unchanged 
  all_eq <- mapply(equivalent_num, cpts, pcpts, SIMPLIFY = TRUE)
  stopifnot(vapply(all_eq, isTRUE, FUN.VALUE = logical(1)))
  # Assemble the grain
  grain <- gRain::grain.CPTspec(gRain::compileCPT(pcpts))
  # Compile and return it 
  gRain::compile.CPTgrain(gRain::compile.CPTgrain(grain))
}
is_grain_compiled <- function(g) {
  if (!requireNamespace("gRain", quietly = TRUE)) {
    stop("gRain needed for this function to work. Please install it.",
         call. = FALSE)
  }
  is(g, "grain") && g$isCompiled
}