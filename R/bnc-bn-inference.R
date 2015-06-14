# x is a bnc_dag
compute_cp <- function(x, dataset) {
  p <- compute_ulcp(x, dataset)
  p <- log_normalize(p)
  stopifnot(are_pdists(p))
  p
}
compute_ll <- function(x, dataset) {
  stop("Not implemented.")
}
# Compute unnormalized log class posterior
compute_ulcp <- function(x, dataset) {
  if (!anyNA(dataset)) {
    compute_ulcp_complete(x, dataset)
  }
  else { 
    ind_complete <- complete.cases(dataset)
    # TODO: this might be better with a data.table. Yet, grain is slow anyway so little to be gained relatively.
    p_complete <- compute_ulcp_complete(x, dataset[ind_complete, , drop=F])
    p_incomplete <- compute_ulcp_incomplete(x, dataset[!ind_complete, , drop=F])
    # put the two together
    p <- matrix(numeric(), nrow=nrow(dataset), ncol=ncol(p_incomplete), 
                   dimnames=list(NULL, dimnames(p_incomplete)[[2]]))
    p[ind_complete, ] <- p_complete
    p[!ind_complete, ] <- p_incomplete
    p
  }
}
compute_ulcp_complete <- function(x, dataset) {
  # Check dataset complete
  stopifnot(!anyNA(dataset))  
  compute_augnb_luccpx(x, dataset)
}
compute_ulcp_incomplete <- function(x, dataset) {
  # Check all rows in dataset have missings 
  stopifnot(sum(complete.cases(dataset)) == 0)
  # Check x is a bnc_bn
  check_bnc_bn(x)
  # Get gRain from bnc_bn 
  grain <- to_grain(x)
  # Get class var
  class <- bnc_class(x)
  # Call grain class posterior
  compute_grain_luccpx(grain, dataset, class)
}