#' Compute the (conditional) mutual information between two variables.
#' 
#' Computes the (conditional) mutual information between two variables. If 
#' \code{z} is not \code{NULL} then returns the conditional mutual information,
#' \eqn{I(X;Y|Z)}. Otherwise, returns mutual information, \eqn{I(X;Y)}.
#' 
#' \eqn{I(X;Y|Z) = H(X|Z) + H(Y|Z) - H(X,Y,Z) - H(Z)}, where \eqn{H()} is 
#' Shannon's entropy.
#' 
#' @param x A length one character.
#' @param y A length one character.
#' @param dataset A data frame. Must contain x, y and, optionally, z columns.
#' @param z A character vector.
#' @param unit A character. Logarithm base. See \code{entropy} package.
#' @export
#' @examples 
#' data(car)
#' cmi('maint', 'class', car)
cmi <- function(x, y, dataset, z = NULL, unit="log") {  
# Check x,y,z have length 1. z may be NULL
  stopifnot(assertthat::is.string(x), assertthat::is.string(y))
  stopifnot(assertthat::is.string(z) || is.null(z))  
  if (assertthat::is.string(z)) {  # Compute X,Y,Z empirical contigency table
    freqs <- extract_ctgt(c(x,y,z), dataset)  
  }
  else { # Compute X,Y empirical contigency table    
    freqs <- extract_ctgt(c(x, y), dataset)  
  }
# Compute cmi (mi)    
  cmi_table(freqs, unit = unit)
}
#' Returns the conditional mutual information three variables.
#' @keywords internal
cmi_table <- function(xyz_freqs, unit = "log") {
#   Stop if not 2D or 3D
  len_dim <- length(dim(xyz_freqs))  
  stopifnot(len_dim %in% c(2L:3L))
#  If 2D return the mi from entropy     
  if (len_dim == 2L) {
    return (entropy::mi.empirical(xyz_freqs, unit = unit) )
  }
# Compute X,Z empirical marginal contingency table     
  xz_freqs <- margin.table(xyz_freqs, c(1, 3))
# Compute Y,Z empirical marginal contigency table  
  yz_freqs <- margin.table(xyz_freqs, 2:3)
# Compute Z empirical marginal contigency table 
  z_freqs <- margin.table(xyz_freqs, 3)
# Compute X,Z entropy 
  H_xz <- entropy::entropy(xz_freqs, method = "ML", unit = unit, verbose = F)
# Compute Y,Z entropy
  H_yz <- entropy::entropy(yz_freqs, method = "ML", unit = unit, verbose = F)
# Compute X,Y,Z entropy
  H_xyz <- entropy::entropy(xyz_freqs, method = "ML", unit = unit, verbose = F)
# Compute Z entropy
  H_z <- entropy::entropy(z_freqs, method = "ML", unit = unit, verbose = F)
  H_xz + H_yz - H_xyz - H_z    
}
#' Returns a contingency table over the variables. 
#' 
#' Each variable may be a character vector.
#' 
#' Any rows with incomplete observations of the variables are ignored. 
#' @keywords internal
extract_ctgt <- function(cols, dataset) {
  # check cols non empty unique characters 
  # stopifnot(is_non_empty_complete(cols), is.character(cols), 
  #           are_all_unique(cols), is_subset(cols, colnames(dataset)))
  #Trim dataset (do not use trim_dataset() cause it does not enforce cols order)
  # dataset <- dataset[ , cols, drop = FALSE]  
  table_cpp(dataset, cols)
}
# Get the level of each variable 
extract_var_levels <- function(dataset) {
 check_dataset(dataset)
 lapply(dataset, levels)
}
# Compute the degrees of freedom for a I(X;Y | Z) test for singleton X, Y, Z.
cmi_degrees_freedom <- function(freqs_table) {
#    Stop if not numeric table. 
  stopifnot(is.numeric(freqs_table))
#    Stop if not 3D table  
  dm <- dim(freqs_table)
  stopifnot(length(dm) == 3L)
#    Compute degrees of freedom. 
  (dm[1] - 1) * (dm[2] - 1) * dm[3]
}