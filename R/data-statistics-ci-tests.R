# TODO: Use a single function ci_test(x,y,z,dataset). If z = NULL then
# it is an independence test

#' Chi-squared test of independence for sets of variables
#' 
#' The chi-squared test based on mutual information is used. 
#' 
#' @param x a character vector (a set of columns of \code{dataset})
#' @param y a character vector (a set of columns of \code{dataset} disjoint with x)
#' @param dataset a data frame
#' @param alpha significance level for the test
#' @param stat if \code{stat="x2"} Pearson's X2 statistic is used for the test. Otherwise, 
#' mutual information is used. 
#' @param ai logical. the value to return when the chi-squared approximation is not reliable, 
#' i.e.if \code{ai = T} then the caller assumes independence when the test is unreliable.
#' @return \code{F} if we reject the null hypothesis; \code{T} otherwise. If the test is unreliable 
#' then \code{ai} is returned.
#' @export
#' @examples 
#' data(voting)
#' independence_test('Class','crime',voting)
#' @seealso \code{valid_chisq_approximation} 
independence_test <- function(x, y, dataset, alpha = .05, stat = c("mi","x2"), ai = T) {  
  stat <- match.arg(stat)
  f <- ifelse(identical(stat, "mi"), "chisq_independence_test.mi", "chisq_independence_test.x2")
  p.value <- do.call(f, args=list(x = x, y = y, dataset = dataset))
  # If ai = T the an NA p-value (that of an unreliable tests) is replaced with 1; 0 otherwise   
  if (is.na(p.value)) p.value <- as.numeric(ai)
  p.value > alpha
}
#' Chi-squared independence test based on Pearson's X2 statistic
#' 
#' @details Yates' correction is not used.
#' 
#' @param x a character vector
#' @param y a character vector
#' @param dataset a data frame
#' @export
#' @return the p-value of the test if the chi-squared approximation is reliable; \code{NA}
#' otherwise
#' @seealso \code{valid_chisq_approximation} 
chisq_independence_test.x2 <- function(x, y, dataset) {   
  #   Obtain a 2-dimensional matrix
  ctg_table <- two_dim_ctg_table(x, y, dataset)  
  valid_chisq <- valid_chisq_approximation(ctg_table)
  if (!valid_chisq) {
    #   Return NA to indicate that Ho cannot be rejected    
    return (NA)
  }    
  ch.test <- NULL
  suppressWarnings(ch.test <- chisq.test(ctg_table, correct=F))
  ch.test$p.value  
}
#' Chi-squared independence test based on mutual information (the G2 statistic)
#' 
#' @param x a character 
#' @param y a character 
#' @param dataset a data frame
#' @return the p-value of the test if the chi-squared approximation is reliable. \code{NA}
#' otherwise
#' @export
#' @seealso \code{valid_chisq_approximation} 
chisq_independence_test.mi <- function(x, y, dataset) {
  ctg_table <- table(dataset[,c(x,y)])
  stopifnot(is.matrix(ctg_table))
  valid_chisq <- valid_chisq_approximation(ctg_table)
  if (!valid_chisq) {
    #   Return NA to indicate that the caller should decide whether to reject Ho     
    return (NA)
  }  
  m_info <- mi_freqs(ctg_table)
  x_cardinality <- nrow(ctg_table)
  y_cardinality <- ncol(ctg_table)
  N <- sum(ctg_table)
  pchisq(2 * N * m_info, df = (x_cardinality - 1) * (y_cardinality - 1), lower.tail=F)  
}
#' Check whether the chi-squared approximation is reliable 
#' 
#' The criterion from fast-IAMB is used: the approximation is reliable if the average 
#' cell count is no less than 5
#' @param ctg_table a table
valid_chisq_approximation <- function(ctg_table) { 
  if (sum(ctg_table) == 0) return (F)
  ch.test <- NULL
  suppressWarnings(ch.test <- chisq.test(ctg_table, correct=F))
  mean(ch.test$observed) >= 5
}
#' Check whether Cochrane's conditions for the reliability of the chi-squared approximation 
#' are met 
#' 
#' The criterion is: at least 20% of the EXPECTED counts are 5. This is based on 
#' Cochrane's criterion (check R documentation for independence tests and Agresti). It should
#' also be tested that no count is 0 (Agresti) but this is not done currently (for no particular reason). 
#' 
#' @param ctg_table a table
valid_chisq_approximation.expected <- function(ctg_table) { 
  if (sum(ctg_table) == 0) return (F)
  ch.test <- NULL
  suppressWarnings(ch.test <- chisq.test(ctg_table, correct=F))
  expected_low_counts <-  sum(ch.test$expected < 5)
  expected_low_counts <= ceiling(.2 * length(ctg_table)) 
}
#' Chi-squared tests of conditional independence
#' 
#' Uses I(X;Y|Z) as the statistic 
#' 
#' @param X character vector
#' @param Y character vector
#' @param Z character 
#' @param dataset a data frame
#' @param alpha significance level for the test
#' @param ai logical. the value to return when the chi-squared approximation is not reliable, 
#' i.e.if \code{ai = T} then the caller assumes independence when the test is unreliable.
#' @return \code{F} if we reject the null hypothesis; \code{T} otherwise. If the test is 
#' unreliable then \code{ai} is returned.
#' @export
cond_independence_test <- function(X, Y, Z, dataset, alpha = .05, ai = T) {
  cond_freqs <- table(dataset[,c(X,Y,Z),drop=F])
  if (!valid_chisq_approximation(cond_freqs)) return (NA)  
  p.value <- cond_independence_test_cond_mi(X, Y, Z, dataset)
  if (is.na(p.value)) p.value <- as.numeric(ai)
  p.value > alpha
}
#' Conditional independence test based on conditional mutual information
#' 
#' @param X character vector
#' @param Y character vector
#' @param Z character 
#' @param dataset a data frame
#' @return the p-value of the test     
#' @export
#' @references Koller and Friedman, Probabilistic Graphical Models
cond_independence_test_cond_mi <- function(X, Y, Z, dataset) {
  # I am not sure why I am demanding that Z by a single column   
  stopifnot(length(Z) == 1)  
  cond_freqs <- table(dataset[,c(X,Y,Z),drop=F])
  x_cardinality <- prod(dim(cond_freqs)[1:length(X)])
  y_cardinality <- prod(dim(cond_freqs)[(length(X) + 1):(length(X) + length(Y))])
  z_cardinality <- dim(cond_freqs)[length(X) + length(Y) + 1]
  stopifnot(x_cardinality * y_cardinality * z_cardinality == length(cond_freqs))
  N <- sum(cond_freqs)
  cond_mi <- conditional_mi(X, Y, Z, dataset)
  pchisq(2 * N * cond_mi, df = (x_cardinality - 1) * (y_cardinality - 1) * z_cardinality, 
         lower.tail=F)    
}

ci_test <- function(x, y, z = NULL, dataset, test = "mi", ...) {
  # check data set not missing and it is a data frame   
  stopifnot(!missing(dataset) && is.data.frame(dataset))
  stopifnot(all(c(x,y,z) %in% colnames(dataset)))
  # Check they are disjoint   
  var_sets <- list(x, y, z)
  stopifnot(length(Reduce(intersect, var_sets)) == 0)  
  vars <- c(x, y, z)
  dataset <- dataset[, vars]
  dataset <- na.omit(dataset)   
  if (is.null(z)) 
    bnlearn::ci.test(x = x_vals, y = y_vals, test = test, ...)
  else bnlearn::ci.test(x = x_vals, y = y_vals, z = z_vals, test = test, ...)
}