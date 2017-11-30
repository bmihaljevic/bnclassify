# Computes MANB local arc C->X posterior probabilities and uses them to compute
# the MANB CTPs. 
# Formula for posterior assumes that smooth = 1. 
#  @param manb_prior Currently ignored.
# @return Returns a vector of probabilities of same length of ctgts. 
compute_manb_arc_posteriors <- function(x, ctgts, smooth, prior = 0.5) {
  if (!is_nb(x)) stop("MANB can only be applied to naive Bayes.")
  stopifnot(smooth > 0)
  stopifnot(identical(features(x), names(ctgts)))
  vapply(ctgts, compute_manb_arc_posterior, smooth = smooth, prior = prior, 
         FUN.VALUE = numeric(1))
}
# Assuming class is the last dimension.
# Assuming that all instances are labelled
# Returns a singe probability. 
compute_manb_arc_posterior <- function(nijk, smooth, prior) {
  #  If not 2D then it is not a naive Bayes
  stopifnot(length(dim(nijk)) == 2, are_probs(prior))
  r <- nrow(nijk)
  rt <- ncol(nijk)
  nij <- colSums(nijk)
  nik <- rowSums(nijk)
  ni <- sum(nijk)
  # P(D | C -> X)
  lpa <- rt * lgamma(r * smooth) - sum(lgamma(r * smooth + nij)) + sum(lgamma(smooth + nijk) - lgamma(smooth)) 
  # P(D | C ... X)
  lpna <- lgamma(r*smooth) - lgamma(r*smooth + ni) + sum(lgamma(smooth + nik) - lgamma(smooth))  
  # P(C -> X)
  denom <- matrixStats::logSumExp(c(lpna, lpa))
  lpa_post <- lpa - denom
  # P(C ... X)  
  lpna_post <- lpna - denom
  # Check valid probs
  stopifnot(all.equal(exp(lpa_post) + exp(lpna_post), 1))
  exp(lpa_post)
  # Consider priors: 
  lpa_prior <- log(prior) 
  lnpa_prior <- log(1 - prior) 
  lpa_num <- lpa + lpa_prior
  lpna_num <- lpna + lnpa_prior
  denom <- matrixStats::logSumExp(c(lpa_num, lpna_num))
  lpa_post <- lpa_num - denom
  lpna_post <- lpna_num - denom
  exp(lpa_post) + exp(lpna_post)
  exp(lpa_post)
}
# @param nijk A contingency table of X and C
compute_manb_cpt <- function(nijk, prob_arc, smooth) {
  stopifnot(length(dim(nijk)) == 2, are_probs(prob_arc), length(prob_arc) == 1)
  lnijk <- log(ctgt2cpt(nijk, smooth = smooth))
  lp_arc <- log(prob_arc)
  arc <- lp_arc +  lnijk
  # no arc
  lni <- rowSums(nijk)
  lni[] <- log(normalize(lni + smooth)) 
  lp_noarc <- log(1 - prob_arc)
  no_arc <- lp_noarc + lni
  rt <- ncol(nijk)
  no_arc <- rep(no_arc, rt)
  entries <- matrix(c(as.numeric(arc), as.numeric(no_arc)), ncol = 2)
  arc[] <- matrixStats::rowLogSumExps(entries)
  stopifnot(identical(dimnames(arc), dimnames(nijk)))
  exp(arc)
}	