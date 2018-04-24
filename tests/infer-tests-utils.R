tinfer_consistent <- function(t, dbor) {
  outp <- compute_joint(t, dbor)  
  old <- bnclassify:::compute_anb_log_joint_per_class(t, dbor)
  stopifnot(all.equal(old, outp))  
  wrapped <- bnclassify:::compute_log_joint(t, dbor)
  stopifnot(all.equal(wrapped, outp))   
} 
tinfer_benchmark <- function(t, dbor) {
  f <- features(t)
  cpt <- t$.params$bkblk
  cvar <- class_var(t)       
  
  # 1.152770
  # 1029.943 
  microbenchmark::microbenchmark( { f = compute_joint(t, dbor)},
                                    { h  = compute_log_joint(t, dbor)},
                                  { g = bnclassify:::compute_anb_log_joint_per_class(t, dbor)} ,
                                  times = 1e3 )    
}  