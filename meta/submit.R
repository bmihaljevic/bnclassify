## ========================================= ## 
## Submit                                    ## 
## ========================================= ## 

# must build vignettes first
devtools::build_vignettes()
# Must use the build args for the check to pass on Windows  
build_args <- c('--resave-data', '--compact-vignettes="gs+qpdf"')
devtools::build(args = build_args )

check_args <-  '--as-cran'
# check_version = TRUE # seems this argument of devtools::check has been removed
devtools::check(args = check_args , cran = TRUE,  build_args = build_args ) 

# Full, slow release with questions
# devtools::release(args = build_args) )
# Quick submission
devtools::submit_cran(args = build_args)