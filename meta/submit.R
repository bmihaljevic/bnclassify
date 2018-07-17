## ========================================= ## 
## Submit                                    ## 
## ========================================= ## 

# must build vignettes first
devtools::build_vignettes()
# Must use the build args for the check to pass on Windows  
build_args <- c('--resave-data', '--no-build-vignettes', '--compact-vignettes="gs+qpdf"')
devtools::build(args = build_args )  


# Full, slow release with questions
# devtools::release(args = build_args) )
# Quick submission
devtools::submit_cran(args = build_args)