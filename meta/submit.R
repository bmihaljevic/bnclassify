## ========================================= ## 
## Submit                                    ## 
## ========================================= ## 

# Must use the build args for the check to pass on Windows  
build_args <- c('--resave-data','--compact-vignettes="gs+qpdf"')
devtools::build(args = build_args )  

# Submit with this, but never uncomment it 
# devtools::release(args = build_args) )
# devtools::submit_cran(args = build_args) 