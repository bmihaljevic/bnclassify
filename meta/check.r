## ========================================= ## 
## Run checks prior to submitting            ##
## ========================================= ## 

## for vignette size warning. Since vignettes are only built locally, I only need add this argument
## MUST use this in all my builds and checks!!
build_args <- c('--resave-data','--compact-vignettes="gs+qpdf"')

## First to build win so I can proceed with local while it is tested remotely 
devtools::build_win('.', version = 'R-release', args = build_args ) 
devtools::build_win('.', version = 'R-devel', args = build_args )  

devtools::check(args = '--as-cran', cran = TRUE, check_version = TRUE, build_args = build_args )
## cran = FALSE probably runs tests skipped on cran
devtools::check(cran = FALSE, check_version = TRUE, args = '--as-cran', build_args = build_args )
devtools::check(cran = TRUE, check_version = TRUE , build_args = build_args )    
devtools::check(cran = FALSE, check_version = TRUE, build_args = build_args ) 

## ========================================= ## 
## Submit                                    ## 
## ========================================= ## 

# Must use the build args for the check to pass on Windows
# Submit with this, but never uncomment it

# devtools::release(args = build_args) )
# devtools::submit_cran(args = build_args) 