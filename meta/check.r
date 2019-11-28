## ========================================= ## 
## Run checks prior to submitting            ##
## ========================================= ## 

## I am not currently using AppVeyor, because there are some issues with Bioconductor

## for vignette size warning. Since vignettes are only built locally, I only need add this argument
## MUST use this in all my builds and checks!!
# build_args <- c('--resave-data','--compact-vignettes=gs+qpdf', '--gs_quality=ebook')
build_args <- c('--resave-data','--compact-vignettes=both')
# check_args <-  '--as-cran --use-valgrind'
check_args <-  '--as-cran'

## First to build win so I can proceed with local while it is tested remotely 
devtools::check_win_devel('.', args = build_args ) 
devtools::check_win_release('.', args = build_args ) 
# devtools::build_win('.', version = 'R-release', args = build_args ) 
# devtools::build_win('.', version = 'R-devel', args = build_args )  

devtools::check(args = check_args , cran = TRUE, build_args = build_args )
## cran = FALSE probably runs tests skipped on cran
devtools::check(cran = FALSE, args = check_args, build_args = build_args )
devtools::check(cran = TRUE, build_args = build_args )    
devtools::check(cran = FALSE, build_args = build_args ) 


# see submit.r