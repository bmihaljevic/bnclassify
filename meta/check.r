## ========================================= ## 
## Run checks prior to submitting            ##
## ========================================= ## 

- gha: not using manual currently, could change it


## for vignette size warning. Since vignettes are only built locally, I only need add this argument
## MUST use this in all my builds and checks!!
# build_args <- c('--resave-data','--compact-vignettes=gs+qpdf', '--gs_quality=ebook')
build_args <- c('--resave-data','--compact-vignettes=both')
# check_args <-  '--as-cran --use-valgrind'
check_args <-  '--as-cran'

## First to build win so I can proceed with local while it is tested remotely 
devtools::check_win_devel('.', args = build_args ) 
devtools::check_win_release('.', args = build_args ) 
devtools::check_mac_release( '.', args = build_args ) 
# devtools::build_win('.', version = 'R-release', args = build_args ) 
# devtools::build_win('.', version = 'R-devel', args = build_args )  

# TODO rhub: can I set build args?
rhub::check_for_cran()
rhub::check_with_sanitizers()

devtools::check(args = check_args , cran = TRUE, build_args = build_args, remote = TRUE, manual = TRUE )
## cran = FALSE probably runs tests skipped on cran
devtools::check(cran = FALSE, args = check_args, build_args = build_args )
devtools::check(cran = TRUE, build_args = build_args )    
devtools::check(cran = FALSE, build_args = build_args ) 


# see submit.r