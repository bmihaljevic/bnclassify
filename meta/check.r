# Run checks prior to submitting 

# First to build win so I can proceed with local while it is tested remotely 
devtools::build_win('.', version = 'R-release')
devtools::build_win('.', version = 'R-devel')

devtools::check(args = '--as-cran', cran = TRUE, check_version = TRUE, build_args = c('--resave-data','--compact-vignettes="gs+qpdf"'))

# cran = FALSE probably runs tests skipped on cran
devtools::check(args = '--as-cran', cran = FALSE, check_version = TRUE)
devtools::check(cran = FALSE, check_version = TRUE)

#devtools::check(args = '--as-cran', cran = TRUE, check_version = TRUE)
devtools::check(cran = TRUE, check_version = TRUE )    