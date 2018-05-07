## Test environments
* Ubuntu 16.04, R 3.4.4 (local)
* win-builder (R-devel and R-release)

## R CMD check results
Locally, there were no ERRORs, no WARNINGs, and no NOTEs.

On win-builder there was one WARNING: 

* checking sizes of PDF files under 'inst/doc' ... WARNING
  'gs+qpdf' made some significant size reductions:
     compacted 'overview.pdf' from 427Kb to 148Kb
  consider running tools::compactPDF(gs_quality = "ebook") on these files 

Since the vignette is generated automatically from vignettes/overview.rmd and I don't know of anything I can do to reduce the size of the resulting pdf. The included images are pngs and among them take up only 32K. I avoided the warning locally by using 
`devtools::check(args = '--as-cran', cran = TRUE, check_version = TRUE, build_args = c('--resave-data','--compact-vignettes="gs+qpdf"'))`.


The zip contents. No reason for overview.html
https://win-builder.r-project.org/E9EX4Jdg1DyF/

## Downstream dependencies
There are currently no downstream dependencies for this package.