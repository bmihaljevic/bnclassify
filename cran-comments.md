## Test environments
* local windows install, R 3.2.1
* ubuntu 12.04 (on travis-CI), R 3.2.1
* win-builder, R-devel

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
  
  	This only occurs on win-builder, not locally. I believe it is due to a global option not being set on win-builder.  

## Downstream dependencies
There are currently no downstream dependencies for this package.