## Resubmission
This is a resubmission. In this version I have:

* Fixed the following note: 

	  checking R code for possible problems ... NOTE
	    identify_all_testing_depths: no visible binding for global variable
	      'var'
	    Undefined global functions or variables:
	      var
	    Consider adding
	      importFrom("stats", "var")
	    to your NAMESPACE.   

## Test environments
* local windows install, R 3.2.1
* ubuntu 12.04 (on travis-CI), R 3.2.1
* win-builder, R-devel

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Mihaljevic Bojan <bmihaljevic@fi.upm.es>'
  New submission
  
* checking package dependencies ... NOTE
  No repository set, so cyclic dependency check skipped
  
  	This only occurs on win-builder, not locally. I believe it is due to a global option not being set on win-builder.  

## Downstream dependencies
There are currently no downstream dependencies for this package.