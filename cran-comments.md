## Resubmission
This is a resubmission. In this version I have: 

* Fixed the vignette size WARNING on Windows
* Addressed the UBSAN issues reported on CRAN: 
    * https://www.stats.ox.ac.uk/pub/bdr/memtests/clang-UBSAN/bnclassify/
    * https://www.stats.ox.ac.uk/pub/bdr/memtests/gcc-UBSAN/bnclassify/ 

I have tested for clang-UBSAN issues with the rocker/r-devel-ubsan-clang docker image yet this reported no UBSAN issues. I then manually inspected the code and found two instances that should have triggered such warnings, and corrected the corresponding code. I am not, however, certain that I have fixed all possible issues because, as mentioned, checking on my local rocker/r-devel-ubsan-clang missed existing UBSAN issues in the previous version.

## Test environments
* Ubuntu 16.04 R 3.4.4 (local)
* ubuntu 14.04 R R-release (travis ci)
* ubuntu 14.04 R R-devel (travis ci)
* windows R R-devel (win-builder) 
* windows R R-release (win-builder) 
* Windows i386-w64-mingw32/i386, R 3.5.0  (Appveyor)

## R CMD check results   
Locally, on win-builder, and on Appveyor there were no ERRORs, no WARNINGs, and no NOTEs.  
  
On travis ci there were no ERRORs nor WARNINGs. There was one NOTE. 

  checking installed package size ... NOTE
    installed size is  8.4Mb
    sub-directories of 1Mb or more:
      doc    1.0Mb
      libs   6.9Mb
      
This was probably due to a -g compilation flag that I do not currently know how to remove. It does not occur in other environments. 

## Downstream dependencies
There are currently no downstream dependencies for this package.