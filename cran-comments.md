## Test environments
* Ubuntu 16.04 3.4.4 (local)
* ubuntu 14.04 3.5.0 (travis ci)
* ubuntu 14.04 R-release (travis ci)
* windows R 3.5.0 (win-builder) 
* windows R-release (win-builder) 
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