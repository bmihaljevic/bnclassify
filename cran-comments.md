## Test environments
* Ubuntu 16.04 R 3.6.3 (local)
* ubuntu 14.04 R R-release (travis ci)
* ubuntu 14.04 R R-devel (travis ci)
* windows R R-devel (win-builder) 
* windows R R-release (win-builder) 

## R CMD check results   
Locally and on win-builder there were no ERRORs, no WARNINGs, and no NOTEs.  
  
On travis ci there were no ERRORs nor WARNINGs. There was one NOTE.

  checking installed package size ... NOTE 
    installed size is  7.9Mb 
    sub-directories of 1Mb or more: 
      libs   7.1Mb  
      
This was probably due to a -g compilation flag that I do not currently know how to remove. It does not occur in other environments. 

## Downstream dependencies
There are currently no downstream dependencies for this package.