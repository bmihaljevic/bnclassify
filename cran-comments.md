## Test environments
- Ubuntu 22.04 R 4.3.3 (local) 
- windows R-devel (win-builder) 
- windows R-release (win-builder)  
- Github actions: ...


## R CMD check results   
Locally and on win-builder there were no ERRORs, no WARNINGs, and no NOTEs.        

0 errors | 0 warnings | 0 notes

On Windows R-devel (win-builder), there was 1 WARNING and 2 NOTES. The WARNING was:


* checking sizes of PDF files under 'inst/doc' ... WARNING
  'gs+qpdf' made some significant size reductions:
     compacted 'methods.pdf' from 326Kb to 72Kb
     compacted 'overview.pdf' from 495Kb to 184Kb
     
Note that this warning does not appear locally, nor when build with Github actions.

On R hub,   	Windows Server 2022, R-devel, 64 bit we get a single warning:  incoming feasibility. 
New submission

Package was archived on CRAN

## Reverse dependencies
There are currently no downstream dependencies for this package.