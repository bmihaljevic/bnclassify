## Test environments
- ubuntu 22.04, R 4.3.3 (local) 
- macos-latest,   r: release
- windows-latest, r: release
- ubuntu-latest,   r: devel
- ubuntu-latest,   r: release
- ubuntu-latest,   r: oldrel-1
- windows R-devel (win-builder) 
- windows R-release (win-builder)  


## R CMD check results   
0 errors | 0 warnings | 1 notes

The not is related to the fact that this is a submission of an archived package. The words identified as possibly misspelled are last names and are not misspelled. The note is:

  Checking CRAN incoming feasibility ... [7s/18s] NOTE
    Maintainer: ‘Mihaljevic Bojan <boki.mihaljevic@gmail.com>’
    
    New submission
    
    Package was archived on CRAN
    
    Possibly misspelled words in DESCRIPTION:
      Bielza (3:145)
      Larranaga (3:154)
    
    CRAN repository db overrides:
      X-CRAN-Comment: Archived on 2023-10-20 as check problems were not
        corrected in time.

On Windows R-devel (win-builder), there was 1 WARNING and 2 NOTES. The WARNING was:

* checking sizes of PDF files under 'inst/doc' ... WARNING
  'gs+qpdf' made some significant size reductions:
     compacted 'methods.pdf' from 326Kb to 72Kb
     compacted 'overview.pdf' from 495Kb to 184Kb
     
## Reverse dependencies
There are currently no downstream dependencies for this package.