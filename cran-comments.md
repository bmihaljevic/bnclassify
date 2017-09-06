## Resubmission
This is a re-submission. The CRAN reviewer has asked me to: 
- Provide some references in the 'Description' field of DESCRIPTION file
- Add more small executable examples in Rd-files

I have:
- Added a reference to a survey paper covering most of the implemented algorithms
- All 17 documented function groups now have small executable examples: added 6 examples and removed one function that lacked an example 

## Test environments
* ubuntu 14.04, R-devel and R-release (travis-CI)
* Ubuntu 14.04, R 3.4.1 (local)
* win-builder (R-devel and R-release)

## R CMD check results
There were no ERRORs, WARNINGs. There was one NOTE:   

checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Mihaljevic Bojan <bmihaljevic@fi.upm.es>’

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  Bielza (3:156)
  Larranaga (3:165)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2016-05-02 as check problems were not
    corrected despite reminders.

## Downstream dependencies
There are currently no downstream dependencies for this package.