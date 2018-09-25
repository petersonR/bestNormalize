## Test environments 
- win-builder (devel, release)
- local Windows (10), R 3.5.1
- travis-ci.org Ubuntu (R-oldrel, R-release, R-devel)
- builder.r-hub.io Linux (Fedora, R-devel) 

## R CMD check results
There were no ERRORs or WARNINGs 

There was one NOTE:
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ryan Andrew Peterson <ryan-peterson@uiowa.edu>'

Days since last update: 1

This update is to fix the issue on Fedora and Solaris of 
the disparity in the CPU/elapsed time. I thought I had 
resolved it in yesterday's update, but the problem lied in 
another package. I was given until June 1, 2018 to solve the issue, 
hence the need to re-update so quickly.

## Downstream dependencies
There are currently no downstream dependencies for this package