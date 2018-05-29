## Test environments 
- win-builder (devel, release)
- local Windows (10), R 3.4.3
- travis-ci.org Ubuntu (R-oldrel, R-release, R-devel)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs

## Downstream dependencies
There are currently no downstream dependencies for this package

## Other notes

This release attempts to speed up the CRAN checks in response
to a comment from Professor Brian Ripley, noting that for 
Solaris and Fedora checks, the CPU time was greater than the
elapsed time (and it was taking a while). 