## Test environments 
- win-builder (devel, release)
- local Windows (10), R 3.4.3
- travis-ci.org (R-oldrel, R-release, R-devel)

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ryan Andrew Peterson <ryan-peterson@uiowa.edu>'

Possibly mis-spelled words in DESCRIPTION:
  ORQ (12:29, 12:35)

ORQ is the acronym for the ordered quantile normalization procedure.

** running examples for arch 'x64' ... [4s] NOTE
Examples with CPU or elapsed time > 10s
              user system elapsed
bestNormalize  9.6   0.82    3.09

The new version of the package performs some computationally intensive cross-
validation. The example is relatively minimal and should not take much longer 
than it did in this case, but is necessary to show how the process works.
The note may not show up if the runtime is quick enough.

## Downstream dependencies
There are currently no downstream dependencies for this package

