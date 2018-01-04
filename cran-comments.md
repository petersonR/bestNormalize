## Test environments 
- win-builder (devel, release)
- local Windows (10), R 3.4.2
- local ubuntu (14.04), R 3.4.2

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 2 NOTEs:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ryan Andrew Peterson <ryan-peterson@uiowa.edu>'

Possibly mis-spelled words in DESCRIPTION:
  WxF (15:5)
  Yeo (14:25)

Yeo is a name and Lambert's WxF is a type of transformation.

** running examples for arch 'x64' ... [6s] NOTE
Examples with CPU or elapsed time > 10s
               user system elapsed
bestNormalize 10.03   0.66    4.77

The new version of the package performs some computationally intensive cross-
validation. The example is relatively minimal and should not take much longer 
than it did in this case, but is necessary to show how the process works.

## Downstream dependencies
There are currently no downstream dependencies for this package

