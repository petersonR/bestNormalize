## Test environments 
- win-builder (release)
- local macOS (Catalina 11.2.2)
- travis-ci.org Ubuntu (R-release, R-devel)

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies
mlr3pipelines suggests bestNormalize but only uses its 
documentation and a minor transformation in a unit test. 
Therefore it will not be affected. 
