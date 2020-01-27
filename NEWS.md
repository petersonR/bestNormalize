# bestNormalize 1.4.3

- Allow options to be passed through bestNormalize to specific transformation functions 
- Slight bug fix to square root transformation (a = 0 by default, not .001)
- Slight bug fix in the "quiet" argument for bestNormalize with LOO
- Slight bug fix to `plot.bestNormalize` which was improperly labeling transformations
- `exp_x` having trouble with `standardize` option, so added option `allow_exp_x` to 
  `bestNormalize` to allow a workaround, and changed it so if any infinite values
  are produced during the transformation, exp_x will not work (that way, `bestNormalize`
  will not include this in its results).
- Progress bar will now only displayed if `quiet` is `FALSE` and `length(x) > 2000`

# bestNormalize 1.4.2

- Update citation to point to newly published work.
- Update maintainer email to new address (same person, new affiliation). 

# bestNormalize 1.4.1

- Correctly subtract 1/2 from ranks in ORQ transformation to make quantile estimation unbiased (this was a bug in 1.3.0, as ranks start at 1, not zero). Divides by n instead of n+1. 
- Specify the weights for the GLM in the ORQ transformation to be the number of observations. This doesn't change the transformation but seems to have a bit faster computational speed, and it's more mathematically tractable. 
- Other various bug fixes to tests and to plotting functions.

# bestNormalize 1.3.0

- Add 1/2 to ranks in ORQ transformation to make quantile estimation unbiased (should have minimal impact)
- Add option `loo` for leave-one-out cross-validation
- Add progress bar for cross-validation methods (both with/without parallel) 
- Add "no_transform" function - does the same thing as I(x) but in the syntax of other transformations 
  (this allows the normalization statistics to also be calculated if no transformation is performed). 
- Add support for lambert transforms of type "h" in the `bestNormalize` function via `allow_lambert_h` argument.
- Add "before standardization" to printout of different transforms' means and sds to clarify output

# bestNormalize 1.2.0

- Added other transformations commonly used to normalize a vector 
    - exponential, log, square root, arcsinh
- Lambert WxF is no longer done by default by bestNormalize since it is 
  unstable on certain OS (Linux, Solaris), and does not abide by the CRAN
  policy. 

# bestNormalize 1.1.0

- Clarified that the transformations are standardized by default, and providing option to not standardize in transformations
- Updated tests to run a bit faster and to use proper S3 classes

# bestNormalize 1.0.1

- Added references for original papers (Van der Waerden, Bartlett) 
  that cite the basis for the orderNorm transformation, as well as discussion 
  in Beasley (2009)
- Edited description to clarify that this procedure is a new adaptation of an 
  older technique rather than a new technique in itself

# bestNormalize 1.0.0

- Added feature to estimate out-of-sample normality statistics in bestNormalize 
  instead of in-sample ones via repeated cross-validation
    - Note: set `out_of_sample = FALSE` to maintain backward-compatibility with prior versions
      and set `allow_orderNorm = FALSE` as well so that it isn't automatically selected

- Improved extrapolation of the ORQ (orderNorm) method
    - Instead of linear extrapolation, it uses binomial (logit-link) model on ranks
    - No more issues with Cauchy transformation
    
- Added plotting feature for transformation objects

- Cleared up some documentation

# bestNormalize 0.2.2

- Changed the name of the orderNorm technique to "Ordered Quantile normalization".

# bestNormalize 0.2.1

- Made description more clear in response to comments from CRAN 

# bestNormalize 0.2.0

## First submission to CRAN
