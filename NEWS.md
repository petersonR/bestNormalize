# bestNormalize 1.0.0.9000

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
