# bestNormalize 0.2.2.9002

- Added experimental feature to estimate out-of-sample normality statistics in bestNormalize 
  instead of in-sample ones via repeated cross-validation
    - Note: set `out_of_sample = FALSE` to maintain backward-compatibility with prior versions

- Improved extrapolation of the ORQ (orderNorm) method
    - Instead of linear extrapolation, it uses binomial (logit-link) model on ranks
    
- Added plotting feature for transformation objects

# bestNormalize 0.2.2

- Changed the name of the orderNorm technique to "Ordered Quantile normalization".

# bestNormalize 0.2.1

- Made description more clear in response to comments from CRAN 

# bestNormalize 0.2.0

## First submission to CRAN
