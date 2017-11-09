
<!-- README.md is generated from README.Rmd. Please edit that file -->
bestNormalize: Flexibly calculate the best normalizing transformation for a vector
==================================================================================

The `bestNormalize` R package was designed to help find a normalizing transformation for a vector. There are many techniques that have been developed in this aim, however each has been subject to their own strengths/weaknesses, and it is unclear on how to decide which will work best until the data is oberved. This package will look at a range of possible transformations and return the best one, i.e. the one that makes it look the *most* normal.

This package also introduces a new normalization technique, `orderNorm`, which transforms the data based off of a rank mapping to the normal distribution, which allows us to *guarantee* normally distributed transformed data (if ties are not present).

Installation
------------

You can install bestNormalize from github with:

``` r
# install.packages("devtools")
devtools::install_github("petersonR/bestNormalize")
```

Example
-------

In this example, we generate 1000 draws from a gamma distribution, and normalize them:

``` r
library(bestNormalize)
set.seed(100)
x <- rgamma(1000, 1, 1)

# Estimate best transformation
BN_obj <- bestNormalize(x)
BN_obj
#> Best Normalizing transformation with 1000 Observations
#>  Estimated Normality Statistics (Pearson P / df, lower => more normal):
#>  - Box-Cox: 0.8188 
#>  - Lambert's W: 1.28 
#>  - Yeo-Johnson: 5.8284 
#>  - orderNorm: 0.0066 
#>  
#> Based off these, bestNormalize chose:
#> OrderNorm Transformation with 1000 nonmissing obs and no ties 
#>  - Original quantiles:
#>    0%   25%   50%   75%  100% 
#> 0.000 0.253 0.693 1.437 7.431

# Perform transformation
gx <- predict(BN_obj)

# Perform reverse transformation
x2 <- predict(BN_obj, newdata = gx, inverse = TRUE)

# Prove the transformation is 1:1
all.equal(x2, x)
#> [1] TRUE
```
