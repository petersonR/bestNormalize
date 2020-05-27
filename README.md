
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bestNormalize: Flexibly calculate the best normalizing transformation for a vector

[![Travis-CI Build
Status](https://travis-ci.org/petersonR/bestNormalize.svg?branch=master)](https://travis-ci.org/petersonR/bestNormalize)
[![CRAN
version](https://www.r-pkg.org/badges/version/bestNormalize)](https://cran.r-project.org/package=bestNormalize)
[![downloads](https://cranlogs.r-pkg.org/badges/bestNormalize)](https://cran.r-project.org/package=bestNormalize)

The `bestNormalize` R package was designed to help find a normalizing
transformation for a vector. There are many techniques that have been
developed in this aim, however each has been subject to their own
strengths/weaknesses, and it is unclear on how to decide which will work
best until the data is oberved. This package will look at a range of
possible transformations and return the best one, i.e. the one that
makes it look the *most* normal.

Note that some authors use the term “normalize” differently than in this
package. We define “normalize”: to transform a vector of data in such a
way that the transformed values follow a Gaussian distribution (or
equivalently, a bell curve). This is in contrast to other such
techniques designed to transform values to the 0-1 range, or to the -1
to 1 range.

This package also introduces a new adaptation of a normalization
technique, which we call Ordered Quantile normalization (`orderNorm()`,
or ORQ). ORQ transforms the data based off of a rank mapping to the
normal distribution. This allows us to *guarantee* normally distributed
transformed data (if ties are not present). The adaptation uses a
shifted logit approximation on the ranks transformation to perform the
transformation on newly observed data outside of the original domain. On
new data within the original domain, the transformation uses linear
interpolation of the fitted transformation.

To evaluate the efficacy of the normalization technique, the
`bestNormalize()` function implements repeated cross-validation to
estimate the Pearson’s P statistic divided by its degrees of freedom.
This is called the “Normality statistic”, and if it is close to 1 (or
less), then the transformation can be thought of as working well. The
function is designed to select the transformation that produces the
lowest P / df value, when estimated on out-of-sample data (estimating
this on in-sample data will always choose the orderNorm technique, and
is generally not the main goal of these procedures).

## Installation

You can install the most recent (devel) version of bestNormalize from
github with:

``` r
# install.packages("devtools")
devtools::install_github("petersonR/bestNormalize")
```

Or, you can download it from CRAN with:

``` r
install.packages("bestNormalize")
```

## Example

In this example, we generate 1000 draws from a gamma distribution, and
normalize them:

``` r
library(bestNormalize)
```

``` r
set.seed(100)
x <- rgamma(1000, 1, 1)

# Estimate best transformation with repeated cross-validation
BN_obj <- bestNormalize(x, allow_lambert_s = TRUE)
BN_obj
#> Best Normalizing transformation with 1000 Observations
#>  Estimated Normality Statistics (Pearson P / df, lower => more normal):
#>  - arcsinh(x): 3.6204
#>  - Box-Cox: 0.96
#>  - Exp(x): 50.8513
#>  - Lambert's W (type s): 1.0572
#>  - Log_b(x+a): 1.908
#>  - No transform: 6.7851
#>  - orderNorm (ORQ): 1.0516
#>  - sqrt(x + a): 1.4556
#>  - Yeo-Johnson: 1.7385
#> Estimation method: Out-of-sample via CV with 10 folds and 5 repeats
#>  
#> Based off these, bestNormalize chose:
#> Standardized Box Cox Transformation with 1000 nonmissing obs.:
#>  Estimated statistics:
#>  - lambda = 0.2739638 
#>  - mean (before standardization) = -0.3870903 
#>  - sd (before standardization) = 1.045498

# Perform transformation
gx <- predict(BN_obj)

# Perform reverse transformation
x2 <- predict(BN_obj, newdata = gx, inverse = TRUE)

# Prove the transformation is 1:1
all.equal(x2, x)
#> [1] TRUE
```

As of version 1.3, the package supports leave-one-out cross-validation
as well. ORQ normalization works very well when the size of the test
dataset is low relative to the training data set, so it will often be
selected via leave-one-out cross-validation (which is why we set
`allow_orderNorm = FALSE`
here).

``` r
(BN_loo <- bestNormalize(x, allow_orderNorm = FALSE, allow_lambert_s = TRUE, loo = TRUE))
#> Best Normalizing transformation with 1000 Observations
#>  Estimated Normality Statistics (Pearson P / df, lower => more normal):
#>  - arcsinh(x): 14.0712
#>  - Box-Cox: 0.8077
#>  - Exp(x): 451.435
#>  - Lambert's W (type s): 1.269
#>  - Log_b(x+a): 4.5374
#>  - No transform: 26.624
#>  - sqrt(x + a): 3.3655
#>  - Yeo-Johnson: 5.7997
#> Estimation method: Out-of-sample via leave-one-out CV
#>  
#> Based off these, bestNormalize chose:
#> Standardized Box Cox Transformation with 1000 nonmissing obs.:
#>  Estimated statistics:
#>  - lambda = 0.2739638 
#>  - mean (before standardization) = -0.3870903 
#>  - sd (before standardization) = 1.045498
```

It is also possible to visualize these transformations:

``` r
plot(BN_obj, leg_loc = "bottomright")
```

For a more in depth tutorial, please consult [the package
vignette](https://CRAN.R-project.org/package=bestNormalize/vignettes/bestNormalize.html).
