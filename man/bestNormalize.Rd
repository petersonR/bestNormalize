\name{bestNormalize}

\alias{bestNormalize}
\alias{Lambert}
\alias{YJ}
\alias{BC}
\alias{orderNorm.smooth}
\alias{binarize}

\title{
Create Transformation object
}
\description{
These functions create objects with transformation information, including transformed values.  The objects can then be passed into their respective predict functions to apply the transformation (or reverse transformation, with the exception of binarize).
}
\usage{
bestNormalize(x, norm_test = c("lillie"), D_max = 1, allow_orderNorm = T, ...)
Lambert(x, type = "s")
YJ(x, ...)
BC(x, ...)
orderNorm.smooth(x, ties_method = c('warn', 'jitter'))
binarize(x, location_measure = 'median')
}
\arguments{
  \item{x}{
Training data to be used to build normalizing transformation.
}
  \item{norm_test}{
The name of the normality test to be used. (Currently only 'lillie' supported.)
}
  \item{D_max}{
Maximum value of the D statistic for the normality of the transformed values before binarizing (see references)
}
  \item{allow_orderNorm}{
Should a rank-based normalization technique be allowed?
}

  \item{ties_method}{
How should orderNorm handle ties? (recommended is 'warn', since normality is not guaranteed if ties)
}

\item{location_measure} {
Central location estimator for binarize function ("median", "mode", or "mean")
}

  \item{\dots}{
Other objects passed to normalizing functions.
}
}
\details{
See package vignette for more information and details about these transformations.  
}
\value{
\item{x.t}{Transformed values}
\item{x}{Original values}
\item{mean}{mean used for transformation centering}
\item{sd}{scale used for transformation scaling}
\item{n}{number of values in vector}
\item{method}{Name of transformation method used}

\item{lambda}{lambda for BC and YJ functions}
\item{type}{type used for Lambert function}
\item{tau.mat}{parameter values for Lambert function}
\item{ties_method}{ties method for orderNorm (usually "warn")}
\item{warn_status}{flag to say if ties were found for orderNorm}

\item{location_measure}{Central location estimator for binarize function}
\item{location}{Central location estimate for binarize function}
}
\references{
Dallal, G.E. and Wilkinson, L. (1986): An analytic approximation to the distribution of Lilliefors'
test for normality. The American Statistician, 40, 294-296.}

Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. JRSS B 26 211-246.

Goerg, G.M. (2011). "Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation". Annals of Applied Statistics, 5 (3), 2197-2230.

Yeo, I.-K. and Johnson, R. A. (2000) A new family of power transformations to improve normality or symmetry. Biometrika, 87, 954-959.

\author{
Ryan Peterson
}

\seealso{
\code{\link{nortest::lillie.test}} for information on normality test and statistic.  \code{\link{VGAM::yeo.johnson}}, \code{\link{forecast::BoxCox}}, \code{\link{LambertW::Gaussianize}}, \code{\link{mgcv::gam}} for further transformation information.

For predictions using these functions, see 
\code{\link{predict.BN}}, 
\code{\link{predict.lw}}, 
\code{\link{predict.bc}}, 
\code{\link{predict.yj}}, 
\code{\link{predict.orderNorm.smooth}},
\code{\link{predict.binarize}}
}
\examples{

set.seed(100)
train <- rgamma(100, 1, 1)
LW.obj <- Lambert(train)
BC.obj <- BC(train)
YJ.obj <- YJ(train)
orderNorm.obj <- orderNorm.smooth(train)

binarize.obj <- binarize(train)
BNobject <- bestNormalize(train) 

xx <- seq(min(train), max(train), length = 100)
plot(xx, predict.lw(LW.obj, newdata = xx), type = 'l', col = 1)
lines(xx, predict.bc(BC.obj, newdata = xx), col = 2)
lines(xx, predict.yj(YJ.obj, newdata = xx), col = 3)
lines(xx, predict.orderNorm.smooth(orderNorm.obj, newdata = xx), col = 4)

legend('topleft', legend = c('LambertW', 'BoxCox', 'YeoJohnson', 'OrderNorm'), 
       col = 1:4, lty = 1)

hist(train)
par(mfrow = c(2,2))
hist(LW.obj$x.t, main = 'LambertW transformation')
hist(BC.obj$x.t, main = 'BoxCox transformation')
hist(YJ.obj$x.t, main = 'Yeo-Johnson transformation')
hist(orderNorm.obj$x.t, main = 'orderNorm transformation')

table(binarize.obj$x.t)

hist(BNobject$x.t, main = paste('Best Transformation:', BNobject$method))
par(mfrow = c(1,1))
plot(xx, predict.BN(BNobject, newdata = xx), type = 'l', col = 1, 
     main = 'Best Normalizing transformation', ylab = 'f(xx)')
