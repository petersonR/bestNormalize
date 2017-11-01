\name{bestNormalize-package}
\alias{bestNormalize-package}
\alias{bestNormalize}
\docType{package}
\title{
\packageTitle{bestNormalize}
}
\description{
\packageDescription{bestNormalize}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{bestNormalize}
\packageIndices{bestNormalize}
This package was built to have a similar feel to the preProcess functionality in the caret package.  Transformations can be built and stored into objects, then those objects can be used to transform/retransform new data based off the training set.  This is useful for de-skewing data.
}
\author{
\packageAuthor{bestNormalize}
}
\references{
Dallal, G.E. and Wilkinson, L. (1986): An analytic approximation to the distribution of Lilliefors'
test for normality. The American Statistician, 40, 294-296.}

Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. JRSS B 26 211-246.

Goerg, G.M. (2011). "Lambert W Random Variables - A New Family of Generalized Skewed Distributions with Applications to Risk Estimation". Annals of Applied Statistics, 5 (3), 2197-2230.

Yeo, I.-K. and Johnson, R. A. (2000) A new family of power transformations to improve normality or symmetry. Biometrika, 87, 954-959.
}
\keyword{ package }
\seealso{
\code{\link[<caret>:<caret>-package]{<caret>}}
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
}
