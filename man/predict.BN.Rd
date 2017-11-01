\name{predict.BN}
\alias{predict.BN}
\alias{predict.lw}
\alias{predict.bc}
\alias{predict.yj}
\alias{predict.orderNorm.smooth}
\alias{predict.binarize}

\title{
Predictions for transformation functions
}
\description{
These functions take an object of their respective types, and produce transformed values according to the transformation used on the original data.  All functions are 1-to-1 except for the binarize transformation, so inverse transformations are also supported.}
\usage{
predict.BN(BNobject, newdata = NULL, inverse = F)
predict.lw(LW.obj, newdata = NULL, inverse = F) 
predict.bc(BC.obj, newdata = NULL, inverse = F)
predict.yj(YJ.obj, newdata = NULL, inverse = F)

predict.orderNorm.smooth(orderNorm.obj, newdata = NULL, inverse = F)
revGam(gam_fit, newdata = NULL)

predict.binarize(binarize.obj, newdata = NULL, inverse = F)
}
\arguments{
  \item{BNobject}{
Object produced by bestNormalize function
}
  \item{newdata}{
An optional vector with which to predict. 
}
  \item{inverse}{
If TRUE, returns vector of reverse-transformed values.
}
  \item{LW.obj, BC.obj, YJ.obj, orderNorm.obj, binarize.obj}{
  Objects produced by their respective functions
}
  \item{gam_fit}{
if reverse transformations of gam models are requested, this function helps perform this reverse transformation.
}

}
\details{
If newdata omitted and !inverse, the original train values are used.  If omitted and inverse == T, the transformed values are used.  

if object is binarize, inverse == TRUE returns factor.

Note that the revGam function is very innefficient, and can take a fairly long time for large vectors.  Thus, reverse transformations of the orderNorm function should be carefully considered and avoided if possible.
}
\value{
A vector of predicted values
}

\author{
Ryan Peterson
}

\seealso{
\code{\link{bestNormalize}}, 
\code{\link{Lambert}}, 
\code{\link{BC}}, 
\code{\link{YJ}}, 
\code{\link{orderNorm.smooth}},
\code{\link{binarize}}

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
