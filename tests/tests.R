
train <- rgamma(100, 1, 1)
LW.obj <- Lambert(train)
BC.obj <- BC(train)
YJ.obj <- YJ(train)
orderNorm.obj <- orderNorm.smooth(train)
binarize.obj <- binarize(train)
BNobject <- bestNormalize(train) 

# Test transformations
all.equal(YJ.obj$x.t, predict.yj(YJ.obj))
all.equal(LW.obj$x.t, predict.lw(LW.obj))
all.equal(BC.obj$x.t, predict.bc(BC.obj))
all.equal(orderNorm.obj$x.t, predict.orderNorm.smooth(orderNorm.obj))
all.equal(binarize.obj$x.t, predict.binarize(binarize.obj))

# Test reverse transformations
all.equal(YJ.obj$x, predict.yj(YJ.obj, inverse = T))
all.equal(LW.obj$x, predict.lw(LW.obj, inverse = T))
all.equal(BC.obj$x, predict.bc(BC.obj, inverse = T))
all.equal(orderNorm.obj$x, 
          predict.orderNorm.smooth(orderNorm.obj, inverse = T), 
          tolerance = (.Machine$double.eps)^0.25)

###### Test BestNormalize
# Test BC
BNobject <- bestNormalize(train)
all.equal(BNobject$x.t, predict.BN(BNobject))
all.equal(BNobject$x, predict.BN(BNobject, inverse = T))

# Test Yeo-Johnson BN 
BNobject <- bestNormalize(c(train, -1))
all.equal(BNobject$x.t, predict.BN(BNobject))
all.equal(BNobject$x, predict.BN(BNobject, inverse = T))

# Test lw BN 
BNobject <- bestNormalize(rgamma(100, 10, 1))
all.equal(BNobject$x.t, predict.BN(BNobject))
all.equal(BNobject$x, predict.BN(BNobject, inverse = T))

# Test orderNorm BN 
BNobject <- bestNormalize(train, D_max = .01,
                          allow_orderNorm = T)
all.equal(BNobject$x.t, predict.BN(BNobject))
all.equal(BNobject$x, predict.BN(BNobject, inverse = T), 
          tolerance = (.Machine$double.eps)^0.25)
