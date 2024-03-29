% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log_x.R
\name{log_x}
\alias{log_x}
\alias{predict.log_x}
\alias{print.log_x}
\title{Log(x + a) Transformation}
\usage{
log_x(x, a = NULL, b = 10, standardize = TRUE, eps = 0.001, warn = TRUE, ...)

\method{predict}{log_x}(object, newdata = NULL, inverse = FALSE, ...)

\method{print}{log_x}(x, ...)
}
\arguments{
\item{x}{A vector to normalize with with x}

\item{a}{The constant to add to x (defaults to max(0, -min(x) + eps));
see \code{bestLogConstant}}

\item{b}{The base of the log (defaults to 10)}

\item{standardize}{If TRUE, the transformed values are also centered and
scaled, such that the transformation attempts a standard normal}

\item{eps}{The allowed error in the expression for the selected a}

\item{warn}{Should a warning result from infinite values?}

\item{...}{additional arguments}

\item{object}{an object of class 'log_x'}

\item{newdata}{a vector of data to be (potentially reverse) transformed}

\item{inverse}{if TRUE, performs reverse transformation}
}
\value{
A list of class \code{log_x} with elements 
\item{x.t}{transformed 
  original data} 
  \item{x}{original data} 
  \item{mean}{mean after transformation but prior to standardization} 
  \item{sd}{sd after transformation but prior to standardization} 
  \item{a}{estimated a value} 
  \item{b}{estimated base b value} 
  \item{n}{number of nonmissing observations}
  \item{norm_stat}{Pearson's P / degrees of freedom}
  \item{standardize}{was the transformation standardized}
  
  The \code{predict} function returns the numeric value of the transformation
  performed on new data, and allows for the inverse transformation as well.
}
\description{
Perform a log_b (x+a) normalization transformation
}
\details{
\code{log_x} performs a simple log transformation in the context of 
bestNormalize, such that it creates a transformation that can be estimated
and applied to new data via the \code{predict} function. The parameter a is
essentially estimated by the training set by default (estimated as the minimum
possible to some extent epsilon), while the base must be 
specified beforehand.
}
\examples{
x <- rgamma(100, 1, 1)

log_x_obj <- log_x(x)
log_x_obj
p <- predict(log_x_obj)
x2 <- predict(log_x_obj, newdata = p, inverse = TRUE)

all.equal(x2, x)

}
