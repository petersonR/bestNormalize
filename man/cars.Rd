\name{cars}
\alias{cars}
\docType{data}
\title{
Autotrader Cars Data
}
\description{
A web scrape of cars for sale on autotrader.com within 25 miles of Minneapolis, MN.  Data collected on 4/28/2017.
}
\usage{data("cars")}
\format{
  A data frame with 6283 observations on the following 10 variables.
  \describe{
    \item{\code{Car_Info}}{Raw Car information from web}
    \item{\code{Link}}{append to "autotrader.com" for hyperlink to posting}
    \item{\code{Make}}{Car manufacturer}
    \item{\code{Year}}{Year manufactured}
    \item{\code{Location}}{a character vector}
    \item{\code{Radius}}{a numeric vector}
    \item{\code{price}}{Price of vehicle, US 2017 dollars}
    \item{\code{mileage}}{Mileage of vehicle listed}
    \item{\code{status}}{Used vs. Certified vs. New}
    \item{\code{model}}{Make + model}
  }
}
\details{
For more details, contact author.
}
\source{
autotrader.com
}

\examples{
data(cars)

str(cars)
plot(cars$year, cars$price)

}
\keyword{datasets}
