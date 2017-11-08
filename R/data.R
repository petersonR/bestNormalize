#' Prices of 6,283 cars listed on Autotrader
#'
#' A dataset containing the prices and other attributes of over 6000 cars in the Minneapolis area. 
#'
#' @format A data frame with 6283 rows and 10 variables:
#' \describe{
#'   \item{price}{price, in US dollars}
#'   \item{Car_Info}{Raw description from website}
#'   \item{Link}{hyperlink to listing (must be appended to https://www.autotrader.com/)}
#'   \item{Make}{Car manufacturer}
#'   \item{Year}{Year car manufactured}
#'   \item{Location}{Location of listing}
#'   \item{Radius}{Radius chosen for search}
#'   \item{mileage}{mileage on vehicle}
#'   \item{status}{used/new/certified}
#'   \item{model}{make and model, separated by space}
#' }
#' @source \url{https://www.autotrader.com/}
"autotrader"