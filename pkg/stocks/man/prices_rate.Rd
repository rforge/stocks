\name{prices.rate}
\alias{prices.rate}
\title{
Calculate Growth Rate From a Vector of Stock Prices or Investment Balances
}
\description{
The formula is simply: \code{overall.rate = prices[length(prices)] / prices[1] - 1}. If \code{units.rate} is specified, then it converts to x-unit growth rate.
}
\usage{
prices.rate(prices, units.rate = NULL, nas = FALSE)
}
\arguments{
  \item{prices}{
Numeric vector of stock prices or investment balances.
}
  \item{units.rate}{
Number of units for growth rate calculation. If unspecified, function returns total growth. To get annualized growth rate, set to 252 if \code{prices} has daily prices, 12 if \code{prices} has monthly prices, etc.
}
  \item{nas}{
If \code{TRUE}, function finds and removes any missing values (\code{NA}s) in \code{prices}. The default is \code{FALSE} to maximize speed for the usual case where there are no missing values.
}
}
\value{
Numeric value indicating the growth rate.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{gains.rate}}
}
\examples{
# Create vector of daily closing prices for a hypothetical stock
prices <- c(100.4, 98.7, 101.3, 101.0, 100.9)

# Overall growth is 0.50\%
prices.rate(prices)

# Average daily growth is 0.12\%
prices.rate(prices, 1)

# Corresponds to 36.6\% annualized growth
prices.rate(prices, 252)
}