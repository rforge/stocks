\name{prices.rate}
\alias{prices.rate}
\title{
Calculate Growth Rate From a Vector of Stock Prices or Investment Balances
}
\description{
The formula is simply: overall.rate = prices[length(prices)] / prices[1] - 1. If xday is specified, then it converts to x-day growth rate. For example, if prices is a vector of stock prices over a five-day period, say c(100.4, 98.7, 101.3, 101.0, 100.9), then the overall growth rate is approximately 0.0050 over four full trading days. If xday.rate is set to 1, this rate is converted to an average daily rate of approximately 
}
\usage{
prices.rate(prices, xday.rate = NULL)
}
\arguments{
  \item{prices}{
Numeric vector of stock prices or investment balances. Has to be daily prices if you want to calculate x-day rather than overall growth.
}
  \item{xday.rate}{
Number of days for growth rate calculation. If unspecified, function returns overall growth. If set to 251, function would return annual growth rate.
}
}
\details{
NA
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
\note{
NA
}
\seealso{
NA
}
\examples{
# Create vector of daily closing prices for a hypothetical stock
prices <- c(100.4, 98.7, 101.3, 101.0, 100.9)

# Overall growth is 0.50\%
prices.rate(prices)

# Average daily growth is 0.12\%
prices.rate(prices, 1)

# Corresponds to 36.6\% annual growth
prices.rate(prices, 251)
}