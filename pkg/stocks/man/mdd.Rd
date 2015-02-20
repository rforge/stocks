\name{mdd}
\alias{mdd}
\title{
Maximum Drawdown
}
\description{
Calculates maximum drawdown for a numeric vector (usually a vector of daily closing prices for a stock).
}
\usage{
mdd(prices = NULL, highs = NULL, lows = NULL, indices = FALSE)
}
\arguments{
  \item{prices}{
Numeric vector of stock prices. Can be closing prices, but minute-to-minute stock prices are preferred since maximum drawdown is calculated from overall highs and lows, not daily closing prices.
}
  \item{highs}{
Numeric vector of daily highs for the stock of interest.  
}
  \item{lows}{
Numeric vector of daily lows for the stock of interest.
}
  \item{indices}{
If TRUE, function returns maximum drawdown and the indices corresponding to the start and end of the drawdown period; if FALSE, function only returns the maximum drawdown.
}
}
\details{
Drawdown is defined as 1 minus the ratio of the current stock price to the historical maximum price. For example, if the current price is \$50, and the previous maximum was \$80, the drawdown is 1 - (50/80) = 0.375. Maximum drawdown is calculated by going through the entire history of a stock, calculating drawdown for every single data point, and taking the maximum of those values.

This function requires a prices vector OR a highs vector and a lows vector. A prices vector where each price represents a minute-to-minute update is preferred, because then there is no risk of missing a high or low value and potentially miscalculating maximum drawdown. Using daily closing prices would result in something slightly different than maximum drawdown being calculated, but some users might still want to calculate that metric.

If a highs and low vector are provided, they should represent daily highs and lows for a stock. This should give equivalent results as entering a minute-to-minute price vector EXCEPT when the true maximum drawdown occurs when a stock goes from a historical maximum to a lower value within a single day. In that scenario, it is impossible to tell from the highs and lows vector whether the historical high preceded the low. The function arbitrarily assumes that the low came first.
}
\value{
If indices is TRUE, a numeric vector indicating the maximum drawdown as well as the indices corresponding to the start and end of the drawdown period; if indices is FALSE, a numeric value indicating the maximum drawdown.
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
# Randomly generate minute-to-minute stock gains over a 5-year period
stockgains <- rnorm(6.5*60*251*5, 1.00001, 0.001)

# Convert to stock prices assuming an initial price of $9.50 per share
}