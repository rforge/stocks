\name{mdd}
\alias{mdd}
\title{
Maximum Drawdown
}
\description{
Calculates maximum drawdown for a numeric vector.
}
\usage{
mdd(prices = NULL, 
    gains = NULL, 
    highs = NULL, lows = NULL, 
    indices = FALSE, 
    nas = FALSE)
}
\arguments{
  \item{prices}{
Numeric vector of investment prices. Can be closing prices, but minute-to-minute stock prices are preferred since maximum drawdown is really calculated from overall highs and lows, not daily closing prices.
}
  \item{gains}{
Numeric vector of gains (daily or otherwise).
  }
  \item{highs}{
Numeric vector of daily highs.
}
  \item{lows}{
Numeric vector of daily lows.
}
  \item{indices}{
If \code{TRUE}, function returns maximum drawdown and the indices corresponding to the start and end of the drawdown period; if \code{FALSE}, function only returns the maximum drawdown.
}
  \item{nas}{
If \code{TRUE}, function finds and removes any missing values (\code{NA}s) in \code{prices}. The default is \code{FALSE} to maximize speed for the usual case where there are no missing values.
}
}
\details{
Drawdown is defined here as 1 minus the ratio of the current stock price to the historical maximum price. For example, if the current price is \$50, and the previous maximum was \$80, the drawdown is \code{1 - (50 / 80) = 0.375}. Maximum drawdown is calculated by going through the entire history of a stock, calculating drawdown for every single data point, and taking the maximum of those values.

This function requires a prices vector OR a gains vector OR a highs vector and a lows vector. A prices vector where each price represents a minute-to-minute update is preferred, because then there is no risk of missing a high or low value and potentially miscalculating maximum drawdown. Using daily closing prices would result in something slightly different than maximum drawdown being calculated, but some users might still want to calculate that metric. If gains (e.g. minute-to-minute or daily) are input rather than balances, the function internally generates a balances vector based on the gains, and then calculates maximum drawdown from the balances vector.

If highs and lows vectors are provided, they should represent daily highs and lows for a stock. This should give equivalent results as entering a minute-to-minute price vector EXCEPT when the true maximum drawdown occurs when a stock goes from a historical maximum to a lower value within a single day. In that scenario, it is impossible to tell from the highs and lows vector whether the historical high preceded the low. The function arbitrarily assumes that the low came first.
}
\value{
If \code{indices} is \code{TRUE}, a numeric vector indicating the maximum drawdown as well as the indices corresponding to the start and end of the drawdown period; if \code{indices} is \code{FALSE}, a numeric value indicating the maximum drawdown.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
This function uses C++ code to achieve a 2-3 times speed increase compared to the base R code: \code{1 - min(prices / cummax(prices))}.
}
\seealso{
\code{\link{gains.rate}}, \code{\link{prices.rate}}, \code{\link{sharpe.ratio}}, \code{\link{sortino.ratio}}, \code{\link{rrr}}
}
\examples{
# Randomly generate minute-to-minute stock gains over a 2-year period
set.seed(123)
stock.gains <- rnorm(6.5 * 60 * 252 * 2, 0.000005, 0.001)

# Convert to stock prices assuming an initial price of $9.50 per share
stock.prices <- balances(ratios = stock.gains + 1, initial = 9.50)

# Plot minute-to-minute stock prices (200k data point, may be slow)
plot(stock.prices)

# Maximum drawdown based on stock prices
mdd(prices = stock.prices)

# Same answer using gains rather than prices
mdd(gains = stock.gains)
}