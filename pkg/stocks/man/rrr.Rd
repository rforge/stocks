\name{rrr}
\alias{rrr}
\title{
Risk-Return Ratio
}
\description{
Calculate ratio if investment return to investment risk based on a vector of prices or gains. The formula is: growth rate / mdd. Here growth rate is the growth of a stock or investment of some time period, and mdd is the maximum drawdown over that time period. User can specify prices or gains.
}
\usage{
rrr(prices = NULL, gains = NULL, highs = NULL, lows = NULL, nas = FALSE)
}
\arguments{
  \item{prices}{
Numeric vector of stock prices (typically daily closing prices). 
}
  \item{gains}{
Numeric vector of gains.
}
  \item{highs}{
Numeric vector of daily highs. 
}
  \item{lows}{
Numeric vector of daily lows.
}
  \item{nas}{
If TRUE, function finds and removes any missing values (NA's) in prices vector. The default is FALSE to maximize speed for the usual case where there are no missing values.
}  
}
\value{
Numeric value indicating the risk-return ratio.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{sharpe.ratio}}, \code{\link{sortino.ratio}}, \code{\link{mdd}}
}
\examples{
# Randomly generate daily stock gains over a 5-year period
set.seed(123)
stock.gains <- rnorm(251*5, 0.0005, 0.01)

# Convert to daily balances assuming an initial balance of $10,000
daily.balances <- balances(stock.gains + 1)

# Total return is about 1.23
daily.balances[length(daily.balances)] / daily.balances[1] - 1

# Maximum drawdown is about 0.19
mdd(daily.balances)

# Ratio of these two is about 6.48
(daily.balances[length(daily.balances)] / daily.balances[1] - 1) / mdd(daily.balances)

# Easier to calculate using rrr function
rrr(daily.balances)
}