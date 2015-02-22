\name{stocks-package}
\alias{stocks-package}
\alias{stocks}
\docType{package}
\title{
Fast Functions for Stock Market Analysis
}
\description{
Provides functions for analyzing historical performance of stocks or other investments. Functions are written in C++ to quickly calculate maximum draw-down, Sharpe ratio, risk return ratio, and other commonly used metrics of stock performance.
}
\details{
\tabular{ll}{
Package: \tab stocks\cr
Type: \tab Package\cr
Version: \tab 1.1.1\cr
Date: \tab 2015-02-22\cr
License: \tab GPL-2\cr
}
The following functions are included:

\code{\link{convert.rate}}
\code{\link{diffs}}
\code{\link{pdiffs}}
\code{\link{pchanges}}
\code{\link{ratios}}
\code{\link{balances}}
\code{\link{final.balance}}
\code{\link{balances.rate}}
\code{\link{gains.rate}}
\code{\link{positives}}
\code{\link{negatives}}
\code{\link{nonpositives}}
\code{\link{nonnegatives}}
\code{\link{mdd}}
\code{\link{sharpe}}
\code{\link{sortino}}
\code{\link{rrr}}
}
\author{
Dane R. Van Domelen

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\seealso{
NA
}
\examples{
# Load 10 years of daily closing prices for hypothetical stock ZZZ
data(zzz)

# Overall growth rate is about 0.846, or 84.6\%
prices.rate(zzz)

# Average annual growth is about 6.3\%
prices.rate(zzz, xday.rate = 251)

# Calculate ratio of closing prices for each day relative to the previous day
daily.ratios <- ratios(zzz)

# Figure out final balance if we had invested $10,000 on day 1
final.balance(ratios)

# Calculate Sharpe ratio
sharpe(prices = zzz)

# Calculate Sortino ratio
sortino(prices = zzz)

# Calculate maximum drawdown
mdd(prices = zzz)

# Calculate risk-return ratio
rrr(prices = zzz)
}