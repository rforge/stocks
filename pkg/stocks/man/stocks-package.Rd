\name{stocks-package}
\alias{stocks-package}
\alias{stocks}
\docType{package}
\title{
Fast Functions for Stock Market Analysis
}
\description{
Functions for analyzing historical performance of stocks or other investments. Calculate maximum draw-down, Sharpe ratio, Sortino ratio, risk return ratio, alpha and beta from the capital assets pricing model (CAPM), and other commonly used metrics of stock performance. C++ used where possible to improve processing speed.
}
\details{
\tabular{ll}{
Package: \tab stocks\cr
Type: \tab Package\cr
Version: \tab 1.1.2\cr
Date: \tab 2015-03-23\cr
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
\code{\link{balances.weighted}}
\code{\link{prices.rate}}
\code{\link{gains.rate}}
\code{\link{pos}}
\code{\link{neg}}
\code{\link{nonpos}}
\code{\link{nonneg}}
\code{\link{mdd}}
\code{\link{sharpe.ratio}}
\code{\link{sortino.ratio}}
\code{\link{rrr}}
\code{\link{ticker.dates}}
\code{\link{monthly.gains}}
\code{\link{yearly.gains}}
}
\author{
Dane R. Van Domelen

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
1. Jeffrey A. Ryan (2014). quantmod: Quantitative Financial Modelling Framework. R package version 0.4-2/r613. http://R-Forge.R-project.org/projects/quantmod/.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\seealso{
NA
}
\examples{
# Load and plot 10 years of daily closing prices for hypothetical stock ZZZ
data(zzz)
plot(zzz)

# Overall 10-year growth is about 0.846, or 84.6\%
prices.rate(zzz)

# Average annual growth is about 6.3\%
prices.rate(zzz, xday.rate = 251)

# Calculate ratio of closing prices for each day relative to the previous day
daily.ratios <- ratios(zzz)

# Figure out final balance if we had invested $10,000 on day 1
final.balance(daily.ratios)

# Calculate Sharpe ratio
sharpe.ratio(prices = zzz)

# Calculate Sortino ratio
sortino.ratio(prices = zzz)

# Calculate maximum drawdown
mdd(prices = zzz)

# Calculate risk-return ratio
rrr(prices = zzz)

# View list of Vanguard ETFs
data(vanguard.etfs)
vanguard.etfs
}