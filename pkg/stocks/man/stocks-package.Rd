\name{stocks-package}
\alias{stocks-package}
\alias{stocks}
\docType{package}
\title{
Fast Functions for Stock Market Analysis
}
\description{
Functions for analyzing stocks or other investments. Main features are loading
and aligning historical data for ticker symbols, calculating performance metrics
for individual funds or portfolios (e.g. compound annualized growth rate,
maximum drawdown, Sharpe/Sortino ratio), and creating graphs. C++ code is used
where possible to improve processing speed.
}
\details{
\tabular{ll}{
Package: \tab stocks\cr
Type: \tab Package\cr
Version: \tab 1.1.2\cr
Date: \tab 2017-10-18\cr
License: \tab GPL-2\cr
}
The following functions are included:

\code{\link{diffs}}\cr
\code{\link{pdiffs}}\cr
\code{\link{pchanges}}\cr
\code{\link{ratios}}\cr
\code{\link{pos}}\cr
\code{\link{neg}}\cr
\code{\link{nonpos}}\cr
\code{\link{nonneg}}\cr
\code{\link{convert.rate}}\cr
\code{\link{daily.yearly}}\cr
\code{\link{yearly.daily}}\cr
\code{\link{balances}}\cr
\code{\link{final.balance}}\cr
\code{\link{ticker.dates}}\cr
\code{\link{load.gains}}\cr
\code{\link{load.prices}}\cr
\code{\link{prices.rate}}\cr
\code{\link{gains.rate}}\cr
\code{\link{mdd}}\cr
\code{\link{sharpe.ratio}}\cr
\code{\link{sortino.ratio}}\cr
\code{\link{rrr}}\cr
\code{\link{metrics}}\cr
\code{\link{beta.trailing50}}\cr
\code{\link{twofunds.graph}}\cr
\code{\link{threefunds.graph}}\cr
\code{\link{growth.graph}}\cr
\code{\link{gains.graph}}\cr
\code{\link{onemetric.graph}}\cr
\code{\link{twometrics.graph}}\cr
\code{\link{onemetric.overtime.graph}}
\code{\link{targetbeta.twofunds}}
\code{\link{contango.simple}}
\code{\link{contango.hedged}}
}
\author{
Dane R. Van Domelen

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
1. Jeffrey A. Ryan (2016). quantmod: Quantitative Financial Modelling Framework.
R package version 0.4-6, \url{https://cran.r-project.org/package=quantmod}.

Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\examples{
# Load historical prices for VFINX and VBLTX
#vfinx.vbltx <- load.prices(tickers = c("VFINX", "VBLTX"))

# Calculate total growth for VFINX and VBLTX
#apply(vfinx.vbltx, 2, prices.rate)

# Calculate performance metrics for VFINX and VBLTX
#metrics(prices = vfinx.vbltx)

# Plot mean vs. SD of daily gains for VFINX/VBLTX portfolios
#graph1 <- twofunds.graph(tickers = c("VFINX", "VBLTX"))

# Plot CAGR vs. MDD for VFINX/VBLTX and VFINX/VWEHX portfolios
#graph2 <- twofunds.graph(tickers = matrix(c("VFINX", "VBLTX", "VFINX", "VWEHX"), 2),
#                         x.metric = "mdd", y.metric = "cagr")

# Plot mean vs. SD of monthly gains for VWEHX/VFINX/VBLTX portfolio
#graph3 <- threefunds.graph(tickers = c("VWEHX", "VFINX", "VBLTX"))
}
