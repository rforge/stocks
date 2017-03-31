\name{stocks-package}
\alias{stocks-package}
\alias{stocks}
\docType{package}
\title{
Fast Functions for Stock Market Analysis
}
\description{
Functions for analyzing stocks or other investments. Main features are loading and aligning historical data for ticker symbols, calculating performance metrics for individual funds or portfolios (e.g. compound annualized growth rate, maximum drawdown, Sharpe/Sortino ratio), and creating graphs. C++ code is used where possible to improve processing speed.
}
\details{
\tabular{ll}{
Package: \tab stocks\cr
Type: \tab Package\cr
Version: \tab 1.1.2\cr
Date: \tab 2016-03-31\cr
License: \tab GPL-2\cr
}
The following functions are included:

\code{\link{diffs}}
\code{\link{pdiffs}}
\code{\link{pchanges}}
\code{\link{ratios}}
\code{\link{pos}}
\code{\link{neg}}
\code{\link{nonpos}}
\code{\link{nonneg}}
\code{\link{convert.rate}}
\code{\link{daily.yearly}}
\code{\link{yearly.daily}}
\code{\link{balances}}
\code{\link{final.balance}}
\code{\link{ticker.dates}}
\code{\link{load.gains}}
\code{\link{load.prices}}
\code{\link{prices.rate}}
\code{\link{gains.rate}}
\code{\link{mdd}}
\code{\link{sharpe.ratio}}
\code{\link{sortino.ratio}}
\code{\link{rrr}}
\code{\link{metrics}}
\code{\link{twofunds.graph}}
\code{\link{threefunds.graph}}
\code{\link{growth.graph}}
\code{\link{gains.graph}}
\code{\link{onemetric.graph}}
\code{\link{twometrics.graph}}
\code{\link{onemetric.overtime.graph}}
}
\author{
Dane R. Van Domelen

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
1. Jeffrey A. Ryan (2016). quantmod: Quantitative Financial Modelling Framework. R package version 0.4-6, \url{https://cran.r-project.org/package=quantmod}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\examples{
# Load historical prices for VFINX and VBLTX
vfinx.vbltx <- load.prices(tickers = c("VFINX", "VBLTX"))

# Calculate total growth for VFINX and VBLTX
apply(vfinx.vbltx, 2, prices.rate)

# Calculate performance metrics for VFINX and VBLTX
metrics(prices = vfinx.vbltx)

# Plot mean vs. SD of daily gains for VFINX/VBLTX portfolios
graph1 <- twofunds.graph(tickers = c("VFINX", "VBLTX"))

# Plot CAGR vs. MDD for VFINX/VBLTX and VFINX/VWEHX portfolios
graph2 <- twofunds.graph(tickers = matrix(c("VFINX", "VBLTX", "VFINX", "VWEHX"), 2),
                         x.metric = "mdd", y.metric = "cagr")
                         
# Plot mean vs. SD of monthly gains for VWEHX/VFINX/VBLTX portfolio
graph3 <- threefunds.graph(tickers = c("VWEHX", "VFINX", "VBLTX"))
}