\name{targetbeta.twofunds}
\alias{targetbeta.twofunds}
\title{
Implement a Two-Fund Strategy that Targets a Particular Beta
}
\description{
Implements a two-fund strategy where allocations to each fund are adjusted to maintain some user-specified portfolio beta. For example, you could back-test a zero-beta (i.e. market neutral) VFINX/VBLTX strategy using this function.
Creates graph of investment growth for one or more investments.
}
\usage{
targetbeta.twofunds(tickers = NULL, intercepts = NULL, slopes = NULL, ..., 
                    benchmark.ticker = NULL, reference.tickers = NULL,
                    tickers.gains = NULL, benchmark.gains = NULL, reference.gains = NULL, 
                    target.beta = 0, tol = 0.15, 
                    window.units = 50, failure.method = c("cash", "closer"),
                    maxall.tol = tol - 0.05,
                    initial = 10000)
}
\arguments{
  \item{tickers}{
Character vector specifying 2 ticker symbols.
}
  \item{intercepts}{
Numeric vector of values to add to daily gains for each ticker. For example, if you want to simulate a 1\% annual expense ratio for only the second fund, you would set intercepts to c(0, convert.rate(-0.01, units.in = 252, units.out = 1)).
}
  \item{slopes}{
Numeric vector of values to multiply daily gains for each ticker by. For example, if you want to simulate a 2x leveraged version of the first fund, you would set slopes to c(2, 1). The slopes are multiplied prior to adding the intercepts.
}
  \item{...}{
Arguments to pass along with tickers, intercepts, and slopes to \code{\link{load.gains}} function, if you wish to load data on the fly rather than specify tickers.gains.
}
  \item{benchmark.ticker}{
Character vector indicating ticker symbol for benchmark index that beta should be calculated from. If unspecified, the first fund is used as the benchmark.
}
  \item{reference.tickers}{
Character string or vector of ticker symbols to include for comparative purposes.
}
  \item{tickers.gains}{
Numeric matrix of gains (daily or otherwise) for the two funds.
}
  \item{benchmark.gains}{
Numeric vector or matrix of gains for the benchmark index that beta should be calculated from. If unspecified, the first fund is used as the benchmark.
}
  \item{reference.gains}{
Numeric vector or matrix of gains for reference funds.
}
  \item{target.beta}{
Numeric value indicating what portfolio beta you want to achieve.
}
  \item{tol}{
Numeric value indicating how far the effective portfolio beta has to deviate from target.beta to trigger a rebalancing trade.
}
  \item{window.units}{
Numeric value indicating the width of the trailing moving window used to estimate each fund's beta.
}
  \item{failure.method}{
Character string or vector specifying method(s) to use when fund betas are such that the target portfolio beta cannot be achieved. Possible values are "cash", "fund1", "fund2", "inverse1", "inverse2", and "closer". See Details.
}
  \item{maxall.tol}{
Numeric value indicating tolerance to use when implementing the "fund1.maxall" or "fund2.maxall" failure method. To illustrate, if target.beta is 0, fund 1 has a current beta of 1, fund 2 has a current beta of 0.25, failure.method is "fund2.maxall", and maxall.tol is 0.1, a trade will be triggered that results in 40\% fund 2 and 60\% cash. The portfolio beta is 0.4 * 0.25 = 0.1. The reason you might want maxall.tol to be less than tol is to avoid frequently triggering another trade on the very next day, as fund 2's beta changes a little and moves the portfolio beta outside of [target.beta - tol, target.beta + tol].
}
  \item{initial}{
Numeric value giving the initial value of the portfolio.
}
}
\details{
If tickers input is specified, it gets passed to \code{\link{load.gains}} to load historical prices from Yahoo! Finance using the quantmod package [1]. If tickers.gains is specified, gains from that matrix are used.

The general implementation is as follows. Beta for each of the two funds is estimated based on the first window.units gains. Initial allocations are selected to achieve portfolio beta of target.beta. If that is not possible - for example, if target.beta is 0, and both funds have positive beta - then the action taken depends on what method is selected through the failure.method input (details below).

Assuming the target beta is attainable, the function moves over 1 day, and applies each fund's gains for that day. It then re-calculates each fund's beta based on the window.units-width interval, and determines the effective portfolio beta based on fund allocations and betas. If the effective beta is outside of [target.beta - tol, target.beta + tol], a rebalancing trade is triggered. As before, if the target beta cannot be achieved, certain actions are taken depending on the selected method.

When outside of a trade because the target beta could not be achieved, the function attempts to rebalance each time it shifts over to a new day, regardless of the effective portfolio beta.

When failure.method is set to "cash", the entire portfolio balance is allocated to cash when the target beta cannot be achieved. 

When failure.method is set to "fund1" (or "fund2"), the entire portfolio balance is allocated to the first (or second) fund when the target beta cannot be achieved. 

When failure.method is set to "fund1.maxall" (or "fund2.maxall"), when the target beta cannot be achieved, fund 1 (or fund 2) is combined with cash, with the fund 1 (fund 2) allocation as high as possible while staying within maxall.tol of target.beta.

When failure.method is set to "inverse1" (or "inverse2"), an inverse version of the first (or second) fund is used when the target beta cannot be achieved. In many cases where the target beta cannot be achieved with the two funds, it can be achieved with an inverse version of one and the other. If the target beta still cannot be achieved, the entire portfolio balance is allocated to cash.

When failure.method is set to "closer", the entire portfolio balance is allocated to whichever fund has a beta closer to target.beta. 
}
\value{
For each method, a 4-element list of the following: (1) Numeric matrix named "fund.balances" giving fund balances over time; (2) Numeric matrix named "fund.betas" giving fund betas over time; (3) Numeric vector named "effective.betas" giving effective portfolio beta over time; and (4) Numeric value named "trades" giving the total number of trades that were executed. If multiple methods are requested, a list of lists is returned.
}
\note{
NA
}
\references{
1. Jeffrey A. Ryan (2016). quantmod: Quantitative Financial Modelling Framework. R package version 0.4-6, \url{https://cran.r-project.org/package=quantmod}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\examples{
NA
}