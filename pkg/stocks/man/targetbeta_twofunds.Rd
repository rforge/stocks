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
                    benchmark.tickers = NULL, reference.tickers = NULL,
                    tickers.gains = NULL, benchmark.gains = NULL, reference.gains = NULL, 
                    target.beta = 0, tol = 0.15,
                    window.units = 50, failure.method = c("cash", "closer"),
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
  \item{benchmark.tickers}{
Character vector of length 1 or 2 indicating ticker symbols for benchmark indexes, to be used if x.metric or y.metric is set to "alpha", "beta", "r.squared", "pearson", "spearman", or "alpha2", "beta2", and so on. For example, if you wish to plot correlation with SPY on the x-axis and correlation with TLT on the y-axis, you would set x.metric to "pearson" and y.metric to "pearson2", and set benchmark.tickers to c("SPY", "TLT").
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
Character string or vector specifying which method to use when fund betas are such that the target portfolio beta cannot be achieved. Possible values are as follows: "cash", "fund1", "fund2", "inverse1", "inverse2", "closer". See Details.
}
  \item{initial}{
Numeric value indicating what the initial value of the portfolio should be.
}
}
\details{
If tickers input is specified, it gets passed to \code{\link{load.gains}} to load historical prices from Yahoo! Finance using the quantmod package [1]. If tickers.gains is specified, gains from that matrix are used.

The general implementation is as follows. Beta for each of the two funds is estimated based on the first window.units gains. Initial allocations are selected to achieve portfolio beta of target.beta. If that is not possible - for example, if target.beta is 0, and both funds have positive beta - then the action taken depends on what method is selected through the failure.method input (details below).

Assuming the target beta is attainable, the function moves over 1 day, and applies each fund's gains for that day. It then re-calculates each fund's beta based on the window.units-width interval, and determines the effective portfolio beta based on fund allocations and betas. If the effective beta is outside of [target.beta - tol, target.beta + tol], a rebalancing trade is triggered. As before, if the target beta cannot be achieved, certain actions are taken depending on the selected method.

When outside of a trade because the target beta could not be achieved, the function attempts to rebalance each time it shifts over to a new day, regardless of the effective portfolio beta.

When failure.method is set to "cash", the entire portfolio balance is allocated to cash when the target beta cannot be achieved. 

When failure.method is set to "fund1" (or "fund2"), the entire portfolio balance is allocated to the first (or second) fund when the target beta cannot be achieved. 

When failure.method is set to "inverse1" (or "inverse2"), an inverse version of the first (or second) fund is used when the target beta cannot be achieved. In many cases where the target beta cannot be achieved with the two funds, it can be achieved with an inverse version of one and the other. If the target beta still cannot be achieved, the entire portfolio balance is allocated to cash.

When failure.method is set to "closer", the entire portfolio balance is allocated to whichever fund has a beta closer to target.beta. 
}
\value{
For each method, a 4-element list of the following: (1) Matrix of fund balances over time; (2) Matrix of fund betas over time; (3) Vector of effective portfolio beta over time; (4) Number of trades. If multiple methods are requested, a list of lists is returned.
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