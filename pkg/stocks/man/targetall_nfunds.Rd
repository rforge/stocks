\name{targetall.nfunds}
\alias{targetall.nfunds}
\title{
Implement a Fixed-Allocation Trading Strategy Using N Funds
}
\description{
Implements a trading strategy aimed at maintaining a fixed allocation to each of
several component funds, rebalancing when the effective allocations deviate too
far from the targets.
}
\usage{
targetall.nfunds(tickers = NULL,
                 intercepts = NULL, slopes = NULL, ...,
                 tickers.gains = NULL,
                 target.alls = NULL,
                 tol = 0.05,
                 rebalance.cost = 0,
                 initial = 10000)
}
\arguments{
  \item{tickers}{
Character vector specifying 2 or more ticker symbols.
}
  \item{intercepts}{
Numeric vector of values to add to daily gains for each ticker. For example, if
you want to simulate a 1\% annual expense ratio for only the second fund, you
would set \code{intercepts = c(0, convert.rate(-0.01, units.in = 252,
units.out = 1))}.
}
  \item{slopes}{
Numeric vector of values to multiply daily gains for each ticker by. For
example, if you want to simulate a 2x leveraged version of the first fund, you
would set \code{slopes = c(2, 1)}. The slopes are multiplied prior to adding the
intercepts.
}
  \item{...}{
Arguments to pass along with \code{tickers}, \code{intercepts}, and
\code{slopes} to \code{\link{load.gains}} function, if you wish to load data on
the fly rather than specify \code{tickers.gains}.
}
  \item{tickers.gains}{
Numeric matrix of gains (daily or otherwise) for 2 or more funds.
}
  \item{target.alls}{
Numeric value indicating target allocations to each fund. If unspecified, equal
allocations are used (e.g. if there are 3 funds, allocations are 1/3, 1/3, 1/3).
}
  \item{tol}{
Numeric value indicating how far the effective allocations can drift away from
the targets before a rebalancing trade is triggered.
}
  \item{rebalance.cost}{
Numeric value indicating how much each rebalancing trade costs. For example, if
each trade costs $7 and you're looking at a 3-fund strategy, you should set
\code{rebalance.cost = 21} since rebalancing requires three trades.
}
  \item{initial}{
Numeric value giving the initial value of the portfolio.
}
}
\details{
If \code{tickers} is specified, it gets passed to \code{\link{load.gains}} to
load historical prices from Yahoo! Finance using the \bold{quantmod} package
[1]. If \code{tickers.gains} is specified, gains from that matrix are used.
}
\value{
A 2-element list containing the following:

1. Numeric matrix named \code{fund.balances} giving the balance for each fund
and the portfolio over time. \cr
2. Numeric value named \code{rebalance.count} giving the number of rebalancing
trades that were executed.
}
\note{
NA
}
\references{
1. Jeffrey A. Ryan (2016). quantmod: Quantitative Financial Modelling Framework.
R package version 0.4-6, \url{https://cran.r-project.org/package=quantmod}.

Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\examples{
NA
}
