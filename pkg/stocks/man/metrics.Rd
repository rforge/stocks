\name{metrics}
\alias{metrics}
\title{
Calculate a Variety of Performance Metrics
}
\description{
Calculates total growth, compound annualized growth rate, maximum drawdown, Sharpe ratio, Sortino ratio, and alpha and beta if a benchmark is specified. 
}
\usage{
metrics(tickers = NULL, intercepts = NULL, slopes = NULL,
        gains = NULL, prices = NULL, 
        from = "1900-01-01", to = Sys.Date(),
        benchmark.fund = NULL, time.scale = "daily")
}
\arguments{
  \item{tickers}{
Character string or vector of ticker symbols.
}
  \item{intercepts}{
Numeric vector of values to add to daily gains for each ticker. For example, if you have two tickers and want to simulate a 1\% annual expense ratio for only the second fund, you would set intercepts to c(0, convert.rate(-0.01, units.in = 252, units.out = 1)).
}
  \item{slopes}{
Numeric vector of values to multiply daily gains for each ticker by. For example, if you have two tickers and want to simulate a 2x leveraged version of the second fund, you would set slopes to c(1, 2). The slopes are multiplied prior to adding the intercepts.
}
  \item{gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a particular investment.
}
  \item{prices}{
Numeric matrix of prices (daily or otherwise), where each column has prices for a particular investment.
}
  \item{from}{
Date or character string in form of date (e.g. "2015-01-01" for Jan. 1, 2015), indicating the starting date for the period of interest.
}
  \item{to}{
Date or character string in form of date (e.g. "2016-01-01" for Jan. 1, 2016), indicating the ending date for the period of interest.
}
  \item{benchmark.fund}{
Numeric value or character string indicating which of the tickers should serve as the benchmark fund for calculating alpha and beta. If not specified, alpha and beta are not calculated.
}
  \item{time.scale}{
Controls time frame for gains. Possible values are "daily", "monthly", and "yearly".
}
}
\details{
If tickers input is specified, function loads historical prices for the tickers from Yahoo! Finance using the quantmod package [1]. If gains or prices are specified, performance metrics are calculated directly from that information.
}
\value{
A numeric matrix where each column indicates the daily/monthly/yearly gains for a given ticker over the time period of interest.
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
\seealso{
\code{\link{gains.rate}}, \code{\link{prices.rate}}, \code{\link{sharpe.ratio}}, \code{\link{sortino.ratio}}, \code{\link{rrr}}
}
\examples{
# Calculate performance metrics for leveraged ETFs SSO and UPRO, 
# using SPY as benchmark for alpha and beta
perf.metrics <- metrics(tickers = c("SSO", "UPRO", "SPY"), 
                        benchmark.fund = "SPY")
}