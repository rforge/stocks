\name{metrics}
\alias{metrics}
\title{
Calculate a Variety of Performance Metrics
}
\description{
Calculates any or all of the following: mean and standard deviation of gains; total growth and compound annualized growth rate; maximum drawdown; Sharpe ratio and Sortino ratio; alpha, beta, Pearson correlation, and Spearman correlation, using first investment as the benchmark; and Pearson and Spearman autocorrelation, defined as the correlation between subsequent gains for each investment.
}
\usage{
metrics(tickers = NULL, ...
        gains = NULL, prices = NULL,
        perf.metrics = c("mean", "sd", "growth", "cagr", "mdd", "sharpe",
                         "sortino", "alpha", "beta", "r.squared", "pearson",
                         "spearman", "auto.pearson", "auto.spearman"))
}
\arguments{
  \item{tickers}{
Character vector of ticker symbols.  
}
  \item{...}{
Arguments to pass along with tickers to \code{\link{load.gains}} function, if you wish to load data on the fly rather than specify gains or prices.
}
  \item{gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a particular investment.
}
  \item{prices}{
Numeric matrix of prices (daily or otherwise), where each column has prices for a particular investment.
}
  \item{perf.metrics}{
Character vector indicating what metrics should be calculated. If it includes "alpha", "beta", "r.squared", "pearson", or "spearman", the first investment is used as the benchmark.
}
}
\details{
If tickers input is specified, it gets passed to \code{\link{load.gains}} to load historical prices from Yahoo! Finance using the quantmod package [1]. If gains or prices are specified, performance metrics are calculated directly from that information.
}
\value{
A list containing a data frame with the performance metrics and a correlation matrix for gains for the various investments.
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
# Calculate performance metrics for leveraged ETFs SSO and UPRO, using SPY 
# as benchmark for alpha and beta - "on the fly" method
metrics1 <- metrics(tickers = c("SPY", "SSO", "UPRO"))

# Calculate same performance metrics, but specify gains input
gains <- load.gains(tickers = c("SPY", "SSO", "UPRO"))
metrics2 <- metrics(gains = gains)

# Results are identical
all(metrics1$perf.metrics == metrics2$perf.metrics)
}