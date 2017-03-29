\name{metrics}
\alias{metrics}
\title{
Calculate a Variety of Performance Metrics
}
\description{
Calculates any or all of the following: mean and standard deviation of gains; total growth and compound annualized growth rate; maximum drawdown; Sharpe ratio and Sortino ratio; alpha, beta, Pearson correlation, and Spearman correlation, using first investment as the benchmark; and Pearson and Spearman autocorrelation, defined as the correlation between subsequent gains for each investment.
}
\usage{
metrics(tickers = NULL, intercepts = NULL, slopes = NULL,
        gains = NULL, prices = NULL, 
        from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
        earliest.subset = TRUE,
        perf.metrics = c("mean", "sd", "growth", "cagr", "mdd", "sharpe",
                         "sortino", "alpha", "beta", "r.squared", "pearson",
                         "spearman", "auto.pearson", "auto.spearman"))
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
  \item{time.scale}{
Character string that controls the time frame for gains. Possible values are "daily", "monthly", and "yearly".
}
  \item{earliest.subset}{
If TRUE, only the subset of tickers with data going back to "from" date (or the earliest) are included; if FALSE, all tickers are included, and data covering their mutual lifetimes is used.
}
  \item{perf.metrics}{
Character vector indicating what metrics should be calculated. If it includes "alpha", "beta", "r.squared", "pearson", or "spearman", the first investment is used as the benchmark.
}
}
\details{
If tickers input is specified, function loads historical prices for the tickers from Yahoo! Finance using the quantmod package [1]. If gains or prices are specified, performance metrics are calculated directly from that information.
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
# Calculate performance metrics for leveraged ETFs SSO and UPRO, 
# using SPY as benchmark for alpha and beta
metrics1 <- perf.metrics(tickers = c("SPY", "SSO", "UPRO"))
}