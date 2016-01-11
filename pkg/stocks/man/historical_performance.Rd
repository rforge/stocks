\name{historical.performance}
\alias{historical.performance}
\title{
Analyze Historical Performance of Individual Funds or a Portfolio of Funds
}
\description{
Calculate various measures of historical performance (total growth, compound annual growth rate, maximum drawdown, Sharpe ratio, Sortino ratio) for requested funds and portfolio of requested funds with specified weights.
}
\usage{
historical.performance(tickers, from = NULL, to = NULL, weights = NULL, 
                       reference.tickers = "SPY", plot.growth = TRUE)
}
\arguments{
  \item{tickers}{
Character string or vector of ticker symbols.
}
  \item{from}{
Date or character string in form of date (e.g. '2015-01-01' for Jan. 1, 2015), indicating the starting date for the period of interest. Default is 1 year prior to current date.
}
  \item{to}{
Date or character string in form of date (e.g. '2016-01-01' for Jan. 1, 2016), indicating the ending date for the period of interest. Default is current date.
}
  \item{weights}{
Vector of numeric weights or allocations to each fund in tickers vector. Function rebalances weights to add to 1 if they do not already.
}
  \item{reference.tickers}{
Character string or vector indicating ticker symbols of reference funds to compare results against.
}
  \item{plot.growth}{
If TRUE, function plots growth of \$10k for individual funds, portfolio of funds, and reference funds.
}
}
\value{
List where first element is matrix of performance metrics, second element is matrix of daily gains, third element is matrix of daily balances for initial \$10k investment, and fourth element is plot of growth of \$10k (if requested).
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
This function prints warnings which are typically nothing to worry about.
}
\seealso{
NA
}
\examples{
NA
}