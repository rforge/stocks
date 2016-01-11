\name{twofund.portfolio}
\alias{twofund.portfolio}
\title{
Analyze Historical Performance of Two-Fund Portfolio as a Function of Asset Allocation
}
\description{
Calculate various measures of historical performance for a two-fund portfolio with all possible asset allocations.
}
\usage{
twofund.portfolio(tickers, index.ticker = "SPY", from = NULL, to = NULL,
                  reference.tickers = "SPY", plots = TRUE)
}
\arguments{
  \item{tickers}{
Character vector of two ticker symbols.
}
  \item{index.ticker}{
Character string indicating ticker symbol for the index that the stock is being compared to.
}
  \item{from}{
Date or character string in form of date (e.g. '2015-01-01' for Jan. 1, 2015), indicating the starting date for the period of interest. Default is 1 year prior to current date.
}
  \item{to}{
Date or character string in form of date (e.g. '2016-01-01' for Jan. 1, 2016), indicating the ending date for the period of interest. Default is current date.
}
  \item{reference.tickers}{
Character string or vector indicating ticker symbols of reference funds to compare results against.
}
  \item{plots}{
If TRUE, function returns three plots in addition to other results (compound annual growth rate vs. maximum drawdown; mean vs. standard deviation of daily gains; and Sharpe ratio vs. standard deviation of daily gains).
}
}
\value{
Depending on the function inputs, some combination of a data frame with performance statistics for the two-fund portfolio with various allocations, a data frame with performance statistics for the reference.tickers funds, and three plots. If more than one object, they are returned as a list.
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