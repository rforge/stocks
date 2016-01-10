\name{capm.daily}
\alias{capm.daily}
\title{
Calculate Capital Assets Pricing Model (CAPM) Metrics Based on Daily Gains
}
\description{
Calculate CAPM metrics for a stock relative to an index.
}
\usage{
capm.daily(tickers, index = "SPY", from = NULL, to = NULL, weights = NULL,
           weights.popmoments = TRUE, align.all = TRUE, decimals = getOption("digits"), 
           plot.characteristic = TRUE)
}
\arguments{
  \item{tickers}{
Character string or vector of ticker symbols.
}
  \item{index}{
Character string indicating ticker symbol for the index that the stock is being compared to.
}
  \item{from}{
Date or character string in form of date (e.g. '1950-01-03' for January 3, 1950), indicating the starting date for the period of interest.
}
  \item{to}{
Date or character string in form of date (e.g. '2015-03-09' for March 9, 2015), indicating the ending date for the period of interest.
}
  \item{weights}{
Vector of numeric weights.
}
  \item{weights.popmoments}{
If TRUE, optimal weights are based on estimated population moments.
}
  \item{align.all}{
If TRUE, all dates are aligned.
}
  \item{decimals}{
Whole number indicating the number of decimal places you would like the CAPM metrics rounded to.
}
  \item{plot.characteristic}{
If TRUE, function plots characteristic lines for individual funds and optimally weighted portfolio.
}
}
\value{
List where first element is list of trading dates, second element is matrix of growth of \$100, third element is data frame of performance metrics, fourth element is list of characteristic line regression fits, fifth element is covariance matrix, and sixth element is vector of optimal weights.
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