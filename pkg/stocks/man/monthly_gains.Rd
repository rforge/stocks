\name{monthly.gains}
\alias{monthly.gains}
\title{
Calculate Monthly Gains for One or More Tickers
}
\description{
Calculate monthly gains for one or more ticker symbols. User can enter names of tickers or a list of objects returned from the getSymbols function in the quantmod package.
}
\usage{
monthly.gains(tickers = NULL, quantmod.list = NULL, from = NULL, to = NULL, 
              decimals = getOption("digits"), partialmonth.min = 10)
}
\arguments{
  \item{tickers}{
Character string or vector of character string with ticker symbols that Yahoo! Finance recognizes.
}
  \item{quantmod.list}{
List of objects returned from getSymbols function in the quantmod package [1].
}
  \item{from}{
Date or character string in form of date (e.g. '1950-01-03' for January 3, 1950), indicating the starting date for the period of interest. Default is 5 years prior to the 'to' date.
}
  \item{to}{
Date or character string in form of date (e.g. '2015-03-09' for March 9, 2015), indicating the ending date for the period of interest. Default is current date.
}
  \item{decimals}{
Whole number indicating the number of decimal places you would like numbers rounded to.
}
  \item{partialmonth.min}{
Minimum number of trading days for a month to be included in the analysis. Mainly used to control whether partial month at beginning and end of time period is included.
}
}
\value{
Data frame where each row shows the year, month, number of trading days, total growth for each ticker, compound annual growth rate (CAGR) for each ticker, and compound daily growth rate (CADR) for each ticker.
}
\references{
1. Jeffrey A. Ryan (2014). quantmod: Quantitative Financial Modelling Framework. R package version 0.4-2/r613. http://R-Forge.R-project.org/projects/quantmod/.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
This function prints warnings which are typically nothing to worry about.
}
\seealso{
\code{\link{yearly.gains}}
}
\examples{
# Get monthly gains for UPRO and the S&P 500 over the past 5 years
upro.sp500.gains <- monthly.gains(tickers = c("UPRO", "^GSPC"))
}