\name{ticker.dates}
\alias{ticker.dates}
\title{
Get Yahoo! Finance Start/End Dates for One or More Tickers
}
\description{
This function just returns the start/end dates of tickers on Yahoo! Finance, so you can see what range of dates are available for one or more funds.
}
\usage{
ticker.dates(tickers, from = "1950-01-01", to = Sys.Date())
}
\arguments{
  \item{tickers}{
Character string or vector of character strings with ticker symbols that Yahoo! Finance recognizes.
}
  \item{from}{
Date or character string in form of date (e.g. \code{"1950-01-03"} for January 3, 1950), indicating the starting date for the period of interest.
}
  \item{to}{
Date or character string in form of date (e.g. \code{"2015-03-09"} for March 9, 2015), indicating the ending date for the period of interest.
}
}
\value{
Data frame showing start date, end date, and number of trading days for each ticker.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\examples{
# See what dates are available for Apple and Amazon
#ticker.dates(c("AAPL", "AMZN"))
}