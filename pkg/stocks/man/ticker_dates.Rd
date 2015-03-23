\name{ticker.dates}
\alias{ticker.dates}
\title{
Get Yahoo! Finance Start/End Dates for One or More Tickers
}
\description{
This function just returns the start/end dates of tickers on Yahoo! Finance, so you can see what range of dates are available for one or more funds.
}
\usage{
ticker.dates(tickers)
}
\arguments{
  \item{tickers}{
Character string or vector of character strings with tickers that Yahoo! Finance recognizes.
}
}
\value{
Data frame showing start and end dates for each ticker.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\examples{
# See what dates are available for Apple and Amazon
ticker.dates(c("AAPL", "AMZN"))
}