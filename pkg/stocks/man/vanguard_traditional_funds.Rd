\name{vanguard.traditional.funds}
\alias{vanguard.traditional.funds}
\docType{data}
\title{
List of Vanguard Traditional Mutual Funds as of March 6, 2017.
}
\description{
Simply a character vector of ticker symbols for Vanguard mutual funds.
}
\usage{data("vanguard.traditional.funds")}
\format{
  A character vector with 6 tickers.
}
\source{
\url{https://investor.vanguard.com/mutual-funds/list#/mutual-funds/asset-class/month-end-returns}
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\examples{
# Load and print list of mutual funds
data(vanguard.traditional.funds)
vanguard.traditional.funds
}
\keyword{datasets}