\name{vanguard.bond.etfs}
\alias{vanguard.bond.etfs}
\docType{data}
\title{
List of Vanguard Bond ETFs as of March 6, 2017.
}
\description{
Simply a character vector of ticker symbols for Vanguard ETFs.
}
\usage{data("vanguard.bond.etfs")}
\format{
  A character vector with 14 tickers.
}
\source{
\url{https://investor.vanguard.com/etf/list?assetclass=bond#/etf/asset-class/month-end-returns}
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\examples{
# Load and print list of ETFs
data(vanguard.bond.etfs)
vanguard.bond.etfs
}
\keyword{datasets}