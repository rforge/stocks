\name{vanguard.etfs}
\alias{vanguard.etfs}
\docType{data}
\title{
List of Vanguard ETFs and their inception dates, current as of March 20, 2015.
}
\description{
Just a character vector of ticker symbols for Vanguard ETFs.
}
\usage{data("vanguard.etfs")}
\format{
  The format is:
 chr "vanguard.etfs"
}
\source{
\url{https://personal.vanguard.com/us/funds/etf/ETFTableView}
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\examples{
# Load and print Vanguard ETFs
data(vanguard.etfs)
vanguard.etfs
}
\keyword{datasets}