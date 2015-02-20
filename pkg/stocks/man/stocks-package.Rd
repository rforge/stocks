\name{stocks-package}
\alias{stocks-package}
\alias{stocks}
\docType{package}
\title{
Fast Functions for Stock Market Analysis
}
\description{
Provides functions for analyzing historical performance of stocks or other investments. Functions are written in C++ to quickly calculate maximum draw-down, Sharpe ratio, risk return ratio, and other commonly used metrics of stock performance.
}
\details{
\tabular{ll}{
Package: \tab stocks\cr
Type: \tab Package\cr
Version: \tab 1.1.1\cr
Date: \tab 2015-02-20\cr
License: \tab GPL-2\cr
}
The following functions are included:

\code{\link{convert.rate}}
\code{\link{diffs}}
\code{\link{pdiffs}}
\code{\link{pchanges}}
\code{\link{ratios}}
\code{\link{balances}}
\code{\link{mdd}}
}
\author{
Dane R. Van Domelen

Maintainer: Dane R. Van Domelen <vandomed@gmail.com>
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\keyword{ package }
\seealso{
NA
}
\examples{
# Your portfolio has tripled in a 13-year period - calculate your average annual gain
convert.rate(rate = 2, days.in = 13*251, days.out = 251)

# Randomly generate 10 values from a standard normal distribution
x <- rnorm(10)

# Calculate vector of differences between subsequent values
y1 <- diffs(x)

# Calculate vector of proportion differences between subsequent values
y2 <- pdiffs(x)

# Calculate vector of proportion changes between subsequent values
y3 <- pchanges(x)
}