\name{sortino.ratio}
\alias{sortino.ratio}
\title{
Sortino Ratio
}
\description{
Calculate Sortino ratio based on a vector of gains or prices. The formula is: \code{(mean(gains) - rf) / sd(negatives(gains))}. Here \code{rf} is some risk-free rate of return, \code{gains} is a vector of gains (usually daily, but could be any time interval), and \code{negatives(gains)} gets the subset of gains that are negative.
}
\usage{
sortino.ratio(gains = NULL, 
              prices = NULL, 
              rf = 0, 
              nas = FALSE)
}
\arguments{
  \item{gains}{
Numeric vector of gains.
}
  \item{prices}{
Numeric vector of investment prices (typically daily closing prices). 
}
  \item{rf}{
Risk-free rate of return hypothetically available to the investor.
}
  \item{nas}{
If \code{TRUE}, function finds and removes any missing values (\code{NA}s) in \code{prices}. The default is \code{FALSE} to maximize speed for the usual case where there are no missing values.
}
}
\value{
Numeric value indicating the Sortino ratio.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
Several definitions of Sortino ratio are commonly used; this simple version may or may not be the one you prefer.
}
\seealso{
\code{\link{gains.rate}}, \code{\link{prices.rate}}, \code{\link{mdd}}, \code{\link{sharpe.ratio}}, \code{\link{rrr}}
}
\examples{
# Randomly generate daily stock gains over a 5-year period
set.seed(123)
stock.gains <- rnorm(252 * 5, 0.0005, 0.01)

# Calculate Sortino ratio using risk-free return of 0
sortino.ratio(stock.gains)
}