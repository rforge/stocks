\name{sharpe}
\alias{sharpe}
\title{
Sharpe Ratio
}
\description{
Calculate Sharpe ratio based on a vector of gains (or prices). The formula is: (mean(gains) - rf) / sd(gains). Here rf is some risk-free rate of return, and gains is a vector of gains (usually daily, but could be weekly, monthly, or some other time interval). User can specify prices or gains.
}
\usage{
sharpe(gains = NULL, prices = NULL, rf = 0)
}
\arguments{
  \item{gains}{
Numeric vector of gains.
}
  \item{prices}{
Numeric vector of stock prices (typically daily closing prices). 
}
  \item{rf}{
Risk-free rate of return hypothetically available to the investor.
}
}
\value{
Numeric value indicating the Sharpe ratio.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
Several definitions of Sharpe ratio are commonly used; this may or may not be the version you prefer.
}
\seealso{
\code{\link{sortino}}, \code{\link{rrr}}, \code{\link{mdd}}
}
\examples{
# Randomly generate daily stock gains over a 5-year period
stock.gains <- rnorm(251*5, 0.0005, 0.01)

# Calculate Sharpe ratio using risk-free return of 0
sharpe(stock.gains)
}