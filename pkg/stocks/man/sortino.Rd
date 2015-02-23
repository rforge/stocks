\name{sortino}
\alias{sortino}
\title{
Sortino Ratio
}
\description{
Calculate Sortino ratio based on a vector of gains (or prices). The formula is: (mean(gains) - rf) / sd(negatives(gains)). Here rf is some risk-free rate of return, gains is a vector of gains (usually daily, but could be weekly, monthly, or some other time interval), and negatives(gains) gets the subset of gains that are negative. User can specify prices or gains.
}
\usage{
sortino(prices = NULL, gains = NULL, rf = 0)
}
\arguments{
  \item{prices}{
Numeric vector of stock prices (typically daily closing prices). 
}
  \item{gains}{
Numeric vector of gains.
  }
  \item{rf}{
Risk-free rate of return hypothetically available to the investor.
}
}
\details{
NA
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
\code{\link{sharpe}}, \code{\link{rrr}}, \code{\link{mdd}}
}
\examples{
# Randomly generate daily stock gains over a 5-year period
stock.gains <- rnorm(251*5, 0.0005, 0.01)

# Calculate Sortino ratio using risk-free return of 0
sortino(stock.gains)
}