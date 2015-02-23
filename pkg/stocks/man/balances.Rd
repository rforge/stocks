\name{balances}
\alias{balances}
\title{
Calculate Balances Based on Initial Balance and Vector of Ratios
}
\description{
Calculates vector of balances based on initial balance and vector of ratios from one time point to the next (i.e. proportion gains + 1). This is a simple function containing this R code: initial * cumprod(ratios)
}
\usage{
balances(ratios, initial = 10000)
}
\arguments{
  \item{ratios}{
Numeric vector of ratios between subsequent stock prices (i.e. proportion gains + 1). For example, if a stock gained 3\%, lost 1\%, then lost 2\%, ratios would be c(1.03, 0.99, 0.98). 
}
  \item{initial}{
Initial balance.
}
}
\value{
Numeric vector indicating balance at each time point.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{final.balance}}
}
\examples{
# Randomly generate daily stock gains over a 5-year period
stockgains <- rnorm(251*5, 1.001, 0.02)

# Create vector of balances if initial balance is $10,000
bals <- balances(stockgains)

# Plot results
plot(bals)
}