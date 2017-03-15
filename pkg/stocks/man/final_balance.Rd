\name{final.balance}
\alias{final.balance}
\title{
Calculate Final Balance Based on Initial Balance and Vector of Ratios
}
\description{
Calculated as: initial * prod(ratios).
}
\usage{
final.balance(ratios, initial = 10000)
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
Numeric vector indicating final balance.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{balances}}
}
\examples{
# Randomly generate daily stock gains over a 5-year period
set.seed(123)
stockgains <- rnorm(252 * 5, 1.0003, 0.02)

# Calculate final balance if initial balance was $10,000
final.balance(stockgains)
}