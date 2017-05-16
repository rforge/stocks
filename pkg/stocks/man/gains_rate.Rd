\name{gains.rate}
\alias{gains.rate}
\title{
Calculate Growth Rate From a Vector of Investment Gains
}
\description{
The formula is simply: \code{overall.rate = prod(gains + 1) - 1}. If \code{units.rate} is specified, then it converts to x-unit growth rate.
}
\usage{
gains.rate(gains, units.rate = NULL, nas = FALSE)
}
\arguments{
  \item{gains}{
Numeric vector of investment gains.
}
  \item{units.rate}{
Number of units for growth rate calculation. If unspecified, function returns total growth. To get annualized growth rate, set to 252 if \code{gains} has daily gains, 12 if \code{gains} has monthly gains, etc.
}
  \item{nas}{
If \code{TRUE}, function finds and removes any missing values (NAs) in \code{prices} vector. The default is \code{FALSE} to maximize speed for the usual case where there are no missing values.
}
}
\value{
Numeric value indicating the growth rate.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{prices.rate}}
}
\examples{
# Create vector of daily gains for a hypothetical stock
daily.gains <- c(-0.02, -0.01, 0.01, 0.02, 0.01)

# Overall growth is 0.95\%
gains.rate(daily.gains)

# Average daily growth is 0.19\%
gains.rate(daily.gains, 1)

# Corresponds to 61.0\% annual growth
gains.rate(daily.gains, 252)
}