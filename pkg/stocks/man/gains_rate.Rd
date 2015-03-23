\name{gains.rate}
\alias{gains.rate}
\title{
Calculate Growth Rate From a Vector of Stock or Investment Gains
}
\description{
The formula is simply: overall.rate =  prod(gains + 1) - 1. If xday is specified, then it converts to x-day growth rate. For example, if balances is a vector of stock gains over a five-day period, say c(-0.02, -0.01, 0.01, 0.02, 0.01), then the total growth rate is approximately 0.0095 over five trading days. If xday.rate is set to 1, this rate is converted to an average daily rate of approximately 0.0019. 
}
\usage{
gains.rate(gains, xday.rate = NULL, nas = FALSE)
}
\arguments{
  \item{gains}{
Numeric vector of stock or investment gains. Has to be daily gains if you want to calculate x-day rather than overall growth.
}
  \item{xday.rate}{
Number of days for growth rate calculation. If unspecified, function returns total growth. If set to 251, function would return annual growth rate.
}
  \item{nas}{
If TRUE, function finds and removes any missing values (NA's) in prices vector. The default is FALSE to maximize speed for the usual case where there are no missing values.
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

# Corresponds to 60.7\% annual growth
gains.rate(daily.gains, 251)
}