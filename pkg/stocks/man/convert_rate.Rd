\name{convert.rate}
\alias{convert.rate}
\title{
Convert Gain from One Time Interval to Another
}
\description{
This is a simple function to convert a proportion gain over one time interval to a proportion gain over a different time interval. For example, you can use this function to figure out that a stock gain of 0.08 (8\%) over 70 trading days corresponds to an annualized (i.e. 252-day) gain of 0.319 (31.9\%). 
}
\usage{
convert.rate(rate, units.in = 1, units.out = 1)
}
\arguments{
  \item{rate}{
Numeric value (or vector) representing the growth rate of an investment over a period of units.in time units. Units could be days, months, years, etc.
}
  \item{units.in}{
Number of time units over which the investment had a growth rate of rate.
}
  \item{units.out}{
Number of time units you would like to calculate a growth rate for.
}
}
\value{
Numeric value (or vector) indicating the proportion gain over units.out time units.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{daily.yearly}}, \code{\link{yearly.daily}}
}
\examples{
# Calculate annualized gain for an 8\% gain over a 70-day period.
convert.rate(rate = 0.08, units.in = 70, units.out = 252)

# Calculate the annual growth rate of a fund that gains 0.02\% per day.
convert.rate(rate = 0.0002, units.in = 1, units.out = 252)

# Calculate the annual growth rate of a fund that gains 1\% per week.
convert.rate(rate = 0.01, units.in = 1, units.out = 52)

# You invest in AAPL and gain 0.5\% in 17 business days. Express as a 5-year growth rate.
convert.rate(rate = 0.005, units.in = 17, units.out = 252 * 5)

# Your portfolio has tripled in a 13-year period. Calculate your average annual gain.
convert.rate(rate = 2, units.in = 13, units.out = 1)
}