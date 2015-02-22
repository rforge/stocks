\name{convert.rate}
\alias{convert.rate}
\title{
Convert Gain from One Time Interval to Another
}
\description{
This is a simple function to convert a proportion gain over one time interval to a proportion gain over a different time interval. For example, you can use this function to figure out that a stock gain of 0.08 (8\%) over 70 trading days corresponds to a 251-day or annual gain of 0.19 (19\%). 
}
\usage{
convert.rate(rate, days.in = 1, days.out = 1)
}
\arguments{
  \item{rate}{
Numeric value (or vector) representing the growth rate of a security or investment over a period of days.in days.
}
  \item{days.in}{
Number of trading days over which the security of interest had a growth rate of rate.
}
  \item{days.out}{
Number of trading days you would like to calculate a growth rate for.
}
}
\details{
NA
}
\value{
Numeric value (or vector) indicating the proportion gain over days.out days.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
NA
}
\seealso{
NA
}
\examples{
# Calculate the annual growth rate of a stock that gains 1\% per week.
convert.rate(rate = 0.01, days.in = 5, days.out = 251)

# You invest in AAPL and gain 0.5\% in 17 business days. Express as a 5-year growth rate.
convert.rate(rate = 0.005, days.in = 17, days.out = 251*5)

# Your portfolio has tripled in a 13-year period. Calculate your average annual gain.
convert.rate(rate = 2, days.in = 13*251, days.out = 251)
}