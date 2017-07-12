\name{yearly.daily}
\alias{yearly.daily}
\title{
Convert X-year Gain to Average Daily Gain
}
\description{
Converts a proportion gain over one or several years to an average daily gain.
For example, an investment that gains 15\% over 3 years has an average daily
gain of approximately 0.0186\%. The formula is:
\code{daily.gain = (total.gain + 1) ^ (1 / (252 * years)) - 1}. Here
\code{total.gain} and \code{daily.gain} are proportions, e.g. 0.15 for 15\%.
}
\usage{
yearly.daily(total.gain, years = 1)
}
\arguments{
  \item{total.gain}{
Numeric value (or vector) representing the proportion gain of an investment over
the number of years specified.
}
  \item{years}{
Number of years.
}
}
\value{
Numeric value indicating average daily gain.
}
\references{
Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{daily.yearly}}, \code{\link{convert.rate}}
}
\examples{
# Calculate average daily gain corresponding to a 15\% gain over a 3-year period
yearly.daily(total.gain = 0.15, years = 3)

# Figure out daily gain required for various gains over a 5-year period
yearly.daily(total.gain = seq(0, 2, 0.25), years = 5)
}
