\name{daily.yearly}
\alias{daily.yearly}
\title{
Convert Daily Gain to X-year Gain
}
\description{
Converts a daily proportion gain to a corresponding annual (or several year) gain. For example, an investment that gains 0.1\% per day would gain approximately 28.5\% in a year (252 trading days). The formula is: \code{total.gain = (1 + daily.gain) ^ (252 * years) - 1}. Here \code{total.gain} and \code{daily.gain} are proportions, e.g. 0.15 for 15\%.
}
\usage{
daily.yearly(daily.gain, years = 1)
}
\arguments{
  \item{daily.gain}{
Numeric value (or vector) representing the daily proportion gain of an investment.
}
  \item{years}{
Number of years over which you would like to calculate the corresponding total gain. For example, set to 3 if you wish to calculate the 3-year gain for an investment that has a daily gain of \code{daily.gain}.
}
}
\value{
Numeric value indicating total gain.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{yearly.daily}}, \code{\link{convert.rate}}
}
\examples{
# Calculate annual gain for an investment that gains 0.1\% per day
daily.yearly(daily.gain = 0.001)

# Calculate 5-year gains corresponding to various daily gains
daily.yearly(daily.gain = seq(0, 0.001, 0.0001), years = 5)
}