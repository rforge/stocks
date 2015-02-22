\name{nonnegatives}
\alias{nonnegatives}
\title{
Return Non-Negative Elements of a Numeric Vector
}
\description{
Simply returns the non-negative elements of some input vector x. Defined as x[which(x >= 0)].
}
\usage{
nonnegatives(x)
}
\arguments{
  \item{x}{
Numeric vector.
}
}
\details{
NA
}
\value{
Numeric vector.
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
\code{\link{positives}}, \code{\link{negatives}}, \code{\link{nonpositives}}
}
\examples{
# Generate 10 values from standard normal distribution
x <- rnorm(10)

# Find non-negative values
nonnegatives(x)
}