\name{nonneg}
\alias{nonneg}
\title{
Return Non-Negative Elements of a Numeric Vector
}
\description{
Simply returns the non-negative elements of some input vector x. Defined as x[which(x >= 0)].
}
\usage{
nonneg(x)
}
\arguments{
  \item{x}{
Numeric vector.
}
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
\seealso{
\code{\link{pos}}, \code{\link{neg}}, \code{\link{nonpos}}
}
\examples{
# Generate 10 values from standard normal distribution
x <- rnorm(10)

# Find non-negative values
nonneg(x)
}