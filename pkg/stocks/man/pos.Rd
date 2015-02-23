\name{pos}
\alias{pos}
\title{
Return Positive Elements of a Numeric Vector
}
\description{
Simply returns the positive (or non-negative) elements of some input vector x. Defined as x[which(x > 0)] or x[which(x >= 0)] depending on include.zero input.
}
\usage{
pos(x, include.zero = FALSE)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{include.zero}{
If TRUE, elements >= 0 are returned; if FALSE, elements > 0 are returned.
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
\code{\link{neg}}, \code{\link{nonpos}}, \code{\link{nonneg}}
}
\examples{
# Generate 10 values from standard normal distribution
x <- rnorm(10)

# Find positive values
pos(x)
}