\name{neg}
\alias{neg}
\title{
Return Negative Elements of a Numeric Vector
}
\description{
Simply returns the negative (or non-positive) elements of some input vector
\code{x}. Defined as \code{x[which(x < 0)]} or \code{x[which(x <= 0)]} depending
on \code{include.zero} input.
}
\usage{
neg(x, include.zero = FALSE)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{include.zero}{
If \code{TRUE}, elements <= 0 are returned; if \code{FALSE}, elements < 0 are
returned.
}
}
\value{
Numeric vector.
}
\references{
Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{pos}}, \code{\link{nonpos}}, \code{\link{nonneg}}
}
\examples{
# Generate 10 values from standard normal distribution
set.seed(123)
x <- rnorm(10)

# Find negative values
neg(x)
}
