\name{nonpos}
\alias{nonpos}
\title{
Return Non-Positive Elements of a Numeric Vector
}
\description{
Simply returns the non-positive elements of some input vector \code{x}. Defined
as \code{x[which(x <= 0)]}.
}
\usage{
nonpos(x)
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
Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{pos}}, \code{\link{neg}}, \code{\link{nonneg}}
}
\examples{
# Generate 10 values from standard normal distribution
set.seed(123)
x <- rnorm(10)

# Find non-positive values
nonpos(x)
}
