\name{ratios}
\alias{ratios}
\title{
Ratios of Subsequent Elements in a Numeric Vector
}
\description{
Calculates vector of ratios of a numeric vector, i.e. ratio of \code{x[2]} to
\code{x[1]}, ratio of \code{x[3]} to \code{x[2]}, and so forth.
}
\usage{
ratios(x)
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
\note{
This function uses C++ code to achieve a 1.5-2 times speed increase compared to
the base R code:
\code{len <- length(prices); prices[2: len] / prices[1: (len - 1)]}.
}
\seealso{
\code{\link{diffs}}, \code{\link{pdiffs}}, \code{\link{pchanges}}
}
\examples{
# Randomly generate 10 values from a standard normal distribution
x <- rnorm(10)

# Calculate vector of ratios
y <- ratios(x)

# View results
x
y
}
