\name{diffs}
\alias{diffs}
\title{
Lagged Differences (Alternate Implementation)
}
\description{
Calculates differences between subsequent (or lagged) elements of a numeric vector. Very similar to base function diff, but written in C++ to run faster.
}
\usage{
diffs(x, lag = 1)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{lag}{
Controls spacing between differences. For example, lag of 1 means you want to calculate differences between elements 1 and 2, 2 and 3, 3 and 4, and so on; a lag of 2 means you want calculate differences between elements 1 and 3, 2 and 4, 3 and 5, and so on.
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
\note{
This function uses C++ code to achieve an approximate 4 times speed increase compared to the base R function diff.
}
\seealso{
\code{\link{pdiffs}}, \code{\link{pchanges}}, \code{\link{ratios}}
}
\examples{
# Randomly generate 1 million values from a Poisson(3) distribution
x <- rpois(100000, 3)

# Calculate vector of differences between subsequent values
y <- diffs(x)

# Could get same result from base R function diff
z <- diff(x)
all(y == z)

# But diffs is faster
benchmark(y = diffs(x))
benchmark(z = diff(x))

# diffs also faster than diff for 2-lag difference
x <- rnorm(100000)
y <- diffs(x, 2)
z <- diff(x, 2)
all(y == z)
benchmark(y = diffs(x))
benchmark(z = diff(x))
}