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
NA
}
\examples{
# Randomly generate 1 million values from a Poisson(3) distribution
x <- rpois(1000000, 3)

# Calculate vector of differences between subsequent values
y <- diffs(x)
}