\name{ratios}
\alias{ratios}
\title{
Ratios of Subsequent Elements in a Numeric Vector
}
\description{
Calculates vector of ratios of a numeric vector, i.e. ratio of x[2] to x[1], ratio of x[3] to x[2], and so forth. 
}
\usage{
ratios(x)
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
NA
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