\name{pdiffs}
\alias{pdiffs}
\title{
Lagged Proportion/Percent Differences
}
\description{
Calculates proportion/percent differences between subsequent (or lagged) elements of a numeric vector.
}
\usage{
pdiffs(x, lag = 1, percent = FALSE)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{lag}{
Controls spacing between proportion/percent differences. For example, lag of 1 means you want to calculate differences between elements 1 and 2, 2 and 3, 3 and 4, and so on; a lag of 2 means you want calculate differences between elements 1 and 3, 2 and 4, 3 and 5, and so on.
}
  \item{percent}{
TRUE for percent differences, FALSE for proportion differences. Percent differences are just proportion differences multiplied by 100.
}
}
\details{
Each proportion/percent difference is based on two numbers, say x1 and x2. The proportion difference is defined as (x1 - x2) / mean(x1, x2), and the percent difference is the same quantity multiplied by 100. Notice that this is NOT proportion/percent change, which divides by the initial value x1 rather than the average of x1 and x2. See \code{\link{pchanges}} for the proportion/percent change operation.
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

# Calculate vector of proportion differences between subsequent values
y <- pdiffs(x)

# Calculate percent differences instead
z <- pdiffs(x, percent = TRUE)

# Compare results
x
y
z
}