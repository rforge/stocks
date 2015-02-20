\name{pchanges}
\alias{pchanges}
\title{
Lagged Proportion/Percent Changes
}
\description{
Calculates proportion/percent changes between subsequent (or lagged) elements of a numeric vector.
}
\usage{
pchanges(x, lag = 1, percent = FALSE)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{lag}{
Controls spacing between proportion/percent changes. For example, lag of 1 means you want to calculate changes between elements 1 and 2, 2 and 3, 3 and 4, and so on; a lag of 2 means you want calculate changes between elements 1 and 3, 2 and 4, 3 and 5, and so on.
}
  \item{percent}{
TRUE for percent changes, FALSE for proportion changes. Percent changes are just proportion changes multiplied by 100.
}
}
\details{
Each proportion/percent change is based on two numbers, say x1 and x2. The proportion change is defined as (x1 - x2) / x1, and the percent change is the same quantity multiplied by 100. Notice that this is NOT proportion/percent difference, which divides by the average of x1 and x2 rather than the initial value x1. See \code{\link{pdiffs}} for the proportion/percent difference operation.
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

# Calculate vector of proportion changes between subsequent values
y <- pchanges(x)

# Calculate percent changes instead
z <- pchanges(x, percent = TRUE)

# Compare results
x
y
z
}