\name{pdiffs}
\alias{pdiffs}
\title{
Lagged Proportion/Percent Differences
}
\description{
Calculates proportion/percent differences between subsequent (or lagged)
elements of a numeric vector.
}
\usage{
pdiffs(x, lag = 1, percent = FALSE)
}
\arguments{
  \item{x}{
Numeric vector.
}
  \item{lag}{
Controls spacing between proportion/percent differences. For example, lag of 1
means you want to calculate differences between elements 1 and 2, 2 and 3, 3 and
4, and so on; a lag of 2 means you want calculate differences between elements
1 and 3, 2 and 4, 3 and 5, and so on.
}
  \item{percent}{
\code{TRUE} for percent differences, \code{FALSE} for proportion differences.
Percent differences are just proportion differences multiplied by 100.
}
}
\details{
Each proportion/percent difference is based on two numbers, say \code{x1} and
\code{x2}. The proportion difference is defined as
\code{(x1 - x2) / mean(x1, x2)}, and the percent difference is the same quantity
multiplied by \code{100}. Notice that this is NOT proportion/percent change,
which divides by the initial value \code{x1} rather than the average of
\code{x1} and \code{x2}. See \code{\link{pchanges}} for the proportion/percent
change operation.
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
This function uses C++ code to achieve a 3-4 times speed increase compared to
the base R code: \code{len <- length(x); p1 <- x[2: len]; p2 <- x[1: (len - 1)];
(p1 - p2) / (0.5 * (p1 + p2))}.
}
\seealso{
\code{\link{diffs}}, \code{\link{pchanges}}, \code{\link{ratios}}
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
