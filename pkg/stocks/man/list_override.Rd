\name{list.override}
\alias{list.override}
\title{
Add/Replace Elements of First List with Elements of Second List
}
\description{
Adds each element of \code{list2} to \code{list1}, overriding any elements of
the same name. Similar to \code{modifyList} function in \bold{utils} package,
but either list can be \code{NULL}. Useful for \code{do.call} statements, when
you want to combine a list of default inputs with a list of user-specified
inputs.
}
\usage{
list.override(list1, list2)
}
\arguments{
  \item{list1}{
Initial list that has some number of named elements. Can be \code{NULL} or an
empty list.
}
  \item{list2}{
List with named elements that will be added to \code{list1}, replacing any
elements with the same name. Can be \code{NULL} or an empty list.
}
}
\details{
NA
}
\value{
A list containing the named elements initially in \code{list1} and not in
\code{list2}, any additional named elements in \code{list2}, and any named
elements in \code{list1} that were replaced by elements of the same name in
\code{list2}.
}
\note{
NA
}
\references{
Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{modifyList}}, \code{\link{do.call}}
}
\examples{
# Create list that has default inputs to the plot function
list.defaults <- list(x = 1: 5, y = 1: 5, type = "l", lty = 1)

# Create list of user-specified inputs to the plot function
list.user <- list(main = "A Straight Line", lty = 2, lwd = 1.25)

# Combine the two lists into one, giving priority to list.user
list.combined <- list.override(list.defaults, list.user)

# Plot data using do.call
do.call(plot, list.combined)
}
