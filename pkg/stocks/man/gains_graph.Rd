\name{gains.graph}
\alias{gains.graph}
\title{
Create Scatterplot of Investment Gains
}
\description{
Creates scatterplot of gains (daily or otherwise) for one or more investments
vs. another.
}
\usage{
gains.graph(tickers = NULL, ...,
            gains = NULL,
            prices = NULL,
            orders = 1,
            add.plot = FALSE,
            colors = NULL,
            plot.list = NULL,
            points.list = NULL,
            legend.list = NULL,
            pdf.list = NULL,
            bmp.list = NULL,
            jpeg.list = NULL,
            png.list = NULL,
            tiff.list = NULL)
}
\arguments{
  \item{tickers}{
Character vector of ticker symbols. Gains for the first ticker are plotted on
the x-axis, and gains for all other tickers are plotted on the y-axis.
}
  \item{...}{
Arguments to pass along with \code{tickers} to \code{\link{load.gains}}
function, if you wish to load data on the fly rather than specify \code{gains}
or \code{prices}.
}
  \item{gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a
particular investment. Gains from the first column are plotted on the x-axis,
and gains for all other columns are plotted on the y-axis.
}
  \item{prices}{
Numeric matrix of prices (daily or otherwise), where each column has prices for
a particular investment. Gains derived from the first column are plotted on the
x-axis, and gains derived from all other columns are plotted on the y-axis.
}
  \item{orders}{
Numeric value or vector of values specifying the order of linear regression
models for each y-axis fund. Set to \code{1} for simple linear regression,
\code{2} for linear regression with first-order and second-order terms, and so
on.
}
  \item{add.plot}{
If \code{TRUE}, data is plotted on current plot frame rather than a new one.
}
  \item{colors}{
Character string of colors for each y-axis investment.
}
  \item{plot.list}{
Optional list of inputs to pass to \code{\link{plot}} function.
}
  \item{points.list}{
Optional list of inputs to pass to \code{\link{points}} function.
}
  \item{legend.list}{
Optional list of inputs to pass to \code{\link{legend}} function.
}
  \item{pdf.list}{
Optional list of inputs to pass to \code{\link{pdf}} function. If not
\code{NULL}, .pdf file is created.
}
  \item{bmp.list}{
Optional list of inputs to pass to \code{\link{bmp}} function. If not
\code{NULL}, .bmp file is created.
}
  \item{jpeg.list}{
Optional list of inputs to pass to \code{\link{jpeg}} function. If not
\code{NULL}, .jpg file is created.
}
  \item{png.list}{
Optional list of inputs to pass to \code{\link{png}} function. If not
\code{NULL}, .png file is created.
}
  \item{tiff.list}{
Optional list of inputs to pass to \code{\link{tiff}} function. If not
\code{NULL}, .tif file is created.
}
}
\details{
If \code{tickers} input is specified, it gets passed to \code{\link{load.gains}}
to load historical prices from Yahoo! Finance using the \bold{quantmod} package
[1]. If \code{gains} or \code{prices} is specified, the graph is based directly
on that information. Either way, the first fund's gains are plotted on the
x-axis, and all the others are plotted on the y-axis.
}
\value{
In addition to the graph, a list containing fitted linear regression models
returned by the \code{\link{lm}} function for each y-axis fund vs. the x-axis
fund.
}
\note{
NA
}
\references{
1. Jeffrey A. Ryan (2016). quantmod: Quantitative Financial Modelling Framework.
R package version 0.4-6, \url{https://cran.r-project.org/package=quantmod}.

Acknowledgment: This material is based upon work supported by the National
Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\examples{
NA
}
