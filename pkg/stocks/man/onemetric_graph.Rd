\name{onemetric.graph}
\alias{onemetric.graph}
\title{
Graph Performance Metric
}
\description{
Create plot comparing a performance metric across several investments.
}
\usage{
onemetric.graph(tickers = NULL, ...,
                gains = NULL,
                prices = NULL,
                y.metric = "cagr",
                add.plot = FALSE,
                sort.tickers = TRUE,
                plot.list = NULL,
                points.list = NULL,
                axis.list = NULL,
                pdf.list = NULL,
                bmp.list = NULL,
                jpeg.list = NULL,
                png.list = NULL,
                tiff.list = NULL)
}
\arguments{
  \item{tickers}{
Character vector of ticker symbols. If \code{y.metric} requires a benchmark
index (i.e. is \code{"alpha"}, \code{"beta"}, \code{"r.squared"},
\code{"pearson"}, or \code{"spearman"}), the first ticker is taken to be the
benchmark.
}
  \item{...}{
Arguments to pass along with \code{tickers} to \code{\link{load.gains}}
function, if you wish to load data on the fly rather than specify \code{gains}
or \code{prices}.
}
  \item{gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a
particular investment. If \code{y.metric} requires a benchmark index, the first
column should be gains for the benchmark.
}
  \item{prices}{
Numeric matrix of prices (daily or otherwise), where each column has prices for
a particular investment. If \code{y.metric} requires a benchmark index, the
first column should be prices for the benchmark.
}
  \item{y.metric}{
Character string specifying what performance metric should be plotted. Possible
values are as follows: \code{"mean"} for mean of gains; \code{"sd"} for standard
deviation of gains; \code{"growth"} for total growth; \code{"cagr"} for compound
annualized growth rate; \code{"mdd"} for maximum drawdown; \code{"sharpe"} for
Sharpe ratio; \code{"sortino"} for Sortino ratio; \code{"alpha"} or
\code{"beta"} for alpha and beta from capital assets pricing model theory;
\code{"r.squared"} for R-squared from regression on benchmark fund;
\code{"pearson"} or \code{"spearman"} for Pearson or Spearman correlation with
benchmark fund; or \code{"auto.pearson"} or \code{"auto.spearman"} for Pearson
or Spearman autocorrelation, defined as the correlation between subsequent
gains.
}
  \item{add.plot}{
If \code{TRUE}, data is plotted on current plot frame rather than a new one.
}
  \item{sort.tickers}{
If \code{TRUE}, tickers are arranged in decreasing order of the performance
metric.
}
  \item{plot.list}{
Optional list of inputs to pass to \code{\link{plot}} function.
}
  \item{points.list}{
Optional list of inputs to pass to \code{\link{points}} function.
}
  \item{axis.list}{
Optional list of inputs to pass to \code{\link{axis}} function.
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
If \code{tickers} is specified, it gets passed to \code{\link{load.gains}} to
load historical prices from Yahoo! Finance using the \bold{quantmod} package
[1]. If \code{gains} or \code{prices} is specified, the performance metric is
calculated directly from that information.
}
\value{
In addition to the graph, a data frame containing the tickers and the
performance metric for each ticker.
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
\seealso{
\code{\link{onemetric.overtime.graph}}, \code{\link{twometrics.graph}}
}
\examples{
NA
}
