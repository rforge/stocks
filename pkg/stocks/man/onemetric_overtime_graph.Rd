\name{onemetric.overtime.graph}
\alias{onemetric.overtime.graph}
\title{
Graph Performance Metric Over Time
}
\description{
Create plot comparing a performance metric across several investments over a period of time.
}
\usage{
onemetric.overtime.graph(tickers = NULL, ...,
                         gains = NULL, prices = NULL,
                         y.metric = "cagr", 
                         window.units = 50,
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
Character vector of ticker symbols. If \code{y.metric} requires a benchmark index, the first ticker is taken to be the benchmark.
}
  \item{...}{
Arguments to pass along with tickers to \code{\link{load.gains}} function, if you wish to load data on the fly rather than specify \code{gains} or \code{prices}.
}
  \item{gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a particular investment. If \code{y.metric} requires a benchmark index, the first column should be gains for the benchmark.
}
  \item{prices}{
Numeric matrix of prices (daily or otherwise), where each column has prices for a particular investment. If \code{y.metric} requires a benchmark index, the first column should be prices for the benchmark.
}
  \item{y.metric}{
Character string specifying what performance metric should be plotted. Possible values are as follows: \code{"mean"} for mean of gains; \code{"sd"} for standard deviation of gains; \code{"growth"} for total growth; \code{"cagr"} for compound annualized growth rate; \code{"mdd"} for maximum drawdown; \code{"sharpe"} for Sharpe ratio; \code{"sortino"} for Sortino ratio; \code{"alpha"} or \code{"beta"} for alpha and beta from capital assets pricing model theory; \code{"r.squared"} for R-squared from regression on benchmark fund; \code{"pearson"} or \code{"spearman"} for Pearson or Spearman correlation with benchmark fund; or \code{"auto.pearson"} or \code{"auto.spearman"} for Pearson or Spearman autocorrelation, defined as the correlation between subsequent gains.
}
  \item{window.units}{
Numeric value indicating the width of the moving window. For example, if \code{time.scale} is \code{"daily"} and \code{window.units} is set to \code{50}, the plot will show the performance metric for each 50-gain (51-day) interval spanning the time period of interest.
}
  \item{add.plot}{
If \code{TRUE}, data is plotted on current plot frame rather than a new one.
}
  \item{colors}{
Character string of colors for each fund.
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
Optional list of inputs to pass to \code{\link{pdf}} function. If not \code{NULL}, .pdf file is created.
}
  \item{bmp.list}{
Optional list of inputs to pass to \code{\link{bmp}} function. If not \code{NULL}, .bmp file is created.
}
  \item{jpeg.list}{
Optional list of inputs to pass to \code{\link{jpeg}} function. If not \code{NULL}, .jpg file is created.
}
  \item{png.list}{
Optional list of inputs to pass to \code{\link{png}} function. If not \code{NULL}, .png file is created.
}
  \item{tiff.list}{
Optional list of inputs to pass to \code{\link{tiff}} function. If not \code{NULL}, .tif file is created.
}
}
\details{
If \code{tickers} is specified, it gets passed to \code{\link{load.gains}} to load historical prices from Yahoo! Finance using the quantmod package [1]. If \code{gains} or \code{prices} is specified, the performance metric is calculated directly from that information.
}
\value{
In addition to the graph, a numeric matrix where each column contains the performance metric over time for a particular fund.
}
\note{
NA
}
\references{
1. Jeffrey A. Ryan (2016). quantmod: Quantitative Financial Modelling Framework. R package version 0.4-6, \url{https://cran.r-project.org/package=quantmod}.

Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\seealso{
\code{\link{onemetric.graph}}, \code{\link{twometrics.graph}}
}
\examples{
NA
}