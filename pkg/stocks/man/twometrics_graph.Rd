\name{twometrics.graph}
\alias{twometrics.graph}
\title{
Graph One Performance Metric vs. Another
}
\description{
Create plot showing one performance metric vs. another for several investments.
}
\usage{
twometrics.graph(tickers = NULL, ...,
                 gains = NULL, 
                 prices = NULL,
                 x.metric = "mdd", 
                 y.metric = "cagr", 
                 tickerlabel.offsets = NULL,
                 add.plot = FALSE, 
                 colors = NULL,
                 plot.list = NULL,
                 points.list = NULL,
                 text.list = NULL,
                 pdf.list = NULL,
                 bmp.list = NULL,
                 jpeg.list = NULL,
                 png.list = NULL,
                 tiff.list = NULL)
}
\arguments{
  \item{tickers}{
Character vector of ticker symbols. If \code{x.metric} or \code{y.metric} requires one benchmark index (i.e. one or the other is \code{"alpha"}, \code{"beta"}, \code{"r.squared"}, \code{"pearson"}, or \code{"spearman"}), the first ticker is taken to be the benchmark; if \code{x.metric} and \code{y.metric} require different benchmark indexes (i.e. one is \code{"alpha"}, \code{"beta"}, \code{"r.squared"}, \code{"pearson"}, or \code{"spearman"}, and the other is \code{"alpha2"}, \code{"beta2"}, \code{"r.squared2"}, \code{"pearson2"}, or \code{"spearman2"}), the first ticker is taken to be the first benchmark, and the second ticker is taken to be the second benchmark.
}
  \item{...}{
Arguments to pass along with \code{tickers} to \code{\link{load.gains}} function, if you wish to load data on the fly rather than specify \code{gains} or \code{prices}.
}
  \item{gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a particular investment. If \code{x.metric} or \code{y.metric} requires one benchmark index (i.e. one or the other is \code{"alpha"}, \code{"beta"}, \code{"r.squared"}, \code{"pearson"}, or \code{"spearman"}), the first column is taken to be the benchmark; if \code{x.metric} and \code{y.metric} require different benchmark indexes (i.e. one is \code{"alpha"}, \code{"beta"}, \code{"r.squared"}, \code{"pearson"}, or \code{"spearman"}, and the other is \code{"alpha2"}, \code{"beta2"}, \code{"r.squared2"}, \code{"pearson2"}, or \code{"spearman2"}), the first column is taken to be the first benchmark, and the second column is taken to be the second benchmark.
}
  \item{prices}{
Numeric matrix of prices (daily or otherwise), where each column has prices for a particular investment. If \code{x.metric} or \code{y.metric} requires one benchmark index (i.e. one or the other is \code{"alpha"}, \code{"beta"}, \code{"r.squared"}, \code{"pearson"}, or \code{"spearman"}), the first column is taken to be the benchmark; if \code{x.metric} and \code{y.metric} require different benchmark indexes (i.e. one is \code{"alpha"}, \code{"beta"}, \code{"r.squared"}, \code{"pearson"}, or \code{"spearman"}, and the other is \code{"alpha2"}, \code{"beta2"}, \code{"r.squared2"}, \code{"pearson2"}, or \code{"spearman2"}), the first column is taken to be the first benchmark, and the second column is taken to be the second benchmark.
}
  \item{x.metric}{
Character string specifying what performance metric should be plotted on x-axis. Possible values are as follows: \code{"mean"} for mean of gains; \code{"sd"} for standard deviation of gains; \code{"growth"} for total growth; \code{"cagr"} for compound annualized growth rate; \code{"mdd"} for maximum drawdown; \code{"sharpe"} for Sharpe ratio; \code{"sortino"} for Sortino ratio; \code{"alpha"} or \code{"beta"} for alpha and beta from capital assets pricing model theory; \code{"r.squared"} for R-squared from regression on benchmark fund; \code{"pearson"} or \code{"spearman"} for Pearson or Spearman correlation with benchmark fund; \code{"alpha2"}, \code{"beta2"}, \code{"r.squared2"}, \code{"pearson2"}, or \code{"spearman2"} for same as previously described, but using the second benchmark index; or \code{"auto.pearson"} or \code{"auto.spearman"} for Pearson or Spearman autocorrelation, defined as the correlation between subsequent gains.
}
\item{y.metric}{
Same as \code{x.metric}, but for y-axis variable.
}
  \item{tickerlabel.offsets}{
Can be any of the following: numeric vector of length 2 giving x-axis and y-axis offsets for ticker labels on all data points; 2-column matrix where each row gives x-axis offset and y-axis offset for a particular ticker, following same order of tickers as in \code{tickers} vector; or a 3-column data frame where each row contains a ticker symbol, an x-axis offset, and a y-axis offset.
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
  \item{text.list}{
Optional list of inputs to pass to \code{\link{text}} function.
}
  \item{pdf.list}{
Optional list of inputs to pass to \code{\link{pdf}} function. If not \code{NULL}, .pdf file is created.
}
  \item{bmp.list}{
Optional list of inputs to pass to \code{\link{bmp}} function. If not \code{NULL}, .bmp file is created.
}
  \item{jpeg.list}{
Optional list of inputs to pass to \code{\link{pdf}} function. If not \code{NULL}, .jpg file is created.
}
  \item{png.list}{
Optional list of inputs to pass to \code{\link{png}} function. If not \code{NULL}, .png file is created.
}
  \item{tiff.list}{
Optional list of inputs to pass to \code{\link{tiff}} function. If not \code{NULL}, .tif file is created.
}
}
\details{
If \code{tickers} is specified, it gets passed to \code{\link{load.gains}} to load historical prices from Yahoo! Finance using the quantmod package [1]. If \code{gains} or \code{prices} is specified, the performance metrics are calculated directly from that information.
}
\value{
In addition to the graph, a data frame containing the tickers and the performance metrics for each ticker.
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
\code{\link{onemetric.graph}}, \code{\link{onemetric.overtime.graph}}
}
\examples{
NA
}