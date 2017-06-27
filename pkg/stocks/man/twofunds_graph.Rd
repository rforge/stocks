\name{twofunds.graph}
\alias{twofunds.graph}
\title{
Graph One Performance Metric vs. Another for Two-Fund Portfolio(s) with Varying Allocations
}
\description{
Creates graph of one performance metric vs. another as allocation for two-fund portfolio(s) ranges from 0\% to 100\%. A common application would be plotting mean vs. standard deviation of daily or monthly investment gains.
}
\usage{
twofunds.graph(tickers = NULL, intercepts = NULL, slopes = NULL, 
               ..., 
               benchmark.tickers = NULL, 
               reference.tickers = NULL,
               tickers.gains = NULL, 
               benchmark.gains = NULL, 
               reference.gains = NULL,
               step.data = 0.0025, 
               step.points = 0.1,
               x.metric = "sd", 
               y.metric = "mean",
               tickerlabel.offsets = NULL,
               reflabel.offsets = NULL,
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
Character vector or matrix of ticker symbols. If a vector, first and second elements are assumed to be first pair, third and fourth are assumed to be the second pair, and so on. If a matrix, should have two rows, and each column should represent a different pair.
}
  \item{intercepts}{
Numeric vector or matrix of values to add to daily gains for each ticker. For example, if you have two tickers and want to simulate a 1\% annual expense ratio for only the second fund, you would set \code{intercepts = c(0, convert.rate(-0.01, units.in = 252, units.out = 1))}. Like the \code{tickers} input, can be an even length vector or a two-row matrix.
}
  \item{slopes}{
Numeric vector or matrix to multiply daily gains for each ticker by. For example, if you have two tickers and want to simulate a 2x leveraged version of the second fund, you would set \code{slopes = c(1, 2)}. The slopes are multiplied prior to adding the intercepts. Like the \code{tickers} input, can be an even length vector or a two-row matrix.
}
  \item{...}{
Arguments to pass along with \code{tickers}, \code{intercepts}, and \code{slopes} to \code{\link{load.gains}} function, if you wish to load data on the fly rather than specify \code{tickers.gains}.
}
  \item{benchmark.tickers}{
Character vector of length 1 or 2 indicating ticker symbols for benchmark indexes, to be used if \code{x.metric} or \code{y.metric} is set to \code{"alpha"}, \code{"beta"}, \code{"r.squared"}, \code{"pearson"}, \code{"spearman"}, or \code{"alpha2"}, \code{"beta2"}, and so on. For example, if you wish to plot correlation with SPY on the x-axis and correlation with TLT on the y-axis, you would set \code{x.metric = "pearson"}, \code{y.metric = "pearson2"}, and \code{benchmark.tickers = c("SPY", "TLT")}.
}
  \item{reference.tickers}{
Character string or vector of ticker symbols to include on the graph as a data point for comparative purposes.
}
  \item{tickers.gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a particular fund. First and second column are assumed to be first pair, third and fourth are assumed to be second pair, and so on.
}
  \item{benchmark.gains}{
Numeric vector or matrix of gains for benchmark indexes. Only relevant if either \code{x.metric} or \code{y.metric} is set to \code{"alpha"}, \code{"beta"}, \code{"r.squared"}, \code{"pearson"}, \code{"spearman"}, or \code{"alpha2"}, \code{"beta2"}, and so on. For example, if you wish to plot correlation with SPY on the x-axis and correlation with TLT on the y-axis, you would set \code{x.metric = "pearson"} and \code{y.metric = "pearson2"}, and \code{benchmark.gains} should be a 2-column matrix where the first column has SPY gains and the second column has TLT gains.
}
  \item{reference.gains}{
Numeric vector or matrix of gains for reference funds.
}
  \item{step.data}{
Allocation increments for data used to fit curves.
}
  \item{step.points}{
Allocation increments for data points plotted on top of curves. Set to \code{NULL} to suppress data points.
}
  \item{x.metric}{
Character string specifying what performance metric should be plotted on x-axis. Possible values are as follows: \code{"mean"} for mean of gains; \code{"sd"} for standard deviation of gains; \code{"growth"} for total growth; \code{"cagr"} for compound annualized growth rate; \code{"mdd"} for maximum drawdown; \code{"sharpe"} for Sharpe ratio; \code{"sortino"} for Sortino ratio; \code{"alpha"} or \code{"beta"} for alpha and beta from capital assets pricing model theory; \code{"r.squared"} for R-squared from regression on benchmark fund; \code{"pearson"} or \code{"spearman"} for Pearson or Spearman correlation with benchmark fund; \code{"alpha2"}, \code{"beta2"}, \code{"r.squared2"}, \code{"pearson2"}, or \code{"spearman2"} for same as previously described, but using the second benchmark index; \code{"auto.pearson"} or \code{"auto.spearman"} for Pearson or Spearman autocorrelation, defined as the correlation between subsequent gains; or \code{"allocation"} for allocation to first fund in each pair.
}
  \item{y.metric}{
Same as \code{x.metric}, but for y-axis variable.
}
  \item{tickerlabel.offsets}{
Either a numeric vector of length 2 giving the x-axis and y-axis offsets for ticker labels, or a 2-column matrix where each row gives the x-axis offset and y-axis offset for a particular ticker, following the same order of tickers as in the \code{tickers} matrix (going down each column, then to the next row) or across the \code{ticker.gains} matrix.
}
  \item{reflabel.offsets}{
Either a numeric vector of length 2 giving the x-axis and y-axis offsets for reference ticker labels, or a 2-column matrix where each row gives the x-axis offset and y-axis offset for a particular reference ticker, following the same order of tickers as in the \code{reference.tickers} vector or across the \code{reference.gains} matrix.
}
  \item{add.plot}{
If \code{TRUE}, data is plotted on current plot frame rather than a new one.
}
  \item{colors}{
Character string of colors for each fund pair.  
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
If \code{tickers} is specified, it gets passed to \code{\link{load.gains}} to load historical prices from Yahoo! Finance using the quantmod package [1]. If \code{tickers.gains} is specified, performance metrics are calculated directly from that matrix.
}
\value{
In addition to the graph, a list containing the following items: (1) A list named \code{portfolio.xy} where each element is a two-column matrix of x- and y-axis values for a fund pair; (2) a numeric vector named \code{means} of mean gains for each ticker; and (3) a numeric matrix named \code{corr.matrix} containing a correlation matrix for gains of each ticker.
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
\examples{
NA
}