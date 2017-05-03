\name{threefunds.graph}
\alias{threefunds.graph}
\title{
Graph One Performance Metric vs. Another for Three-Fund Portfolio(s) with Varying Allocations
}
\description{
Creates graph of one performance metric vs. another as allocation for three-fund portfolio(s) varies. Each curve shows data for a fixed allocation to fund 1 of a set, while the remaining allocation ranges from 100\% fund 2 to 100\% fund 3.
}
\usage{
threefunds.graph(tickers = NULL, intercepts = NULL, slopes = NULL, ...,
                 benchmark.tickers = NULL, reference.tickers = NULL,
                 tickers.gains = NULL, benchmark.gains = NULL, reference.gains = NULL,
                 step.data = 0.0025, step.points = 0.1, step.curves = 0.2,
                 x.metric = "sd", y.metric = "mean",
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
Character vector or matrix of ticker symbols. If a vector, the first, second, and third elements are assumed to be the first three-fund set, the fourth through sixth are assumed to be the second set, and so on. If a matrix, should have three rows, and each column should represent a different three-fund set.
}
  \item{intercepts}{
Numeric vector or matrix of values to add to daily gains for each ticker. For example, if you have three tickers and want to simulate a 1\% annual expense ratio for only the third fund, you would set intercepts to c(0, 0, convert.rate(-0.01, units.in = 252, units.out = 1)). Like the tickers input, can be a vector divisible by three or a three-row matrix.
}
  \item{slopes}{
Numeric vector or matrix of values to multiply daily gains for each ticker by. For example, if you have three tickers and want to simulate a 2x leveraged version of the third fund, you would set slopes to c(1, 1, 2). The slopes are multiplied prior to adding the intercepts. Like the tickers input, can be a vector divisible by three or a three-row matrix.
}
  \item{...}{
Arguments to pass along with tickers, intercepts, and slopes to \code{\link{load.gains}} function, if you wish to load data on the fly rather than specify tickers.gains.
}
  \item{benchmark.tickers}{
Character vector of length 1 or 2 indicating ticker symbols for benchmark indexes, to be used if x.metric or y.metric is set to "alpha", "beta", "r.squared", "pearson", "spearman", or "alpha2", "beta2", and so on. For example, if you wish to plot correlation with SPY on the x-axis and correlation with TLT on the y-axis, you would set x.metric to "pearson" and y.metric to "pearson2", and set benchmark.tickers to c("SPY", "TLT").
}
  \item{reference.tickers}{
Character string or vector of ticker symbols to include on the graph as a data point for comparative purposes.
}
  \item{tickers.gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a particular fund. The first, second, and third column are assumed to be first three-fund set, the fourth through sixth are assumed to be the second set, and so on.
}
  \item{benchmark.gains}{
Numeric vector or matrix of gains for benchmark indexes. Only relevant if either x.metric or y.metric input is set to "alpha", "beta", "r.squared", "pearson", "spearman", or "alpha2", "beta2", and so on. For example, if you wish to plot correlation with SPY on the x-axis and correlation with TLT on the y-axis, you would set x.metric to "pearson" and y.metric to "pearson2", and benchmark.gains should be a 2-column matrix where the first column has SPY gains and the second column has TLT gains.
}
  \item{reference.gains}{
Numeric vector or matrix of gains for reference funds.
}
  \item{step.data}{
Allocation increments for data used to fit curves.
}
  \item{step.points}{
Allocation increments for data points plotted on top of curves. Set to NULL to suppress data points.
}
  \item{step.curves}{
Allocation increments for first fund in each set. For example, if set to 0.2, there will be six curves for each three-fund set: one for 0\% fund 1, another for 20\% fund 1, and so on, up to 100\% fund 1 (which is technically a data point, not a curve).
}
  \item{x.metric}{
Character string specifying what performance metric should be plotted on x-axis. Possible values are as follows: "mean" for mean of gains; "sd" for standard deviation of gains; "growth" for total growth; "cagr" for compound annualized growth rate; "mdd" for maximum drawdown; "sharpe" for Sharpe ratio; "sortino" for Sortino ratio; "alpha" or "beta" for alpha and beta from capital assets pricing model theory; "r.squared" for R-squared from regression on benchmark fund; "pearson" or "spearman" for Pearson or Spearman correlation with benchmark fund; "alpha2", "beta2", r.squared2", "pearson2", or "spearman2" for same as previously described, but using the second benchmark index; "auto.pearson" or "auto.spearman" for Pearson or Spearman autocorrelation, defined as the correlation between subsequent gains; or "allocation" for allocation to first fund in each pair.
}
  \item{y.metric}{
Same as x.metric, but for y-axis variable.
}
  \item{tickerlabel.offsets}{
Either a numeric vector of length 2 giving the x-axis and y-axis offsets for ticker labels, or a 2-column matrix where each row gives the x-axis offset and y-axis offset for a particular ticker, following the same order of tickers as in the tickers matrix (going down each column, then to the next row) or across the ticker.gains matrix.
}
  \item{reflabel.offsets}{
Either a numeric vector of length 2 giving the x-axis and y-axis offsets for reference ticker labels, or a 2-column matrix where each row gives the x-axis offset and y-axis offset for a particular reference ticker, following the same order of tickers as in the reference.tickers vector or across the reference.gains matrix.
}
  \item{add.plot}{
If TRUE, data is plotted on current plot frame rather than a new one.
}
  \item{colors}{
Character string of colors for each three-fund set.  
}
  \item{plot.list}{
Optional list of inputs to pass to plot function.  
}
  \item{points.list}{
Optional list of inputs to pass to points function.
}
  \item{text.list}{
Optional list of inputs to pass to text function.
}
  \item{pdf.list}{
Optional list of inputs to pdf function. If not NULL, .pdf file is created.
}
  \item{bmp.list}{
Optional list of inputs to bmp function. If not NULL, .bmp file is created.
}
  \item{jpeg.list}{
Optional list of inputs to pdf function. If not NULL, .jpg file is created.
}
  \item{png.list}{
Optional list of inputs to pdf function. If not NULL, .png file is created.
}
  \item{tiff.list}{
Optional list of inputs to pdf function. If not NULL, .tif file is created.
}
}
\details{
If tickers input is specified, it gets passed to \code{\link{load.gains}} to load historical prices from Yahoo! Finance using the quantmod package [1]. If tickers.gains is specified, performance metrics are calcualted directly from that matrix.
}
\value{
In addition to the graph, a list containing the following items: (1) A list where each element is a list of two-column matrices of x- and y-axis values for a particular three-fund set with a particular allocation to fund 1; (2) a numeric vector of mean gains for each ticker; and (3) a numeric matrix containing a correlation matrix for gains of each ticker.
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