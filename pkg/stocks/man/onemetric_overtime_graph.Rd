\name{onemetric.overtime.graph}
\alias{onemetric.overtime.graph}
\title{
Graph Performance Metric Over Time
}
\description{
Create plot comparing a performance metric across several investments over a period of time.
}
\usage{
onemetric.overtime.graph(tickers = NULL, intercepts = NULL, slopes = NULL,
                         gains = NULL, prices = NULL,
                         from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                         earliest.subset = FALSE,
                         y.metric = "cagr", 
                         add.plot = FALSE, 
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
Character vector of ticker symbols. If y.metric requires a benchmark index, the first ticker is taken to be the benchmark.
}
  \item{intercepts}{
Numeric vector of values to add to daily gains for each ticker. For example, if you have two tickers and want to simulate a 1\% annual expense ratio for only the second fund, you would set intercepts to c(0, convert.rate(-0.01, units.in = 252, units.out = 1)).
}
  \item{slopes}{
Numeric vector of values to multiply daily gains for each ticker by. For example, if you have two tickers and want to simulate a 2x leveraged version of the second fund, you would set slopes to c(1, 2). The slopes are multiplied prior to adding the intercepts.
}
  \item{gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a particular investment. If y.metric requires a benchmark index, the first column should be gains for the benchmark.
}
  \item{prices}{
Numeric matrix of prices (daily or otherwise), where each column has prices for a particular investment. If y.metric requires a benchmark index, the first column should be prices for the benchmark.
}
  \item{from}{
Date or character string in form of date (e.g. "2015-01-01" for Jan. 1, 2015), indicating the starting date for the period of interest.
}
  \item{to}{
Date or character string in form of date (e.g. "2016-01-01" for Jan. 1, 2016), indicating the ending date for the period of interest.
}
  \item{time.scale}{
Character string that controls the time frame for data points. Possible values are "daily", "monthly", and "yearly".
}
  \item{earliest.subset}{
If TRUE, only the subset of tickers with data going back to "from" date (or the earliest) are included; if FALSE, all tickers are included, and data covering their mutual lifetimes is used.
}
\item{y.metric}{
Character string specifying what performance metric should be plotted. Possible values are as follows: "mean" for mean of gains; "sd" for standard deviation of gains; "growth" for total growth; "cagr" for compound annualized growth rate; "mdd" for maximum drawdown; "sharpe" for Sharpe ratio; "sortino" for Sortino ratio; "alpha" or "beta" for alpha and beta from capital assets pricing model theory; "r.squared" for R-squared from regression on benchmark fund; "pearson" or "spearman" for Pearson or Spearman correlation with benchmark fund; or "auto.pearson" or "auto.spearman" for Pearson or Spearman autocorrelation, defined as the correlation between subsequent gains.
}
  \item{add.plot}{
If TRUE, data is plotted on current plot frame rather than a new one.
}
  \item{plot.list}{
Optional list of inputs to pass to plot function.  
}
  \item{points.list}{
Optional list of inputs to pass to points function.
}
  \item{axis.list}{
Optional list of inputs to pass to axis function.
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
If tickers input is specified, function loads historical prices for the tickers from Yahoo! Finance using the quantmod package [1]. If gains or prices input is specified, the graph is created directly from that matrix.
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