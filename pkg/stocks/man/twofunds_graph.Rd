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
               benchmark.ticker = "VFINX", reference.tickers = benchmark.ticker,
               tickers.gains = NULL, benchmark.gains = NULL, reference.gains = NULL,
               from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
               step.data = 0.0025, step.points = 0.1, 
               y.axis = "mean", x.axis = "sd", 
               add.plot = FALSE,
               colors = NULL,
               plot.list = NULL,
               points.list = NULL,
               text.list = NULL)
}
\arguments{
  \item{tickers}{
Character vector or matrix of ticker symbols. If a vector, first and second elements are assumed to be first pair, third and fourth are assumed to be the second pair, and so on. If a matrix, should have two rows, and each column should represent a different pair.
}
  \item{intercepts}{
Numeric vector or matrix of values to add to daily gains for each ticker. For example, if you have two tickers and want to simulate a 1\% annual expense ratio for only the second fund, you would set intercepts to c(0, convert.rate(-0.01, units.in = 252, units.out = 1)). Like the tickers input, can be an even length vector or a two-row matrix.
}
  \item{slopes}{
Numeric vector or matrix to multiply daily gains for each ticker by. For example, if you have two tickers and want to simulate a 2x leveraged version of the second fund, you would set slopes to c(1, 2). The slopes are multiplied prior to adding the intercepts. Like the tickers input, can be an even length vector or a two-row matrix.
}
  \item{benchmark.ticker}{
Character string indicating ticker symbol for benchmark index. Only relevant if either x.axis or y.axis input is set to "alpha" or "beta".
}
  \item{reference.tickers}{
Character string or vector of ticker symbols to include on the graph as a data point for comparative purposes.
}
  \item{ticker.gains}{
Numeric matrix of gains (daily or otherwise), where each column has gains for a particular fund. First and second column are assumed to be first pair, third and fourth are assumed to be second pair, and so on.
}
  \item{benchmark.gains}{
Numeric vector or matrix of gains for benchmark index. Only relevant if either x.axis or y.axis input is set to "alpha" or "beta".
}
  \item{reference.gains}{
Numeric vector or matrix of gains for reference funds.
}
  \item{from}{
Date or character string in form of date (e.g. "2015-01-01" for Jan. 1, 2015), indicating the starting date for the period of interest.
}
  \item{to}{
Date or character string in form of date (e.g. "2016-01-01" for Jan. 1, 2016), indicating the ending date for the period of interest.
}
  \item{time.scale}{
Controls time frame for performance metrics. Possible values are "daily", "monthly", and "yearly".
}
  \item{step.data}{
Allocation increments for data used to fit curves.
}
  \item{step.points}{
Allocation increments for data points plotted on top of curves. Set to NULL to suppress data points.
}
  \item{y.axis}{
Character string specifying what performance metric should be plotted on y-axis. Possible values are as follows: "mean" for mean of gains; "sd" for standard deviation of gains; "cagr" for compound annualized growth rate; "mdd" for maximum drawdown; "sharpe" for Sharpe ratio; "sortino" for Sortino ratio; "alpha" or "beta" for alpha and beta from capital assets pricing model theory; or "allocation" for allocation to first fund in each pair.
}
  \item{x.axis}{
Character string specifying what performance metric should be plotted on x-axis. Possible values are as follows: "mean" for mean of gains; "sd" for standard deviation of gains; "cagr" for compound annualized growth rate; "mdd" for maximum drawdown; "sharpe" for Sharpe ratio; "sortino" for Sortino ratio; "alpha" or "beta" for alpha and beta from capital assets pricing model theory; or "allocation" for allocation to first fund in each pair.
}
  \item{add.plot}{
If TRUE, data is plotted on current plot frame rather than a new one.
}
  \item{colors}{
Character string of colors for each fund pair.  
}
  \item{plot.list}{
Optional list of inputs to plot function.  
}
  \item{points.list}{
Optional list of inputs to points function.
}
  \item{text.list}{
Optional list of inputs to text function.
}
}
\details{
If tickers input is specified, function loads historical prices for the tickers from Yahoo! Finance using the quantmod package [1]. If tickers.gains input is specified, performance metrics are calculated directly from that matrix.
}
\value{
In addition to the graph, a list containing the following items: (1) A list where each element is a two-column matrix of x- and y-axis values for a fund pair; (2) a numeric vector of mean gains for each ticker; and (3) a numeric matrix containing a correlation matrix for gains of each ticker.
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