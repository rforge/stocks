\name{growth.graph}
\alias{growth.graph}
\title{
Graph Investment Growth
}
\description{
Creates graph of investment growth for one or more investments.
}
\usage{
growth.graph(tickers = NULL, intercepts = NULL, slopes = NULL,
             prices = NULL,
             from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
             earliest.subset = TRUE,
             initial = 10000,
             add.plot = FALSE,
             colors = NULL, 
             plot.list = NULL,
             points.list = NULL,
             grid.list = NULL,
             legend.list = NULL,
             pdf.list = NULL,
             bmp.list = NULL,
             jpeg.list = NULL,
             png.list = NULL,
             tiff.list = NULL)
}
\arguments{
  \item{tickers}{
Character vector of ticker symbols.
}
  \item{intercepts}{
Numeric vector of values to add to daily gains for each ticker. For example, if you have two tickers and want to simulate a 1\% annual expense ratio for only the second fund, you would set intercepts to c(0, convert.rate(-0.01, units.in = 252, units.out = 1)).
}
  \item{slopes}{
Numeric vector of values to multiply daily gains for each ticker by. For example, if you have two tickers and want to simulate a 2x leveraged version of the second fund, you would set slopes to c(1, 2). The slopes are multiplied prior to adding the intercepts.
}
  \item{prices}{
Numeric matrix of prices (daily or otherwise), where each column has prices for a particular investment.
}
  \item{from}{
Date or character string in form of date (e.g. "2015-01-01" for Jan. 1, 2015), indicating the starting date for the period of interest.
}
  \item{to}{
Date or character string in form of date (e.g. "2016-01-01" for Jan. 1, 2016), indicating the ending date for the period of interest.
}
  \item{time.scale}{
Controls time frame for data points. Possible values are "daily", "monthly", and "yearly".
}
  \item{earliest.subset}{
If TRUE, only the subset of tickers with data going back to "from" date (or the earliest) are included; if FALSE, all tickers are included, and data covering their mutual lifetimes is used.
}
  \item{initial}{
Numeric value indicating what you would like to scale the initial price of each fund to. If set to NULL, actual fund prices are used.
}
  \item{add.plot}{
If TRUE, data is plotted on current plot frame rather than a new one.
}
  \item{colors}{
Character string of colors for each investment.
}
  \item{plot.list}{
Optional list of inputs to pass to plot function.  
}
  \item{points.list}{
Optional list of inputs to pass to points function.
}
  \item{grid.list}{
Optional list of inputs to pass to grid function.  
}
  \item{legend.list}{
Optional list of inputs to pass to legend function.  
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
If tickers input is specified, function loads historical prices for the tickers from Yahoo! Finance using the quantmod package [1]. If prices input is specified, the graph is created directly from that matrix.
}
\value{
In addition to the graph, a list containing the following items: (1) A numeric matrix of prices for each investment; (2) a numeric vector of mean gains for each ticker; and (3) a numeric matrix containing a correlation matrix for gains of each ticker.
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