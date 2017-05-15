\name{contango.simple}
\alias{contango.simple}
\title{
Implement a Simple Contango-Based Volatility Trading Strategy
}
\description{
Implements the following volatility strategy: Each day, hold XIV if contango is greater than xiv.cutpoint, hold VXX if contango is less than vxx.cutpoint, and hold cash otherwise. 
}
\usage{
contango.simple(contango, 
                xiv.gains = NULL, vxx.gains = NULL, 
                xiv.cutpoint = 0, vxx.cutpoint = -Inf,
                initial = 10000)
}
\arguments{
  \item{contango}{
Numeric vector of contango values at the end of each trading day (or week, month, etc.).
}
  \item{xiv.gains}{
Numeric vector of daily gains for XIV. Should be same length as contango and date-shifted one value to the right. For example, the first value of xiv.gains should be the XIV gain for the day AFTER the first contango value.
}
  \item{vxx.gains}{
Numeric vector of daily gains for VXX. Should be same length as contango and date-shifted one value to the right. For example, the first value of vxx.gains should be the VXX gain for the day AFTER the first contango value.
}
  \item{xiv.cutpoint}{
Numeric value giving the contango cutpoint for XIV. For example, if xiv.cutpoint is 5, then XIV will be held whenever contango is greater than 5.  
}
  \item{vxx.cutpoint}{
Numeric value giving the contango cutpoint for VXX. For example, if vxx.cutpoint is -5, then VXX will be held whenever contango is less than -5. 
}
  \item{initial}{
Numeric value giving the initial value of the portfolio.
}
}
\details{
You can find historical contango values from The Intelligent Investor Blog. You can click the first link at \url{http://investing.kuchita.com/2012/06/28/xiv-data-and-pricing-model-since-vix-futures-available-2004/} to download a zip file containing an Excel spreadsheet. Then, you will need to calculate whatever version of "contango" you prefer. I typically define contango as what percent higher the second-month VIX futures are acompared to the first-month futures, i.e. dividing the "2nd mth" column by the "1st mth" column, subtracting 1, and then multiplying by 100.

I think the most common approach for contango-based volatility strategies is holding XIV (inverse volatility) when contango is above some value (e.g. 0\%, 5\%, or 10\%), and holding cash otherwise. You can do that with this function by leaving vxx.cutpoint as -Inf. However, you may also want to hold VXX (volatility) when contango is below some value (e.g. 0\%, -5\%, -10\%), also known as "backwardation". You can implement an XIV-only, VXX-only, or XIV and VXX strategy with this function.

To load daily gains for XIV and/or VXX, you can use \code{\link{load.gains}}, which uses the quantmod package [1] to load data from Yahoo! Finance. You will have to specify the \code{from} and \code{to} inputs to match the date range for your contango values.  
}
\value{
A 4-element list containing the following: (1) A character vector named "holdings" indicating what fund was held each day (XIV, VXX, or cash); (2) A numeric vector named "port.gains" giving the portfolio gain for each day, which will be 0 for days that cash was held and the XIV or VXX gain for days that XIV or VXX was held; (3) A numeric vector named "port.balances" giving the portfolio balance each day; and (4) a numeric value named "trades" giving the total number of trades that were executed. 
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