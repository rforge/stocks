\name{capm.ticker}
\alias{capm.ticker}
\title{
Calculate Capital Assets Pricing Model (CAPM) Metrics
}
\description{
Calculate CAPM metrics for a stock relative to an index: alpha and beta and their associated p-values, and r-squared.
}
\usage{
capm.ticker(ticker, index.ticker = "^GSPC", from = "1950-01-03", to = Sys.Date(), 
            interval.days = 1, decimals = getOption("digits"), gains = FALSE)
}
\arguments{
  \item{ticker}{
Character string indicating the ticker for the stock of interest.
}
  \item{index.ticker}{
Character string indicating the ticker for the index that the stock is being compared to. Default uses S\&P 500.
}
  \item{from}{
Date or character string in form of date (e.g. '1950-01-03' for January 3, 1950), indicating the starting date for the period of interest.
}
  \item{to}{
Date or character string in form of date (e.g. '2015-03-09' for March 9, 2015), indicating the ending date for the period of interest.
}
  \item{interval.days}{
Whole number indicating what time interval you want to use for gains. For example, the default value of 1 means that you want to calculate alpha and beta for daily gains. A value of 2 would mean you want to calculate alpha and beta for 2-day gains, and so forth.
}
  \item{decimals}{
Whole number indicating the number of decimal places you would like the CAPM metrics rounded to.
}
  \item{gains}{
If TRUE, the returned list will include gains for the stock and for the index, in addition to the CAPM metrics which are included regardless.
}
}
\value{
List with alpha, beta, p-value for alpha, p-value for beta, r-squared, and gains for ticker and index if gains input is set to TRUE.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\note{
This function prints warnings which are typically nothing to worry about.
}
\seealso{
NA
}
\examples{
# Get CAPM metrics for Apple over its lifetime, using S&P 500 as benchmark
capm.ticker("AAPL")
}