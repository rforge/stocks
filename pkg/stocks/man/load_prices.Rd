\name{load.prices}
\alias{load.prices}
\title{
Download and Align Historical Prices for a Set of Tickers
}
\description{
Downloads and aligns historical prices for specified tickers from Yahoo!
Finance, using the \bold{quantmod} package [1].
}
\usage{
load.prices(tickers, intercepts = NULL, slopes = NULL, ...,
            from = "1950-01-01", to = Sys.Date(),
            time.scale = "daily",
            preto.days = NULL, prefrom.days = NULL,
            initial = NULL,
            earliest.subset = FALSE, latest.subset = FALSE)
}
\arguments{
  \item{tickers}{
Character string or vector of ticker symbols.
}
  \item{intercepts}{
Numeric vector of values to add to daily gains for each ticker. For example, if
you have two tickers and want to simulate a 1\% annual expense ratio for only
the second fund, you would set \code{intercepts = c(0, convert.rate(-0.01,
units.in = 252, units.out = 1))}.
}
  \item{slopes}{
Numeric vector of values to multiply daily gains for each ticker by. For
example, if you have two tickers and want to simulate a 2x leveraged version of
the second fund, you would set \code{slopes = c(1, 2)}. The slopes are
multiplied prior to adding the intercepts.
}
  \item{...}{
Arguments to pass to \code{\link{getSymbols}} or \code{\link{adjustOHLC}}
functions, which are called internally by this function. For example, you can
download data from Google Finance rather than Yahoo! Finance by adding the
\code{getSymbols} argument \code{src = "google"}.
}
  \item{from}{
Date or character string in form of date (e.g. \code{"2015-01-01"} for Jan. 1,
2015), indicating the starting date for the period of interest.
}
  \item{to}{
Date or character string in form of date (e.g. \code{"2016-01-01"} for Jan. 1,
2016), indicating the ending date for the period of interest.
}
  \item{time.scale}{
Character string that controls the time frame for prices. Possible values are
\code{"daily"}, \code{"monthly"}, and \code{"yearly"}.
}
  \item{preto.days}{
If specified, function returns prices for \code{preto.days} trading days prior
to \code{to}. To illustrate, if you want to load the most recent 50 daily
closing prices, you would leave \code{to} and \code{time.scale} as the defaults
and set \code{preto.days = 50}.
}
  \item{prefrom.days}{
If specified, function returns prices for \code{prefrom.days} trading days prior
to \code{from}. Useful when you want to test a trading strategy starting on a
particular date, but the strategy requires data leading up to that date, e.g.
trailing beta.
}
  \item{initial}{
Numeric value indicating what you would like to scale the initial price of each
fund to. May be useful When loading data for multiple funds that you want to
compare.
}
  \item{earliest.subset}{
If \code{TRUE}, only the subset of tickers with data going back to \code{from}
(or the earliest) are included; if \code{FALSE}, all tickers are included, and
data covering their mutual lifetimes is returned.
}
  \item{latest.subset}{
If \code{TRUE}, only the subset of tickers with data going all the way to
\code{to} (or the latest) are included; if \code{FALSE}, all tickers are
included, and data covering their mutual lifetimes is returned.
}
}
\details{
In aligning historical prices, dates on which not all funds have data are simply
dropped. Messages are printed indicating which dates are dropped for which
tickers.
}
\value{
A numeric matrix where each column indicates the daily/monthly/yearly prices for
a given ticker over the time period of interest. If the time interval is monthly
or yearly, prices are given for the last day of each month/year.
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
\code{\link{load.gains}}
}
\examples{
NA
}
