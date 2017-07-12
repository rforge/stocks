\name{contango.hedged}
\alias{contango.hedged}
\title{
Implement a Hedged Contango-Based Volatility Trading Strategy
}
\description{
Implements the following volatility strategy: Each day, hold XIV/SPXU (weighted
for zero beta) if contango is greater than \code{xiv.spxu.cutpoint}, hold
VXX/UPRO (weighted for zero beta) if contango is less than
\code{vxx.upro.cutpoint}, and hold cash otherwise.
}
\usage{
contango.hedged(contango,
                xiv.spxu.gains = NULL, vxx.upro.gains = NULL,
                xiv.spxu.cutpoint = 6.36, vxx.upro.cutpoint = 5.45,
                xiv.allocation = 0.46, vxx.allocation = 0.46,
                xiv.beta = NULL, vxx.beta = NULL,
                initial = 10000)
}
\arguments{
  \item{contango}{
Numeric vector of contango values at the end of each trading day (or week,
month, etc.).
}
  \item{xiv.spxu.gains}{
2-column numeric matrix of gains for XIV and SPXU. Number of rows should match
length of \code{contango}. The first row should give XIV and SPXU gain for the
day AFTER the first contango value.
}
  \item{vxx.upro.gains}{
2-column numeric matrix of gains for VXX and UPRO. Number of rows should match
length of \code{contango}. The first row should give VXX and UPRO gain for the
day AFTER the first contango value.
}
  \item{xiv.spxu.cutpoint}{
Numeric value giving the contango cutpoint for XIV/SPXU position. For example,
if \code{xiv.spxu.cutpoint = 5}, then XIV/SPXU will be held whenever contango
is greater than 5.
}
  \item{vxx.upro.cutpoint}{
Numeric value giving the contango cutpoint for VXX/UPRO position. For example,
if \code{vxx.upro.cutpoint = -5}, then VXX/UPRO will be held whenever contango
is less than -5.
}
  \item{xiv.allocation}{
Numeric value indicating XIV allocation for XIV/SPXU positions. For example, if
set to 0.46, then 46\% is allocated to XIV and 54\% to SPXU when contango is
greater than \code{xiv.spxu.cutpoint}.
}
  \item{vxx.allocation}{
Numeric value indicating VXX allocation for VXX/UPRO positions. For example, if
set to 0.46, then 46\% is allocated to VXX and 54\% to UPRO when contango is less than than \code{vxx.upro.cutpoint}.
}
  \item{xiv.beta}{
Numeric value indicating XIV's beta. If specified, the function figures out what
\code{xiv.allocation} needs to be for zero-beta XIV/SPXU positions. For example,
if set to 3.5, then 46.2\% XIV/53.8\% SPXU achieves zero beta.
}
  \item{vxx.beta}{
Numeric value indicating VXX's beta. If specified, the function figures out what
\code{vxx.allocation} needs to be for zero-beta VXX/UPRO positions. For example,
if set to -3.5, then 46.2\% VXX/53.8\% UPRO achieves zero beta.
}
  \item{initial}{
Numeric value giving the initial value of the portfolio.
}
}
\details{
You can find historical contango values from The Intelligent Investor Blog. You
can click the first link at \url{http://investing.kuchita.com/2012/06/28/xiv-data-and-pricing-model-since-vix-futures-available-2004/}
to download a zip file containing an Excel spreadsheet. Then, you will need to
calculate whatever version of "contango" you prefer. I typically define contango
as what percent higher the second-month VIX futures are acompared to the first-
month futures, i.e. dividing the "2nd mth" column by the "1st mth" column,
subtracting 1, and then multiplying by 100.

To load daily gains for XIV, SPXU, VXX, and UPRO, you can use
\code{\link{load.gains}}, which uses the \bold{quantmod} package [1] to load
data from Yahoo! Finance. You will have to specify the \code{from} and \code{to} inputs to
match the date range for your contango values.
}
\value{
A 4-element list containing the following: (1) A character vector named
\code{holdings} indicating what fund was held each day (XIV/SPXU, VXX/UPRO, or
cash); (2) a numeric vector named \code{port.gains} giving the portfolio gain
for each day, which will be 0 for days that cash was held and the weighted
XIV/SPXU or VXX/UPRO gain for days that one of those positions was held; (3) a
numeric vector named \code{port.balances} giving the portfolio balance each day;
and (4) a numeric value named \code{trades} giving the total number of trades
that were executed.
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
\examples{
NA
}
