\name{beta.trailing50}
\alias{beta.trailing50}
\title{
Beta for Last 50 Daily Gains
}
\description{
Calculate beta for a ticker symbol based on the previous 50 daily gains. The function loads historical prices from Yahoo! Finance and calculates beta by regressing the fund's gains vs. the benchmark's gains and extracting the slope.
}
\usage{
beta.trailing50(ticker, benchmark.ticker = "SPY")
}
\arguments{
  \item{ticker}{
Ticker symbol for fund that you want to calculate beta for.
}
  \item{benchmark.ticker}{
Ticker symbol for benchmark fund.
}
}
\value{
Numeric value indicating the trailing 50-day beta.
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
\examples{
# Calculate TLT's beta based on the previous 50 daily gains
beta.trailing50("TLT")
}