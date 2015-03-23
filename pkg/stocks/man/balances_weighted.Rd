\name{balances.weighted}
\alias{balances.weighted}
\title{
Calculate Balances of a Weighted Portfolio of Various Funds
}
\description{
Given a matrix of prices or ratios from one time point to the next for several funds (i.e. proportion gains + 1), an initial balance, and weights for each fund, this function calculates the balance in each fund and the overall portfolio at each time point. It returns either the matrix of balances or the final portfolio balance. Frequency of portfolio rebalancing can be adjusted.
}
\usage{
balances.weighted(ratios = NULL, prices = NULL, initial = 10000, 
                  weights = rep(1/ncol(ratios), ncol(ratios)), rebalance.interval = 21, 
                  final.bal = FALSE, nas = FALSE, plot = "all")
}
\arguments{
  \item{ratios}{
Matrix with a column for each fund, where rows indicate ratios between subsequent stock prices (i.e. proportion gains + 1). For example, if the first fund gained 3\%, lost 1\%, then lost 2\%, column 1 of ratios would have values 1.03, 0.99, 0.99.
}
  \item{prices}{
Matrix with a column for each fund, where rows indicate stock prices at each time point.  
}
  \item{initial}{
Initial balance of portfolio.
}
  \item{weights}{
Numeric vector of weights for each fund. Weights have to add to 1.
}
  \item{rebalance.interval}{
Whole number indicating how often you want to rebalance the portfolio to get back to the desired weights. For example, a value of 21 means that portfolio rebalancing is done every 21 days (about 1 month of trading).
}
  \item{final.bal}{
If TRUE, function returns final balance of portfolio rather than matrix of balances for each fund and overall portfolio at each time point.
}
  \item{nas}{
If TRUE, function finds and removes any missing values (NA's) in ratios/prices matrices. The default is FALSE to maximize speed for the usual case where there are no missing values.
}
  \item{plot}{
Controls what (if anything) is plotted. Possible values are as follows: 'all' to plot balances for each fund and the total portfolio (on same scale regardless of weights for purposes of visual comparisons); 'portfolio' to plot portfolio balances; and 'none' to not plot anything.
}
}
\details{
You only need to specify ratios or prices, not both. If both are given, the function uses ratios.
}
\value{
If final.bal is FALSE, a numeric matrix indicating the balance in each fund and the total portfolio balance at each time period; if final.bal is TRUE, a numeric value indicating the final portfolio balance.
}
\note{
This function uses C++ code.
}
\references{
Acknowledgment: This material is based upon work supported by the National Science Foundation Graduate Research Fellowship under Grant No. DGE-0940903.
}
\author{
Dane R. Van Domelen
}
\examples{
NA
}