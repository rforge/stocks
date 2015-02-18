diffs <- function(x, lag = 1) {
  
  .Call('stocks_diffs_c', PACKAGE = 'stocks', x, lag)
  
}