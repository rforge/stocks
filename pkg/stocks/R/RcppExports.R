diffs <- function(x, lag = 1) {
  .Call('stocks_diffs_c', PACKAGE = 'stocks', x, lag)
}

pdiffs <- function(x, lag = 1, percent = FALSE) {
  
  # Check that percent is a logical
  if (!is.logical(percent)) {
    stop("For percent input, please enter TRUE or FALSE")
  }

  # Call pdiffs_c with multiplier depending on percent input
  if (percent) {
    .Call('stocks_pdiffs_c', PACKAGE = 'stocks', x, lag, 100)
  } else {
    .Call('stocks_pdiffs_c', PACKAGE = 'stocks', x, lag, 1)
  }

}

convert.rate <- function(rate, days.in = 1, days.out = 1) {
  out.rate <- ((rate + 1)^(days.out/days.in)) - 1
  return(out.rate)
}