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



pchanges <- function(x, lag = 1, percent = FALSE) {
  
  # Check that percent is a logical
  if (!is.logical(percent)) {
    stop("For percent input, please enter TRUE or FALSE")
  }
  
  # Call pchanges_c with multiplier depending on percent input
  if (percent) {
    .Call('stocks_pchanges_c', PACKAGE = 'stocks', x, lag, 100)
  } else {
    .Call('stocks_pchanges_c', PACKAGE = 'stocks', x, lag, 1)
  }
  
}



ratios <- function(x) {
  .Call('stocks_ratios_c', PACKAGE = 'stocks', x)
}



convert.rate <- function(rate, days.in = 1, days.out = 1) {
  ((rate + 1)^(days.out/days.in)) - 1
}



daily.yearly <- function(daily.gain, years = 1) {
  (1 + daily.gain)^(251*years) - 1
}



yearly.daily <- function(total.gain, years = 1) {
  (total.gain + 1)^(1/(251*years)) - 1
}



balances <- function(ratios, initial = 10000) {
  c(initial, initial * cumprod(ratios))
}



final.balance <- function(ratios, initial = 10000) {
  initial * prod(ratios)
}



balances.weighted <- function(ratios = NULL, prices = NULL, initial = 10000, weights = rep(1/ncol(ratios), ncol(ratios)),
                              rebalance.interval = 21, final.bal = FALSE, nas = FALSE, plot = "all") {
  
  # Return error if neither ratios nor prices are specified
  if (is.null(ratios) & is.null(prices)) {
    stop("Either the ratios input or the prices input must be specified")
  }
  
  # Check that the weights add to 1
  if (! sum(weights) == 1) {
    stop("For the weights input, please enter a numeric vector of values that add to 1")
  }
  
  # Drop NA's from ratios
  if (!is.null(ratios) & nas) {
    ratios <- ratios[complete.cases(ratios), ]
  }
  
  # Convert from prices to ratios if necessary
  if (!is.null(prices) & is.null(ratios)) {
    
    # Drop NA's
    if (nas) {
      prices <- prices[complete.cases(prices), ]
    }
    
    # Create ratios matrix
    ratios <- apply(prices, 2, function(x) pchanges(x)) + 1
    
  }
  
  # Call C++ function balances_weighted_c
  fund.balances <- .Call('stocks_balances_weighted_c', PACKAGE = 'stocks', ratios, initial, weights, rebalance.interval)
  
  # Assign column names
  headings <- colnames(ratios)
  if (is.null(headings)) {
    headings <- paste("fund", 1:ncol(ratios), sep = "")
  }
  colnames(fund.balances) <- c(headings, "portfolio")
  
  # Prepare output depending on value of final.bal
  if (final.bal) {
    ret <- fund.balances[nrow(fund.balances), ncol(fund.balances)]
  } else {
    ret <- fund.balances
  }
  
  # Create plot if requested
  if (plot == "all") {
    
    fund.balances.scaled <- cbind(fund.balances[, 1:(ncol(fund.balances)-1)] %*% diag(1/weights), fund.balances[, ncol(fund.balances)])
    max.y <- max(fund.balances.scaled)
    my.cols <- c(brewer.pal(n = ncol(fund.balances.scaled) - 1, name = "YlOrRd"), "black")
    plot(1:nrow(fund.balances.scaled), fund.balances.scaled[, 1], type = "n", ylim = c(0, max.y), 
         main = "Scaled Balance of Funds and Portfolio", xlab = "Time", ylab = "Balance ($)")
    for (ii in 1:(ncol(fund.balances.scaled))) {
      points(1:nrow(fund.balances.scaled), fund.balances.scaled[, ii], type = "l", col = my.cols[ii])
    }
    grid()
    legend("topleft", legend = colnames(fund.balances), lty = 1, col = my.cols, bg = "white")
    
  } else if (plot == "portfolio") {
    
    plot(1:nrow(fund.balances), fund.balances[, ncol(fund.balances)], type = "l", 
         main = "Portfolio Balance", xlab = "Time", ylab = "Balance ($)")
    grid()
    
  }
  
  # Return object
  return(ret)
  
}



prices.rate <- function(prices, xday.rate = NULL, nas = FALSE) {
  
  # Drop NA's
  if (nas) {
    prices <- prices[!is.na(prices)]
  }
  
  # Get the overall growth rate
  prices.length <- length(prices)
  
  rate1 <- prices[prices.length] / prices[1] - 1
  
  # Convert to x-day growth rate if xday.rate is specified
  if (! is.null(xday.rate) && xday.rate != prices.length - 1) {
    rate1 <- convert.rate(rate = rate1, days.in = prices.length - 1, days.out = xday.rate)
  }
  
  # Return the rate
  return(rate1)
  
}



gains.rate <- function(gains, xday.rate = NULL, nas = FALSE) {
  
  # Drop NA's
  if (nas) {
    gains <- gains[!is.na(gains)]
  }
  
  # Get the overall growth rate
  gains.length <- length(gains)
  rate1 <- prod(gains + 1) - 1
  
  # Convert to x-day growth rate if xday.rate is specified
  if (! is.null(xday.rate) && ! (xday.rate == gains.length)) {
    rate1 <- convert.rate(rate = rate1, days.in = gains.length, days.out = xday.rate)
  }
  
  # Return the rate
  return(rate1)
  
}



pos <- function(x, include.zero = FALSE) {
  
  # Check that include.zero is a logical
  if (!is.logical(include.zero)) {
    stop("For include.zero input, please enter TRUE or FALSE")
  }
  
  # Get positive values
  if (include.zero) {
    x[which(x >= 0)]
  } else {
    x[which(x > 0)]
  }
  
}



neg <- function(x, include.zero = FALSE) {
  
  # Check that include.zero is a logical
  if (!is.logical(include.zero)) {
    stop("For include.zero input, please enter TRUE or FALSE")
  }
  
  # Get negative values
  if (include.zero) {
    x[which(x <- 0)]
  } else {
    x[which(x < 0)]
  }
  
}



nonpos <- function(x) {
  x[which(x <= 0)]
}



nonneg <- function(x) {
  x[which(x >= 0)]
}



mdd <- function(prices = NULL, gains = NULL, highs = NULL, lows = NULL, indices = FALSE, nas = FALSE) {
  
  # Check that indices is a logical
  if (!is.logical(indices)) {
    stop("For indices input, please enter TRUE or FALSE")
  }
  
  # Drop NA's
  if (nas) {
    
    if (!is.null(prices)) {
      prices <- prices[!is.na(prices)]
    } else if (!is.null(highs)) {
      highs <- highs[!is.na(highs)]
      lows <- lows[!is.na(lows)]
    } else if (!is.null(gains)) {
      gains <- gains[!is.na(gains)]
    }
    
  }
  
  # If gains specified rather than prices, convert to prices
  if (!is.null(gains)) {
    prices <- balances(ratios = gains + 1, initial = 1)
  }
  
  # Call C++ function depending on indices and whether prices or highs and lows specified
  if (!is.null(prices)) {
    if (!is.null(highs) | !is.null(lows)) {
      stop("Please input prices OR highs and lows, but not both. If both are available, use prices.")
    }
    if (nas & is.null(gains)) {
      prices <- prices[!is.na(prices)]
    }
    if (indices) {
      mdd.out <- .Call('stocks_mdd_p_c2', PACKAGE = 'stocks', prices)
      names(mdd.out) <- c("mdd", "start.index", "end.index")
    } else {
      mdd.out <- .Call('stocks_mdd_p_c1', PACKAGE = 'stocks', prices)
    }
  } else {
    if (!is.null(prices)) {
      stop("Please input prices OR highs and lows, but not both. If both are available, use prices.")
    }
    if (nas) {
      highs <- highs[!is.na(highs)]
      lows <- lows[!is.na(lows)]
    }
    if (indices) {
      mdd.out <- .Call('stocks_mdd_hl_c2', PACKAGE = 'stocks', highs, lows)
      names(mdd.out) <- c("mdd", "start.index", "end.index")
    } else {
      mdd.out <- .Call('stocks_mdd_hl_c1', PACKAGE = 'stocks', highs, lows)
    }
  }
    
  # Return mdd.out
  return(mdd.out)  
  
}



sharpe.ratio <- function(gains = NULL, prices = NULL, rf = 0, nas = FALSE) {
  
  # Check that either gains or prices is specified
  if (is.null(gains) & is.null(prices)) {
    stop("Please enter a gains vector or a prices vector")
  }
  
  # Convert from prices to gains if necessary
  if (!is.null(prices)) {
    if (nas) {
      prices <- prices[!is.na(prices)]
    }
    gains <- pchanges(prices)
  } else {
    if (nas) {
      gains <- gains[!is.na(gains)]
    }
  }
  
  # Calculate Sharpe ratio
  (mean(gains) - rf) / sd(gains)
  
}



sortino.ratio <- function(gains = NULL, prices = NULL, rf = 0, nas = FALSE) {
  
  # Check that either gains or prices is specified
  if (is.null(gains) & is.null(prices)) {
    stop("Please enter a gains vector or a prices vector")
  }
  
  # Convert from prices to gains if necessary
  if (!is.null(prices)) {
    if (nas) {
      prices <- prices[!is.na(prices)]
    }
    gains <- pchanges(prices)
  } else {
    if (nas) {
      gains <- gains[!is.na(gains)]
    }
  }
  
  # Calculate Sortino ratio
  (mean(gains) - rf) / sd(neg(gains))
  
}



rrr <- function(prices = NULL, gains = NULL, nas = FALSE) {
  
  # Check that either prices or gains is specified
  if (is.null(prices) & is.null(gains)) {
    stop("Please enter a prices vector or a gains vector")
  }
  
  # Calculate overall growth rate
  if (!is.null(prices)) {
    if (nas) {
      prices <- prices[!is.na(prices)]
    }
    ret <- prices.rate(prices)
    max.dd <- mdd(prices = prices)
  } else {
    if (nas) {
      gains <- gains[!is.na(gains)]
    }
    ret <- gains.rate(gains)
    max.dd <- mdd(gains = gains)
  }
  
  # Calculate risk-return ratio
  ret / max.dd
  
}



ticker.dates <- function(tickers) {
  
  # Download stock prices for tickers from Yahoo! Finance, using the quantmod package
  prices <- list()
  for (ii in 1:length(tickers)) {
    prices[[ii]] <- as.matrix(getSymbols(Symbols = tickers[ii], from = "1900-01-01", auto.assign = FALSE, warnings = FALSE))
  }
  
  # Create object to return
  ret <- data.frame(ticker = tickers,
                    start.date = unlist(lapply(prices, function(x) rownames(x)[1])),
                    end.date = unlist(lapply(prices, function(x) rev(rownames(x))[1])))
  return(ret)
  
}



monthly.gains <- function(tickers = NULL, quantmod.list = NULL, from = NULL, 
                          to = NULL, decimals = getOption("digits"),
                          partialmonth.min = 10) {
  
  # Check that either tickers or quantmod.list is specified, but not both
  if (!is.null(tickers) & !is.null(quantmod.list)) {
    stop("Please specify either the tickers input or the quantmod.list input, but not both")
  }
  
  # Create quantmod.list object if a vector of tickers is given
  if (!is.null(tickers)) {
    
    # If to not specified, set to first day of current month
    if (is.null(to)) {
      today <- Sys.Date()
      to <- today
    }
    
    # If from not specified, set to 5 years ago
    if (is.null(from)) {
      from <- as.Date(paste(year(to)-5, month(to), "01", sep = "-")) - 5
    }
    
    # Download stock prices for tickers from Yahoo! Finance, using the quantmod package
    quantmod.list <- list()
    for (ii in 1:length(tickers)) {
      quantmod.list[[ii]] <- as.matrix(getSymbols(Symbols = tickers[ii], from = from, to = to, auto.assign = FALSE, warnings = FALSE))
    }
    names(quantmod.list) <- tickers
    
  } else {
    
    # Get ticker names
    tickers <- names(quantmod.list)
    
    # Convert to matrices if not already
    if (!all(unlist(lapply(quantmod.list, class)) == "matrix")) {
      quantmod.list <- lapply(quantmod.list, as.matrix)
    }
    
  }
  
  # Get dates for each fund
  dates <- lapply(quantmod.list, function(x) as.Date(rownames(x)))
  start.dates <- as.Date(unlist(lapply(dates, function(x) x[1])))
  end.dates <- as.Date(unlist(lapply(dates, function(x) rev(x)[1])))
  
  # If start/end dates are different, get subset of data with mutual lifetimes
  if (length(unique(start.dates)) > 1 | length(unique(end.dates)) > 1) {
    
    # Update from and to dates
    from <- max(start.dates)
    to <- min(end.dates)
    
    # Change matrices within quantmod.list to all cover the same time frame
    quantmod.list <- lapply(quantmod.list, function(x) x[as.Date(rownames(x)) >= from & as.Date(rownames(x)) <= to, ])
    
    # Get updated dates
    dates <- lapply(quantmod.list, function(x) as.Date(rownames(x)))
    start.dates <- as.Date(unlist(lapply(dates, function(x) x[1])))
    end.dates <- as.Date(unlist(lapply(dates, function(x) rev(x)[1])))
    
  }
  
  # Check that dates match up for all of the funds
  if (length(quantmod.list) > 1) {
    for (ii in 1:(length(quantmod.list) - 1)) {
      if (!all(dates[[ii]] == dates[[ii+1]])) {
        stop("Unfortunately, the dates don't match up perfectly for all of the tickers.")
      }
    }
  }
  
  # Get a single dates matrix
  dates <- dates[[1]]
  
  # Create prices matrix
  prices <- matrix(unlist(lapply(quantmod.list, function(x) x[, 6])), ncol = length(quantmod.list), byrow = FALSE)
  colnames(prices) <- tickers
  
  # Initialize gains matrix
  gains <- matrix(NA, nrow = length(unique(year(dates))) * 12, ncol = 3 + 3*ncol(prices))
  
  # Loop through each month and calculate gain for S&P 500 and LUV
  index1 <- 0
  for (yr in unique(year(dates))) {
    for (mnth in 1:12) {
      locs <- which(year(dates) == yr & month(dates) == mnth)
      if (length(locs) >= partialmonth.min) {
        if (locs[1] > 1) {
          locs <- c(locs[1]-1, locs)
        }
        index1 <- index1 + 1
        gains[index1, 1] <- yr
        gains[index1, 2] <- mnth
        gains[index1, 3] <- length(locs) - 1
        gains[index1, 4:(3 + ncol(prices))] <- apply(prices[locs, ], 2, function(x) prices.rate(x))
        gains[index1, (ncol(prices) + 4): (2*ncol(prices) + 3)] <- apply(prices[locs, ], 2, function(x) prices.rate(x, xday.rate = 251))
        gains[index1, (2*ncol(prices) + 4): (3*ncol(prices) + 3)] <- apply(prices[locs, ], 2, function(x) prices.rate(x, xday.rate = 1))
      }
    }
  }
  gains <- gains[1:index1, ]
  gains[, 4:ncol(gains)] <- round(gains[, 4:ncol(gains)], decimals)
  colnames(gains) <- c("year", "month", "days", paste(rep(tickers, 3), rep(c(".growth", ".cagr", ".cdgr"), each = ncol(prices)), sep = ""))
  
  # Convert to data frame and return
  gains <- as.data.frame(gains)
  return(gains)
  
}



yearly.gains <- function(tickers = NULL, quantmod.list = NULL, index = "^GSPC", 
                         from = NULL, to = NULL, decimals = getOption("digits"),
                         partialyear.min = 125) {
  
  # Check that either tickers or quantmod.list is specified, but not both
  if (!is.null(tickers) & !is.null(quantmod.list)) {
    stop("Please specify either the tickers input or the quantmod.list input, but not both")
  }
  
  # Create quantmod.list object if a vector of tickers is given
  if (!is.null(tickers)) {
    
    # If to not specified, set to first day of current month
    if (is.null(to)) {
      today <- Sys.Date()
      to <- today
    }
    
    # If from not specified, set to 5 years ago
    if (is.null(from)) {
      from <- as.Date(paste(year(to)-5, "01-01", sep = "-")) - 5
    }
    
    # Download stock prices for tickers from Yahoo! Finance, using the quantmod package
    quantmod.list <- list()
    for (ii in 1:length(tickers)) {
      quantmod.list[[ii]] <- as.matrix(getSymbols(Symbols = tickers[ii], from = from, to = to, auto.assign = FALSE, warnings = FALSE))
    }
    names(quantmod.list) <- tickers
    
  } else {
    
    # Get ticker names
    tickers <- names(quantmod.list)
    
    # Convert to matrices if not already
    if (!all(unlist(lapply(quantmod.list, class)) == "matrix")) {
      quantmod.list <- lapply(quantmod.list, as.matrix)
    }
    
  }
  
  # Get dates for each fund
  dates <- lapply(quantmod.list, function(x) as.Date(rownames(x)))
  start.dates <- as.Date(unlist(lapply(dates, function(x) x[1])))
  end.dates <- as.Date(unlist(lapply(dates, function(x) rev(x)[1])))
  
  # If start/end dates are different, get subset of data with mutual lifetimes
  if (length(unique(start.dates)) > 1 | length(unique(end.dates)) > 1) {
    
    # Update from and to dates
    from <- max(start.dates)
    to <- min(end.dates)
    
    # Change matrices within quantmod.list to all cover the same time frame
    quantmod.list <- lapply(quantmod.list, function(x) x[as.Date(rownames(x)) >= from & as.Date(rownames(x)) <= to, ])
    
    # Get updated dates
    dates <- lapply(quantmod.list, function(x) as.Date(rownames(x)))
    start.dates <- as.Date(unlist(lapply(dates, function(x) x[1])))
    end.dates <- as.Date(unlist(lapply(dates, function(x) rev(x)[1])))
    
  }
  
  # Check that dates match up for all of the funds
  if (length(quantmod.list) > 1) {
    for (ii in 1:(length(quantmod.list) - 1)) {
      if (!all(dates[[ii]] == dates[[ii+1]])) {
        stop("Unfortunately, the dates don't match up perfectly for all of the tickers.")
      }
    }
  }
  
  # Get a single dates matrix
  dates <- dates[[1]]
  
  # Create prices matrix
  prices <- matrix(unlist(lapply(quantmod.list, function(x) x[, 6])), ncol = length(quantmod.list), byrow = FALSE)
  colnames(prices) <- tickers
  
  # Initialize gains matrix
  gains <- matrix(NA, nrow = length(unique(year(dates))), ncol = 2 + 3*ncol(prices))
  
  # Loop through each month and calculate gain for S&P 500 and LUV
  index1 <- 0
  for (yr in unique(year(dates))) {
    locs <- which(year(dates) == yr)
    if (length(locs) >= partialyear.min) {
      if (locs[1] > 1) {
        locs <- c(locs[1]-1, locs)
      }
      index1 <- index1 + 1
      gains[index1, 1] <- yr
      gains[index1, 2] <- length(locs) - 1
      gains[index1, 3:(2 + ncol(prices))] <- apply(prices[locs, ], 2, function(x) prices.rate(x))
      gains[index1, (ncol(prices) + 3): (2*ncol(prices) + 2)] <- apply(prices[locs, ], 2, function(x) prices.rate(x, xday.rate = 251))
      gains[index1, (2*ncol(prices) + 3): (3*ncol(prices) + 2)] <- apply(prices[locs, ], 2, function(x) prices.rate(x, xday.rate = 1))
    }
  }
  gains <- gains[1:index1, ]
  gains[, 3:ncol(gains)] <- round(gains[, 3:ncol(gains)], decimals)
  colnames(gains) <- c("year", "days", paste(rep(tickers, 3), rep(c(".growth", ".cagr", ".cdgr"), each = ncol(prices)), sep = ""))
  
  # Convert to data frame and return
  gains <- as.data.frame(gains)
  return(gains)
  
}