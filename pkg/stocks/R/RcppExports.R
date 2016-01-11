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
                              rebalance.interval = 21, final.bal = FALSE, nas = FALSE, plot.type = "all") {
  
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
  if (plot.type == "all") {
    
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
    
  } else if (plot.type == "portfolio") {
    
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



rrr <- function(prices = NULL, gains = NULL, highs = NULL, lows = NULL, nas = FALSE) {
  
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
  start.dates <- as.Date(unlist(lapply(dates, function(x) as.character(x[1]))))
  end.dates <- as.Date(unlist(lapply(dates, function(x) as.character(rev(x)[1]))))
  
  # If start/end dates are different, get subset of data with mutual lifetimes
  if (length(unique(start.dates)) > 1 | length(unique(end.dates)) > 1) {
    
    # Update from and to dates
    from <- max(start.dates)
    to <- min(end.dates)
    
    # Change matrices within quantmod.list to all cover the same time frame
    quantmod.list <- lapply(quantmod.list, function(x) x[as.Date(rownames(x)) >= from & as.Date(rownames(x)) <= to, ])
    
    # Get updated dates
    dates <- lapply(quantmod.list, function(x) as.Date(rownames(x)))
    
    start.dates <- as.Date(unlist(lapply(dates, function(x) as.character(x[1]))))
    end.dates <- as.Date(unlist(lapply(dates, function(x) as.character(rev(x)[1]))))
    
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
        gains[index1, 4:(3 + ncol(prices))] <- apply(prices[locs, , drop = F], 2, function(x) prices.rate(x))
        gains[index1, (ncol(prices) + 4): (2*ncol(prices) + 3)] <- apply(prices[locs, , drop = F], 2, function(x) prices.rate(x, xday.rate = 251))
        gains[index1, (2*ncol(prices) + 4): (3*ncol(prices) + 3)] <- apply(prices[locs, , drop = F], 2, function(x) prices.rate(x, xday.rate = 1))
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



yearly.gains <- function(tickers = NULL, quantmod.list = NULL, from = NULL, to = NULL, 
                         decimals = getOption("digits"), partialyear.min = 125) {
  
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
  start.dates <- as.Date(unlist(lapply(dates, function(x) as.character(x[1]))))
  end.dates <- as.Date(unlist(lapply(dates, function(x) as.character(rev(x)[1]))))
  
  # If start/end dates are different, get subset of data with mutual lifetimes
  if (length(unique(start.dates)) > 1 | length(unique(end.dates)) > 1) {
    
    # Update from and to dates
    from <- max(start.dates)
    to <- min(end.dates)
    
    # Change matrices within quantmod.list to all cover the same time frame
    quantmod.list <- lapply(quantmod.list, function(x) x[as.Date(rownames(x)) >= from & as.Date(rownames(x)) <= to, ])
    
    # Get updated dates
    dates <- lapply(quantmod.list, function(x) as.Date(rownames(x)))
    start.dates <- as.Date(unlist(lapply(dates, function(x) as.character(x[1]))))
    end.dates <- as.Date(unlist(lapply(dates, function(x) as.character(rev(x)[1]))))
    
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


capm.daily <- function(tickers, index.ticker = "SPY", from = NULL, to = NULL,
                       weights = NULL, weights.popmoments = TRUE, 
                       align.all = TRUE, decimals = getOption("digits"), 
                       plot.characteristic = TRUE) {
  
  # Check that inputs are valid
  if (! all(is.character("tickers"))) {
    stop("For tickers input, please enter a character string like 'AAPL' for Apple")
  }
  if (! is.character(index.ticker)) {
    stop("For index.ticker input, please enter a character string like 'SPY' for SPDR S&P 500 Trust ETF")
  }
  if (!is.null(from) && ! class(from) %in% c("character", "Date")) {
    stop("For from input, please enter a date or a character string that looks like a date (e.g. '2010-01-04' for January 4, 2010)")
  }
  if (!is.null(to) && ! class(to) %in% c("character", "Date")) {
    stop("For to input, please enter a date or a character string that looks like a date (e.g. '2015-03-09' for March 9, 2015)")
  }
  if (!is.null(weights) && ! (length(weights) == (length(tickers) + 1) | all(weights >= 0 & weights <= 1))) {
    stop("For weights input, please enter vector of weights for index.ticker and tickers that add to 1")
  }
  if (! all(is.logical(align.all))) {
    stop("For align.all input, please enter TRUE or FALSE")
  }
  if (decimals < 0 | ! floor(decimals) == decimals) {
    stop("For decimals input, please enter a whole number greater than or equal to 0")
  }
  if (! is.logical(plot.characteristic)) {
    stop("For plot.characteristic input, please enter TRUE or FALSE")
  }
  
  # If weights not specified, set to equal weights for each fund
  if (is.null(weights)) {
    weights <- rep(1 / (length(tickers) + 1), (length(tickers) + 1))
  }
  
  # Combine index.ticker and tickers into single character string
  index.tickers <- c(index.ticker, tickers)
  
  # If to not specified, set to current date
  if (is.null(to)) {
    today <- Sys.Date()
    to <- today
  }
  
  # If from not specified, set to 1 year ago
  if (is.null(from)) {
    from <- as.Date(paste(year(today)-1, month(today), day(today), sep = "-")) - 1
  }
  
  # Download stock prices for tickers from Yahoo! Finance, using the quantmod package
  prices.list <- list()
  for (ii in 1:length(index.tickers)) {
    temp.prices <- getSymbols(Symbols = index.tickers[ii], from = from, to = to, auto.assign = FALSE, warnings = FALSE)
    prices.list[[ii]] <- as.matrix(adjustOHLC(x = temp.prices, symbol.name = index.tickers[ii]))
  }
  names(prices.list) <- index.tickers
  
  # Get dates for each fund
  dates <- lapply(prices.list, function(x) as.Date(rownames(x)))
  start.dates <- as.Date(unlist(lapply(dates, function(x) as.character(x[1]))))
  end.dates <- as.Date(unlist(lapply(dates, function(x) as.character(rev(x)[1]))))
  lengths <- unlist(lapply(prices.list, nrow))
  
  if (align.all) {
    
    # If start/end dates are different, get subset of data with mutual lifetimes
    if (length(unique(start.dates)) > 1 | length(unique(end.dates)) > 1) {
      
      # Update from and to dates
      from <- max(start.dates)
      to <- min(end.dates)
      
      # Change matrices within prices.list to all cover the same time frame
      prices.list <- lapply(prices.list, function(x) x[as.Date(rownames(x)) >= from & as.Date(rownames(x)) <= to, ])
      
      # Get updated dates
      dates <- lapply(prices.list, function(x) as.Date(rownames(x)))
      start.dates <- as.Date(unlist(lapply(dates, function(x) as.character(x[1]))))
      end.dates <- as.Date(unlist(lapply(dates, function(x) as.character(rev(x)[1]))))
      lengths <- unlist(lapply(prices.list, nrow))
      
    }
    
  } else {
    
    # Add rows of NA's to top of funds that have less data than the longest one
    for (ii in 1:length(prices.list)) {
      
      length.diff <- max(lengths) - lengths[ii]
      if (length.diff > 0) {
        
        prices.list[[ii]] <- rbind(prices.list[[which.max(lengths)]][1:length.diff, ], prices.list[[ii]])
        prices.list[[ii]][1:length.diff, ] <- NA
        
      }
      
    }
    
  }
  
  # Verify that dates match up for all tickers
  dates1 <- rownames(prices.list[[1]])
  for (ii in 2:length(index.tickers)) {
    dates2 <- rownames(prices.list[[ii]])
    if (! all(dates1 == dates2)) {
      stop("Unfortunately the dates for various tickers don't perfectly match, so cannot calculate correlations")
    }
  }
  
  # Create prices and highs/lows matrices
  prices <- matrix(unlist(lapply(prices.list, function(x) x[, 6])), ncol = length(prices.list), byrow = FALSE)
  highs <- matrix(unlist(lapply(prices.list, function(x) x[, 2])), ncol = length(prices.list), byrow = FALSE)
  lows <- matrix(unlist(lapply(prices.list, function(x) x[, 3])), ncol = length(prices.list), byrow = FALSE)
  colnames(prices) <- index.tickers
  colnames(highs) <- index.tickers
  colnames(lows) <- index.tickers
  
  # Calculate daily gains
  gains <- apply(prices, 2, pchanges)
  colnames(gains) <- index.tickers
  
  # Get data on index.ticker throughout its history to get its mean and variance
  if (index.ticker == "^GSPC") {
    index.moments <- c(0.0003406497, 0.000093749092)
  } else {
    index.prices <- as.matrix(getSymbols(Symbols = index.ticker, from = "1950-01-01", to = to, auto.assign = FALSE, warnings = FALSE))
    gains2 <- pchanges(index.prices[, 6])
    index.moments <- c(mean(gains2), var(gains2))
  }
  
  # Loop through and calculate metrics for each fund
  fit <- list()
  resid <- matrix(NA, nrow = nrow(gains), ncol = length(index.tickers))
  colnames(resid) <- index.tickers
  #resid <- matrix(NA, nrow = nrow(gains), ncol = length(tickers) + 3)
  #resid <- data.frame(matrix(NA, nrow = nrow(gains), ncol = length(tickers) + 3))
  metrics <- data.frame(matrix(NA, nrow = length(prices.list), ncol = 22))
  names(metrics) <- c("ticker", "from", "to", "growth", "cagr", "cdgr", "xbar", "s2", "sharpe", "sortino", "mdd", "rrr", 
                      "alpha", "alpha.p", "beta", "beta.p", "alpha2", "alpha2.p", "r.squared", "expectation", "variance", "esd")
  
  for (ii in 1:length(prices.list)) {
    
    metrics[ii, 1] <- index.tickers[ii]
    metrics[ii, 2] <- as.character(start.dates[ii])
    metrics[ii, 3] <- as.character(end.dates[ii])
    metrics[ii, 4:12] <- round(c(gains.rate(gains = gains[, ii], nas = TRUE), 
                                 gains.rate(gains = gains[, ii], 251, nas = TRUE),
                                 gains.rate(gains = gains[, ii], 1, nas = TRUE),
                                 mean(gains[, ii], na.rm = T),
                                 var(gains[, ii], na.rm = T),
                                 sharpe.ratio(gains = gains[, ii], nas = TRUE), 
                                 sortino.ratio(gains = gains[, ii], nas = TRUE),
                                 mdd(highs = highs[, ii], lows = lows[, ii], nas = TRUE),
                                 rrr(gains = gains[, ii], highs = highs[, ii], lows = lows[, ii], nas = TRUE)), decimals)
    
    # CAPM metrics
    current.fit <- lm(gains[, ii] ~ gains[, 1])
    fit[[ii]] <- current.fit
    summary.fit <- summary(current.fit)
    cov.fit <- vcov(current.fit)
    resid[, ii] <- c(rep(NA, max(lengths)-lengths[ii]), summary.fit$residuals)
    
    r2 <- summary.fit$r.squared
    alpha <- summary.fit$coef[1, 1]
    alpha.p <- pnorm(-abs(alpha / summary.fit$coef[1, 2])) * 2
    beta <- summary.fit$coef[2, 1]
    beta.p <- pnorm(-abs(beta / summary.fit$coef[2, 2])) * 2
    
    alpha2 <- alpha + beta * index.moments[1] - index.moments[1]
    var.alpha2 <- cov.fit[1, 1] + index.moments[1]^2 * cov.fit[2, 2] + 2 * index.moments[1] * cov.fit[1, 2]
    alpha2.p <- pnorm(-abs(alpha2 / sqrt(var.alpha2))) * 2
    
    expectation <- alpha2 + index.moments[1]
    variance <- anova(current.fit)[2, 3] + beta^2 * index.moments[2]
    
    esd <- (alpha2 + index.moments[1]) / sqrt(variance)
    
    metrics[ii, 13:22] <- round(c(alpha, alpha.p, beta, beta.p, alpha2, alpha2.p, r2, expectation, variance, esd), decimals)
    
  }
  
  # Replace certain index metrics with NA's
  metrics[1, c(14, 16, 18)] <- NA
  
  # Create covariance matrices
  cov.samplemoments <- cov(gains, use = "pairwise.complete.obs")
  cov.popmoments <- matrix(NA, nrow = length(prices.list), ncol = length(prices.list))
  diag(cov.popmoments) <- metrics$variance
  for (ii in 1:(length(prices.list)-1)) {
    for (jj in (ii+1): length(prices.list)) {
      cov.popmoments[ii, jj] <- cov.popmoments[jj, ii] <- metrics$beta[ii]*metrics$beta[jj]*index.moments[2] + cov(resid[, ii], resid[, jj], use = "complete.obs")
    }
  }
  colnames(cov.popmoments) <- rownames(cov.popmoments) <- index.tickers
  
  # Assign cov.samplemoments of cov.popmoments to cov.matrix depending on cov.weights input
  if (weights.popmoments) {
    cov.matrix <- cov.samplemoments
  } else {
    cov.matrix <- cov.popmoments
  }
  
  # Figure out optimal weights
  f <- function(c) {
    
    esd <- (t(c) %*% metrics$expectation) / sqrt(t(c) %*% cov.matrix %*% c)
    return(-esd)
    
  }
  weights.pre <- nlminb(start = rep(1/length(prices.list), length(prices.list)), objective = f, lower = 0, upper = 1)
  weights.final <- matrix(weights.pre$par / sum(weights.pre$par), ncol = 1)
  
  # Calculate growth of $100 in each fund including weighted funds
  growth100 <- apply(gains[complete.cases(gains), ], 2, function(x) balances(ratios = x + 1, initial = 100))
  growth100 <- cbind(growth100, growth100 %*% weights, growth100 %*% weights.final)
  colnames(growth100) <- c(index.tickers, "entered.weights", "optimal.weights")
  
  # Calculate gains for weighted funds
  weighted.gains <- apply(growth100, 2, pchanges)[, -(2:length(index.tickers))]
  colnames(weighted.gains) <- c(index.ticker, "input.weights", "optimal.weights")
  
  new.rows <- as.data.frame(matrix(NA, nrow = 2, ncol = 22))
  names(new.rows) <- names(metrics)
  metrics <- rbind(metrics, new.rows)
  new.tickers <- c(index.ticker, "input.weights", "optimal.weights")
  new.dates <- rep(dates[which.min(lengths)], 3) 
  
  for (ii in 2:3) {
    
    ind <- ii + length(index.tickers) - 1
    
    metrics[ind, 1] <- new.tickers[ii]
    metrics[ind, 2] <- as.character(max(start.dates))
    metrics[ind, 3] <- as.character(min(end.dates))
    metrics[ind, 4:12] <- round(c(gains.rate(gains = weighted.gains[, ii], nas = TRUE),
                                  gains.rate(gains = weighted.gains[, ii], 251, nas = TRUE),
                                  gains.rate(gains = weighted.gains[, ii], 1, nas = TRUE),
                                  mean(weighted.gains[, ii], na.rm = T),
                                  var(weighted.gains[, ii], na.rm = T),
                                  sharpe.ratio(gains = weighted.gains[, ii], nas = TRUE), 
                                  sortino.ratio(gains = weighted.gains[, ii], nas = TRUE),
                                  mdd(gains = weighted.gains[, ii], nas = TRUE),
                                  rrr(gains = weighted.gains[, ii], nas = TRUE)), decimals)
    
    # CAPM metrics
    current.fit <- lm(weighted.gains[, ii] ~ weighted.gains[, 1])
    fit[[ind]] <- current.fit
    summary.fit <- summary(current.fit)
    cov.fit <- vcov(current.fit)
    
    r2 <- summary.fit$r.squared
    alpha <- summary.fit$coef[1, 1]
    alpha.p <- pnorm(-abs(alpha / summary.fit$coef[1, 2])) * 2
    beta <- summary.fit$coef[2, 1]
    beta.p <- pnorm(-abs(beta / summary.fit$coef[2, 2])) * 2
    
    alpha2 <- alpha + beta * index.moments[1] - index.moments[1]
    var.alpha2 <- cov.fit[1, 1] + index.moments[1]^2 * cov.fit[2, 2] + 2 * index.moments[1] * cov.fit[1, 2]
    alpha2.p <- pnorm(-abs(alpha2 / sqrt(var.alpha2))) * 2
    
    expectation <- alpha2 + index.moments[1]
    variance <- anova(current.fit)[2, 3] + beta^2 * index.moments[2]
    
    esd <- (alpha2 + index.moments[1]) / sqrt(variance)
    
    metrics[ind, 13:22] <- round(c(alpha, alpha.p, beta, beta.p, alpha2, alpha2.p, r2, expectation, variance, esd), decimals)
    
  }
  
  weights.final <- as.vector(weights.final)
  names(weights.final) <- index.tickers
  ret <- list(dates = dates,
              growth100 = growth100,
              metrics = metrics,
              fit = fit,
              cov.matrix = cov.matrix,
              weights.final = weights.final)
  
  # Plot results if requested
  if (plot.characteristic) {
    
    if (length(tickers) == 1) {
      
      # Create titles
      plot.title <- paste("Characteristic Line for ", tickers, sep = "")
      y.title <- paste("Daily gain (%)", sep = "")
      x.title <- paste(index.ticker, " gain (%)", sep = "")
      
      # Get min and max for index.ticker and ticker
      xrange <- range(gains[, 1]) * 100
      yrange <- range(gains[, 2]) * 100
      
      plot(NULL, NULL, main = plot.title, ylab = y.title, xlab = x.title,
           xlim = xrange, ylim = yrange)
      points(gains[, 1] * 100, gains[, 2] * 100, pch = 16)
      points(gains[, 1] * 100, predict(lm(gains[, 2] ~ gains[, 1])) * 100, type = "l", col = "red")
      abline(h = 0, lty = 3, col = "black")
      abline(v = 0, lty = 3, col = "black")
      if (fit[[2]]$coef[2] >= 0) {
        legend("topleft", legend = paste("Y = ", sprintf("%.3f", fit[[2]]$coef[1] * 100), 
                                         " + ", sprintf("%.3f", fit[[2]]$coef[2]),"X", sep = ""),
               lty = 1, col = "red", bg = "white")
      } else {
        legend("topleft", legend = paste("Y = ", sprintf("%.3f", fit[[2]]$coef[1] * 100), 
                                         " - ", sprintf("%.3f", abs(fit[[2]]$coef[2])),"X", sep = ""),
               lty = 1, col = "red", bg = "white")
      }
      
    } else {
      
      # Create titles
      plot.title <- paste("Characteristic Lines")
      y.title <- paste("Daily gain (%)", sep = "")
      x.title <- paste(index.ticker, " gain (%)", sep = "")
      
      plot(NULL, NULL, main = "Characteristic Lines", ylab = y.title, xlab = x.title,
           xlim = c(-0.05, 0.05), ylim = c(-0.05, 0.05))
      x <- c(-0.05, 0.05)
      points(x, x, type = "l", col = "red")
      for (ii in 2:(length(tickers) + 1)) {
        
        y <- metrics$alpha[ii] + metrics$beta[ii] * x
        points(x, y, col = "purple", type = "l")
        
      }
      y <- rev(metrics$alpha)[1] + rev(metrics$beta)[1] * x
      points(x, y, col = "darkblue", type = "l", lwd = 2.5)
      lines(x = c(-0.05, 0.05), y = c(0, 0), lty = 3, col = "black")
      lines(x = c(0, 0), y = c(-0.05, 0.05), lty = 3, col = "black")
      legend("topleft", legend = c("Individual funds", "Optimally weighted"), lty = 1, lwd = c(1, 2.5), col = "purple", bg = "white")
      
    }
    
  }
  
  # Return object
  return(ret)
  
}


beta.trailing50 <- function(ticker) {
  
  # Download stock prices from Yahoo! Finance, using the quantmod package
  ticker.prices <- as.matrix(getSymbols(Symbols = ticker, from = Sys.Date()-90, auto.assign = FALSE, warnings = FALSE))
  spy.prices <- as.matrix(getSymbols(Symbols = "SPY", from = as.Date(rownames(ticker.prices)[1]), 
                                     to = as.Date(rownames(ticker.prices)[nrow(ticker.prices)]), 
                                     auto.assign = FALSE, warnings = FALSE))
  
  # Verify that all dates match
  if (!all (rownames(ticker.prices) == rownames(spy.prices))) {
    stop(paste("SPY closing prices don't match dates of ", ticker, " closing prices", sep = ""))
  }
  
  # Calculate and return ticker's beta
  locs.last51 <- (nrow(ticker.prices) - 50): nrow(ticker.prices)
  ticker.gains <- pchanges(ticker.prices[locs.last51, 6])
  spy.gains <- pchanges(spy.prices[locs.last51, 6])
  ticker.beta <- as.numeric(lm(ticker.gains ~ spy.gains)$coef[2])
  return(ticker.beta)
  
}


historical.performance <- function(tickers, from = NULL, to = NULL, weights = NULL, 
                                   reference.tickers = "SPY", plot.growth = TRUE) {
  
  # Use default values for from and to if unspecified
  if (is.null(from)) {
    from <- as.Date(paste(year(Sys.Date()) - 1, month(Sys.Date()), day(Sys.Date()), sep = "-")) - 1
  }
  if (is.null(to)) {
    to <- Sys.Date()
  }
  
  # If weights do not add to 1, make them add to 1
  if (!is.null(weights)) {
    if (sum(weights) != 1) {
      weights <- weights / sum(weights)
    }
  }
  
  # Load in historical data for each ticker as a list
  ticker.list <- list()
  for (ii in 1:length(tickers)) {
    ticker.list[[ii]] <- as.matrix(getSymbols(tickers[ii], from = from, to = to, auto.assign = FALSE))
  }
  
  # If not all tickers have data of the same length, output error
  if (length(unique(unlist(lapply(ticker.list, nrow)))) > 1) {
    stop("The dates don't match up for the various tickers")
  }
  
  # Using equal weights if unspecified
  if (is.null(weights)) {
    weights <- rep(1 / length(tickers), length(tickers))
  }
  
  # Calculate daily gains for each fund
  gains <- matrix(unlist(lapply(ticker.list, function(x) pchanges(x[, 6]))), ncol = length(tickers), byrow = FALSE)
  colnames(gains) <- tickers
  
  # Calculate portfolio gains, if more than 1 ticker entered
  if (length(tickers) > 1) {
    portfolio.gains <- gains %*% weights
  }
  
  # Add gains data for reference tickers, if any
  if (length(reference.tickers) > 0) {
    ref.ticker.list <- list()
    for (ii in 1:length(reference.tickers)) {
      ref.ticker.list[[ii]] <- as.matrix(getSymbols(reference.tickers[ii], from = from, to = to, auto.assign = FALSE))
    }
    ref.ticker.gains <- matrix(unlist(lapply(ref.ticker.list, function(x) pchanges(x[, 6]))), ncol = length(reference.tickers), byrow = FALSE)
    gains <- cbind(ref.ticker.gains, gains)
    colnames(gains)[1:length(reference.tickers)] <- reference.tickers
  }
  
  # Add gains data for portfolio, if more than 1 ticker entered
  if (length(tickers) > 1) {
    gains <- cbind(portfolio.gains, gains)
    colnames(gains)[1] <- "Portfolio"
  }
  
  # Calculate growth of 10k
  growth10k <- apply(gains, 2, function(x) balances(ratios = x + 1))
  
  # Create matrix of summary statistics for each ticker
  performance <- data.frame(fund = colnames(gains),
                            growth = apply(gains, 2, gains.rate),
                            cagr = apply(gains, 2, function(x) gains.rate(x, xday.rate = 251)),
                            mdd = apply(gains, 2, function(x) mdd(gains = x)),
                            sharpe = apply(gains, 2, function(x) sharpe.ratio(gains = x)),
                            sortino = apply(gains, 2, function(x) sortino.ratio(gains = x)),
                            row.names = NULL)
  
  # Create plot showing growth of $10k, if requested
  if (plot.growth) {
    
    max.y <- max(growth10k)
    my.cols <- brewer.pal(n = ncol(growth10k), name = "RdYlBu")
    plot(1:nrow(growth10k), growth10k[, 1], type = "n", ylim = c(0, max.y * 1.05),
         main = "Growth of $10k", xlab = "Trading day", ylab = "Balance ($)")
    for (ii in 1: ncol(growth10k)) {
      points(1:nrow(growth10k), growth10k[, ii], type = "l", col = my.cols[ii])
    }
    grid()
    legend("bottomleft", legend = colnames(growth10k), lty = 1, col = my.cols, bg = "white")
    growth10k.plot <- recordPlot()
    
  }
  
  # Return list with relevant information
  if (plot.growth) {
    ret.list <- list(performance = performance,
                     gains = gains,
                     growth10k = growth10k,
                     growth10k.plot = growth10k.plot)
  } else {
    ret.list <- list(performance = performance,
                     gains = gains,
                     growth10k = growth10k)
  }
  return(ret.list)
  
}


twofund.portfolio <- function(tickers, index.ticker = "SPY", from = NULL, to = NULL,
                              reference.tickers = "SPY", plots = TRUE) {
  
  # Use default values for from and to if unspecified
  if (is.null(from)) {
    from <- as.Date(paste(year(Sys.Date()) - 1, month(Sys.Date()), day(Sys.Date()), sep = "-")) - 1
  }
  if (is.null(to)) {
    to <- Sys.Date()
  }
  
  # Load in historical data for each ticker as a list and verify that they have the same length
  ticker.list <- list()
  for (ii in 1:length(tickers)) {
    ticker.list[[ii]] <- as.matrix(getSymbols(tickers[ii], from = from, to = to, auto.assign = FALSE))
  }
  ticker.lengths <- unique(unlist(lapply(ticker.list, nrow)))
  if (length(ticker.lengths) > 1) {
    stop("The dates don't match up for the two tickers")
  }
  
  # Load in historical data for index and check that it has the correct length
  index.prices <- as.matrix(getSymbols(index.ticker, from = from, to = to, auto.assign = FALSE))
  if (nrow(index.prices) != ticker.lengths) {
    stop("The dates for index.ticker do not match the dates for the two tickers")
  }
  
  # Load in historical data for reference tickers and check that they have the correct length
  if (length(reference.tickers) > 0) {
    ref.ticker.list <- list()
    for (ii in 1:length(reference.tickers)) {
      ref.ticker.list[[ii]] <- as.matrix(getSymbols(reference.tickers[ii], from = from, to = to, auto.assign = FALSE))
    }
    ref.ticker.lengths <- unique(unlist(lapply(ref.ticker.list, nrow)))
    if (length(ref.ticker.lengths) > 1 | ref.ticker.lengths[1] != ticker.lengths) {
      stop("The dates for reference.tickers do not match the dates for the two tickers")
    }
  }
  
  # Calculate daily gains for the tickers, index, and reference tickers
  ticker.gains <- matrix(unlist(lapply(ticker.list, function(x) pchanges(x[, 6]))), ncol = length(tickers), byrow = FALSE)
  colnames(ticker.gains) <- tickers
  index.gains <- pchanges(index.prices[, 6])
  if (length(reference.tickers) > 0) {
    ref.ticker.gains <- matrix(unlist(lapply(ref.ticker.list, function(x) pchanges(x[, 6]))), ncol = length(reference.tickers), byrow = FALSE)
    colnames(ref.ticker.gains) <- reference.tickers
  }
  
  # Calculate properties of two-fund portfolio with various weights
  alls <- seq(0, 1, 0.001)
  portfolio.stats <- matrix(NA, nrow = length(alls), ncol = 13)
  for (ii in 1:length(alls)) {
    
    portfolio.gains <- ticker.gains %*% c(alls[ii], 1 - alls[ii])
    characteristic.fit <- lm(portfolio.gains ~ index.gains)
    
    portfolio.stats[ii, ] <- c(alls[ii],
                               1 - alls[ii],
                               gains.rate(gains = portfolio.gains),
                               gains.rate(gains = portfolio.gains, xday.rate = 251),
                               mdd(gains = portfolio.gains),
                               mean(portfolio.gains),
                               sd(portfolio.gains),
                               sharpe.ratio(gains = portfolio.gains),
                               sortino.ratio(gains = portfolio.gains),
                               characteristic.fit$coef[1],
                               summary(characteristic.fit)$coef[1, 4],
                               characteristic.fit$coef[2],
                               summary(characteristic.fit)$coef[2, 4])
    
  }
  portfolio.stats <- as.data.frame(portfolio.stats)
  names(portfolio.stats) <- c(paste(tickers, "alloc", sep = "."), "growth",
                              "cagr", "mdd", "mean", "sd", "sharpe", "sortino",
                              "alpha", "alpha.p", "beta", "beta.p")
  
  # Calculate properties of reference tickers
  if (length(reference.tickers) > 0) {
    reference.stats <- matrix(NA, nrow = length(reference.tickers), ncol = 11)
    for (ii in 1:length(reference.tickers)) {
      
      characteristic.fit <- lm(ref.ticker.gains[, ii] ~ index.gains)
      reference.stats[ii, ] <- c(gains.rate(gains = ref.ticker.gains[, ii]),
                                 gains.rate(gains = ref.ticker.gains[, ii], xday.rate = 251),
                                 mdd(gains = ref.ticker.gains[, ii]),
                                 mean(ref.ticker.gains[, ii]),
                                 sd(ref.ticker.gains[, ii]),
                                 sharpe.ratio(gains = ref.ticker.gains[, ii]),
                                 sortino.ratio(gains = ref.ticker.gains[, ii]),
                                 characteristic.fit$coef[1],
                                 summary(characteristic.fit)$coef[1, 4],
                                 characteristic.fit$coef[2],
                                 summary(characteristic.fit)$coef[2, 4])
      
    }
    reference.stats <- data.frame(ticker = reference.tickers,
                                  reference.stats, stringsAsFactors = FALSE)
    names(reference.stats) <- c("ticker", "growth", "cagr", "mdd", "mean", "sd", "sharpe", "sortino",
                                "alpha", "alpha.p", "beta", "beta.p")
  }
  
  # Create plots if requested
  if (plot) {
  
    # CAGR vs. MDD
    plot(portfolio.stats[, "mdd"] * 100, portfolio.stats[, "cagr"] * 100, type = "l",
         main = "CAGR vs. MDD", xlab = "MDD (%)", ylab = "CAGR (%)")
    loc.10s <- seq(1, 1001, 100)
    points(portfolio.stats[loc.10s, "mdd"] * 100, portfolio.stats[loc.10s, "cagr"] * 100, type = "p", pch = 16, col = c("blue", rep("black", 9), "red"))
    if (length(reference.stats) > 0) {
      my.cols <- brewer.pal(n = max(3, nrow(reference.stats)), name = "Spectral")
      for (ii in 1:nrow(reference.stats)) {
        points(reference.stats[ii, "mdd"] * 100, reference.stats[ii, "cagr"] * 100, type = "p", pch = 16, col = my.cols[ii])
      }
      legend("topleft", legend = c(paste("100%", rev(tickers), sep = " "), reference.stats[, 1]), col = c("blue", "red", my.cols), pch = 16, bg = "white")
    } else {
      legend("topleft", legend = paste("100%", rev(tickers), sep = " "), col = c("blue", "red"), pch = 16, bg = "white")
    }
    plot.cagr.mdd <- recordPlot()
    
    # Mean vs. SD
    plot(portfolio.stats[, "sd"] * 100, portfolio.stats[, "mean"] * 100, type = "l",
         main = "Daily Gains, Mean vs. SD", xlab = "SD (%)", ylab = "Mean (%)")
    points(portfolio.stats[loc.10s, "sd"] * 100, portfolio.stats[loc.10s, "mean"] * 100, type = "p", pch = 16, col = c("blue", rep("black", 9), "red"))
    if (length(reference.stats) > 0) {
      for (ii in 1:nrow(reference.stats)) {
        points(reference.stats[ii, "sd"] * 100, reference.stats[ii, "mean"] * 100, type = "p", pch = 16, col = my.cols[ii])
      }
      legend("topleft", legend = c(paste("100%", rev(tickers), sep = " "), reference.stats[, 1]), col = c("blue", "red", my.cols), pch = 16, bg = "white")
    } else {
      legend("topleft", legend = paste("100%", rev(tickers), sep = " "), col = c("blue", "red"), pch = 16, bg = "white")
    }
    plot.mean.sd <- recordPlot()

    # Sharpe vs. SD
    plot(portfolio.stats[, "sd"] * 100, portfolio.stats[, "sharpe"], type = "l",
         main = "Sharpe Ratio vs. SD of Daily Gains", xlab = "SD (%)", ylab = "Sharpe ratio") 
    points(portfolio.stats[loc.10s, "sd"] * 100, portfolio.stats[loc.10s, "sharpe"], type = "p", pch = 16, col = c("blue", rep("black", 9), "red"))
    if (length(reference.stats) > 0) {
      for (ii in 1:nrow(reference.stats)) {
        points(reference.stats[ii, "sd"] * 100, reference.stats[ii, "sharpe"], type = "p", pch = 16, col = my.cols[ii])
      }
      legend("topright", legend = c(paste("100%", rev(tickers), sep = " "), reference.stats[, 1]), col = c("blue", "red", my.cols), pch = 16, bg = "white")
    } else {
      legend("topright", legend = paste("100%", rev(tickers), sep = " "), col = c("blue", "red"), pch = 16, bg = "white")
      
    }
    plot.sharpe.sd <- recordPlot()
    
  }
  
  # Return list with relevant information
  if (! plots) {
    if (length(reference.tickers) == 0) {
      ret <- portfolio.stats
    } else {
      ret <- list(portfolio.stats = portfolio.stats,
                  reference.stats = reference.stats)
    }
  } else {
    if (length(reference.tickers) == 0) {
      ret <- list(portfolio.stats = portfolio.stats,
                  plot.cagr.mdd = plot.cagr.mdd,
                  plot.mean.sd = plot.mean.sd,
                  plot.sharpe.sd = plot.sharpe.sd)
      
    } else {
      ret <- list(portfolio.stats = portfolio.stats,
                  reference.stats = reference.stats,
                  plot.cagr.mdd = plot.cagr.mdd,
                  plot.mean.sd = plot.mean.sd,
                  plot.sharpe.sd = plot.sharpe.sd)
    }
  }
  return(ret)
  
}