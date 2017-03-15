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


convert.rate <- function(rate, units.in = 1, units.out = 1) {
  ((rate + 1)^(units.out / units.in)) - 1
}


daily.yearly <- function(daily.gain, years = 1) {
  (1 + daily.gain)^(252 * years) - 1
}


yearly.daily <- function(total.gain, years = 1) {
  (total.gain + 1)^(1/(252*years)) - 1
}


balances <- function(ratios, initial = 10000) {
  c(initial, initial * cumprod(ratios))
}


final.balance <- function(ratios, initial = 10000) {
  initial * prod(ratios)
}


ticker.dates <- function(tickers, from = "1900-01-01", to = Sys.Date()) {
  
  # Download stock prices for tickers from Yahoo! Finance, using the quantmod package
  prices <- list()
  for (ii in 1:length(tickers)) {
    prices[[ii]] <- as.matrix(getSymbols(Symbols = tickers[ii], from = from, to = to, auto.assign = FALSE, warnings = FALSE))
  }
  
  # Create object to return
  ret <- data.frame(ticker = tickers,
                    start.date = as.Date(unlist(lapply(prices, function(x) rownames(x)[1]))),
                    end.date = as.Date(unlist(lapply(prices, function(x) rev(rownames(x))[1]))),
                    days = unlist(lapply(prices, function(x) nrow(x))))
  return(ret)
  
}


load.gains <- function(tickers, intercepts = NULL, slopes = NULL, 
                       from = "1900-01-01", to = Sys.Date(), time.scale = "daily") {
  
  # Remove CASH if included in tickers
  cash.included <- "CASH" %in% tickers
  if (cash.included) {
    loc.cash <- which(tickers == "CASH")
    tickers <- tickers[-loc.cash]
  }
  
  # Download stock prices for tickers from Yahoo! Finance, using the quantmod package
  prices <- list()
  for (ii in 1:length(tickers)) {
    prices.fund <- as.matrix(getSymbols(Symbols = tickers[ii], from = from, to = to, auto.assign = FALSE, warnings = FALSE))
    prices[[ii]] <- prices.fund
    from <- max(as.Date(from), as.Date(rownames(prices.fund[1, , drop = F])))
  }
  
  # If more than 1 fund, align prices
  if (length(tickers) > 1) {
    
    # Align start dates
    start.dates <- as.Date(unlist(lapply(prices, function(x) rownames(x[1, , drop = F]))))
    if (length(unique(start.dates)) > 1) {
      latest.startdate <- max(start.dates)
      for (ii in 1: length(tickers)) {
        if (start.dates[ii] != latest.startdate) {
          prices.fund <- prices[[ii]]
          dates.fund <- as.Date(rownames(prices.fund))
          loc.start <- which(dates.fund == latest.startdate)
          prices.fund <- prices.fund[loc.start: nrow(prices.fund), ]
          prices[[ii]] <- prices.fund
        }
      }
    }
    
    # Align end dates
    end.dates <- as.Date(unlist(lapply(prices, function(x) rownames(x[nrow(x), , drop = F]))))
    if (length(unique(end.dates)) > 1) {
      earliest.enddate <- min(end.dates)
      for (ii in 1: length(tickers)) {
        if (end.dates[ii] != earliest.enddate) {
          prices.fund <- prices[[ii]]
          dates.fund <- as.Date(rownames(prices.fund))
          loc.end <- which(dates.fund == earliest.enddate)
          prices.fund <- prices.fund[1: loc.end, ]
          prices[[ii]] <- prices.fund
        }
      }
    }
    
    # Go through and remove any dates that don't match the others
    ii <- 2
    while (ii <= length(tickers)) {
      
      # Get price data for 1st and iith fund
      prices.fund1 <- prices[[1]]
      dates.fund1 <- as.Date(rownames(prices.fund1))
      
      prices.fund2 <- prices[[ii]]
      dates.fund2 <- as.Date(rownames(prices.fund2))
      
      # As long as at least 1 date doesn't match up, remove the unmatched date
      while (!suppressWarnings(all(dates.fund1 == dates.fund2))) {
        
        loc <- suppressWarnings(which(dates.fund1 != dates.fund2))[1]
        if (dates.fund1[loc] < dates.fund2[loc]) {
          message(paste("Dropped", dates.fund1[loc], "from", tickers[1], sep = " "))
          prices.fund1 <- prices.fund1[-loc, ]
          dates.fund1 <- dates.fund1[-loc]
          prices[[1]] <- prices.fund1
        } else {
          message(paste("Dropped", dates.fund2[loc], "from", tickers[ii], sep = " "))
          prices.fund2 <- prices.fund2[-loc, ]
          dates.fund2 <- dates.fund2[-loc]
          prices[[ii]] <- prices.fund2
        }
        ii <- 1
        
      }
      ii <- ii + 1
      
    }
    
  }
  
  # If specified, apply intercepts and slopes and convert back to prices
  if ((!is.null(intercepts) & !all(intercepts == 0)) | (!is.null(slopes) & !all(slopes == 1))) {
    
    # Set default values for intercepts/slopes if only the other is specified
    if (is.null(intercepts)) {
      intercepts <- rep(0, length(slopes))
    } else if (is.null(slopes)) {
      slopes <- rep(1, length(slopes))
    }
    
    for (ii in 1: length(prices)) {
      
      if (! (intercepts[ii] == 0 & slopes[ii] == 1)) {
        closing.prices <- prices[[ii]][, 6]
        prices[[ii]][, 6] <- balances(ratios = 1 + intercepts[ii] + slopes[ii] * pchanges(closing.prices), initial = closing.prices[1])
        tickers[ii] <- paste(tickers[ii], "(scaled)")
        # if (slopes[ii] != 1) {
        #   tickers[ii] <- paste(tickers[ii], ".", slopes[ii], "x", sep = "")
        # }
        # if (intercepts[ii] < 0) {
        #   tickers[ii] <- paste(tickers[ii], "-", round(abs(intercepts[ii]), 7), sep = "")
        # } else if (intercepts[ii] > 0) {
        #   tickers[ii] <- paste(tickers[ii], "+", round(intercepts[ii], 7), sep = "")
        # }
      }
      
    }
    
  }
  
  # Convert to prices on last day of month/year if requested
  dates <- as.Date(rownames(prices[[1]]))
  if (time.scale == "monthly") {
    locs <- which(diff(month(dates)) %in% c(1, -11))
    prices <- lapply(prices, function(x) x[locs, ])
    dates <- dates[locs]
    #dates <- sapply(dates, function(x) paste(unlist(strsplit(as.character(x), "-"))[-3], collapse = "-"))
  } else if (time.scale == "yearly") {
    locs <- which(diff(year(dates)) == 1)
    prices <- lapply(prices, function(x) x[locs, ])
    dates <- dates[locs]
    #dates <- year(dates[locs])
  }
  
  # Output message indicating date range
  message(paste("Results are for ", dates[1], " to " , dates[length(dates)], sep = ""))
  
  # Create and return matrix of gains
  gains.mat <- matrix(unlist(lapply(prices, function(x) pchanges(x[, 6]))), byrow = F, ncol = length(tickers))
  if (cash.included) {
    if (loc.cash == 1) {
      gains.mat <- cbind(rep(0, nrow(gains.mat)), gains.mat)
      colnames(gains.mat) <- c("CASH", tickers)
    } else {
      gains.mat <- cbind(gains.mat[, 1: (loc.cash - 1)], rep(0, nrow(gains.mat)), 
                         gains.mat[, loc.cash: ncol(gains.mat)])
      colnames(gains.mat) <- c(tickers[1: (loc.cash - 1)], "CASH", tickers[loc.cash: length(tickers)])
    } 
  } else {
    colnames(gains.mat) <- tickers
  }
  rownames(gains.mat) <- as.character(dates)[-1]
  return(gains.mat)
  
}


load.prices <- function(tickers, intercepts = NULL, slopes = NULL, 
                        from = "1900-01-01", to = Sys.Date(), time.scale = "daily", 
                        initial = NULL) {
  
  # Remove CASH if included in tickers
  cash.included <- "CASH" %in% tickers
  if (cash.included) {
    loc.cash <- which(tickers == "CASH")
    tickers <- tickers[-loc.cash]
  }
  
  # Download stock prices for tickers from Yahoo! Finance, using the quantmod package
  prices <- list()
  for (ii in 1:length(tickers)) {
    prices.fund <- as.matrix(getSymbols(Symbols = tickers[ii], from = from, to = to, auto.assign = FALSE, warnings = FALSE))
    prices[[ii]] <- prices.fund
    from <- max(as.Date(from), as.Date(rownames(prices.fund[1, , drop = F])))
  }
  
  # If more than 1 fund, align prices
  if (length(tickers) > 1) {
    
    # Align start dates
    start.dates <- as.Date(unlist(lapply(prices, function(x) rownames(x[1, , drop = F]))))
    if (length(unique(start.dates)) > 1) {
      latest.startdate <- max(start.dates)
      for (ii in 1: length(tickers)) {
        if (start.dates[ii] != latest.startdate) {
          prices.fund <- prices[[ii]]
          dates.fund <- as.Date(rownames(prices.fund))
          loc.start <- which(dates.fund == latest.startdate)
          prices.fund <- prices.fund[loc.start: nrow(prices.fund), ]
          prices[[ii]] <- prices.fund
        }
      }
    }
    
    # Align end dates
    end.dates <- as.Date(unlist(lapply(prices, function(x) rownames(x[nrow(x), , drop = F]))))
    if (length(unique(end.dates)) > 1) {
      earliest.enddate <- min(end.dates)
      for (ii in 1: length(tickers)) {
        if (end.dates[ii] != earliest.enddate) {
          prices.fund <- prices[[ii]]
          dates.fund <- as.Date(rownames(prices.fund))
          loc.end <- which(dates.fund == earliest.enddate)
          prices.fund <- prices.fund[1: loc.end, ]
          prices[[ii]] <- prices.fund
        }
      }
    }
    
    # Go through and remove any dates that don't match the others
    ii <- 2
    while (ii <= length(tickers)) {
      
      # Get price data for 1st and iith fund
      prices.fund1 <- prices[[1]]
      dates.fund1 <- as.Date(rownames(prices.fund1))
      
      prices.fund2 <- prices[[ii]]
      dates.fund2 <- as.Date(rownames(prices.fund2))
      
      # As long as at least 1 date doesn't match up, remove the unmatched date
      while (!suppressWarnings(all(dates.fund1 == dates.fund2))) {
        
        loc <- suppressWarnings(which(dates.fund1 != dates.fund2))[1]
        if (dates.fund1[loc] < dates.fund2[loc]) {
          message(paste("Dropped", dates.fund1[loc], "from", tickers[1], sep = " "))
          prices.fund1 <- prices.fund1[-loc, ]
          dates.fund1 <- dates.fund1[-loc]
          prices[[1]] <- prices.fund1
        } else {
          message(paste("Dropped", dates.fund2[loc], "from", tickers[ii], sep = " "))
          prices.fund2 <- prices.fund2[-loc, ]
          dates.fund2 <- dates.fund2[-loc]
          prices[[ii]] <- prices.fund2
        }
        ii <- 1
        
      }
      ii <- ii + 1
      
    }
    
  }
  
  # Convert to prices on last day of month/year if requested
  dates <- as.Date(rownames(prices[[1]]))
  if (time.scale == "monthly") {
    locs <- which(diff(month(dates)) %in% c(1, -11))
    prices <- lapply(prices, function(x) x[locs, ])
    dates <- dates[locs]
    #dates <- sapply(dates, function(x) paste(unlist(strsplit(as.character(x), "-"))[-3], collapse = "-"))
  } else if (time.scale == "yearly") {
    locs <- which(diff(year(dates)) == 1)
    prices <- lapply(prices, function(x) x[locs, ])
    dates <- dates[locs]
    #dates <- year(dates[locs])
  }
  
  # Output message indicating date range
  message(paste("Results are for ", dates[1], " to " , dates[length(dates)], sep = ""))
  
  # Create matrix of closing prices
  closing.prices <- matrix(unlist(lapply(prices, function(x) x[, 6])), byrow = F, ncol = length(tickers))
  if (cash.included) {
    if (loc.cash == 1) {
      closing.prices <- cbind(rep(1, nrow(closing.prices)), closing.prices)
      colnames(closing.prices) <- c("CASH", tickers)
    } else {
      closing.prices <- cbind(closing.prices[, 1: (loc.cash - 1)], rep(1, nrow(closing.prices)), 
                              closing.prices[, loc.cash: ncol(closing.prices)])
      colnames(closing.prices) <- c(tickers[1: (loc.cash - 1)], "CASH", tickers[loc.cash: length(tickers)])
    }
  } else {
    colnames(closing.prices) <- tickers
  }
  rownames(closing.prices) <- as.character(dates)
  
  # If intercepts and slopes specified, convert to gains, scale gains, and convert back to prices
  if ((!is.null(intercepts) & !all(intercepts == 0)) | (!is.null(slopes) & !all(slopes == 1))) {
    
    # If intercepts or slopes NULL, set to matrix of 0's and 1's, respectively
    if (is.null(intercepts)) {
      intercepts <- rep(0, length(tickers))
    }
    if (is.null(slopes)) {
      slopes <- rep(1, length(tickers))
    }
    
    # Scale each column of closing prices
    for (ii in 1: length(tickers)) {
      if (intercepts[ii] != 0 | slopes[ii] != 1) {
        closing.prices[, ii] <- balances(ratios = 1 + intercepts[ii] + slopes[ii] * pchanges(closing.prices[, ii]),
                                         initial = closing.prices[1, ii])
        tickers[ii] <- paste(tickers[ii], "(scaled)")
      }
      # if (slopes[ii] != 1) {
      #   tickers[ii] <- paste(tickers[ii], ".", slopes[ii], "x", sep = "")
      # }
      # if (intercepts[ii] < 0) {
      #   tickers[ii] <- paste(tickers[ii], "-", round(abs(intercepts[ii]), 7), sep = "")
      # } else if (intercepts[ii] > 0) {
      #   tickers[ii] <- paste(tickers[ii], "+", round(intercepts[ii], 7), sep = "")
      # }
    }
    colnames(closing.prices) <- tickers
    
  }
  
  # Scale prices to same initial value if requested
  if (!is.null(initial)) {
    
    closing.prices <- apply(closing.prices, 2, function(x) x / x[1] * initial)
    
  }
  
  # Return closing prices
  return(closing.prices)
  
}


prices.rate <- function(prices, units.rate = NULL, nas = FALSE) {
  
  # Drop NA's
  if (nas) {
    prices <- prices[!is.na(prices)]
  }
  
  # Get the overall growth rate
  prices.length <- length(prices)
  rate1 <- prices[prices.length] / prices[1] - 1
  
  # Convert to x-unit growth rate if units.rate is specified
  if (! is.null(units.rate) && units.rate != prices.length - 1) {
    rate1 <- convert.rate(rate = rate1, units.in = prices.length - 1, units.out = units.rate)
  }
  
  # Return the rate
  return(rate1)
  
}


gains.rate <- function(gains, units.rate = NULL, nas = FALSE) {
  
  # Drop NA's
  if (nas) {
    gains <- gains[!is.na(gains)]
  }
  
  # Get the overall growth rate
  gains.length <- length(gains)
  rate1 <- prod(gains + 1) - 1
  
  # Convert to x-unit growth rate if xunit.rate is specified
  if (! is.null(units.rate) && ! (units.rate == gains.length)) {
    rate1 <- convert.rate(rate = rate1, units.in = gains.length, units.out = units.rate)
  }
  
  # Return the rate
  return(rate1)
  
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
  
  # Calculate and return Sharpe ratio
  return((mean(gains) - rf) / sd(gains))
  
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
  
  # Calculate and return risk-return ratio
  return(ret / max.dd)
  
}


metrics <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                    gains = NULL, prices = NULL, 
                    from = "1900-01-01", to = Sys.Date(),
                    benchmark.fund = NULL, time.scale = "daily") {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {
    
    # Obtain matrix of gains for each fund
    gains <- load.gains(tickers = tickers, from = from, to = to,
                        intercepts = intercepts, slopes = slopes, 
                        time.scale = time.scale)
    tickers <- colnames(gains)
    
  } else if (!is.null(prices)) {
    
    # Calculate gains based on price data
    gains <- apply(prices, 2, pchanges)
    
  } else if (is.null(gains)) {
    
    stop("You must specify one of the following inputs: tickers, gains, or prices")
    
  }
  
  # Set tickers to column names of gains matrix; if NULL, use Fund 1, Fund 2, ...
  tickers <- colnames(gains)
  if (is.null(tickers)) {
    tickers <- paste("Fund", 1: ncol(gains))
  }
  
  # Figure out value for units.rate to get annualized growth from gains.rate function
  units.year <- ifelse(time.scale == "daily", 252, ifelse(time.scale == "monthly", 12, 1))
  
  # Calculate performance metrics for each fund
  perf.metrics <- data.frame(ticker = tickers,
                             growth = apply(gains, 2, function(x) gains.rate(gains = x)),
                             cagr = apply(gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)),
                             mdd = apply(gains, 2, function(x) mdd(gains = x)),
                             sharpe = apply(gains, 2, function(x) sharpe.ratio(gains = x)),
                             sortino = apply(gains, 2, function(x) sortino.ratio(gains = x)))
  
  # If benchmark.fund not NULL, add alpha and beta
  if (!is.null(benchmark.fund)) {
    perf.metrics$alpha <- apply(gains, 2, function(x) lm(x ~ gains[, benchmark.fund])$coef[1])
    perf.metrics$beta <- apply(gains, 2, function(x) lm(x ~ gains[, benchmark.fund])$coef[2])
  }
  
  # Calculate mean of gains for each fund and correlation matrix
  tickers.mean <- apply(gains, 2, mean)
  tickers.corr <- cor(gains)
  
  # Return performance metrics, means, and correlation matrix
  return.list <- list(perf.metrics = perf.metrics, tickers.mean = tickers.mean, tickers.corr = tickers.corr)
  return(return.list)
  
}


twofunds.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                           benchmark.ticker = "VFINX", reference.tickers = benchmark.ticker,
                           tickers.gains = NULL, benchmark.gains = NULL, reference.gains = NULL,
                           from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                           step.data = 0.0025, step.points = 0.1, 
                           y.axis = "mean", x.axis = "sd",
                           add.plot = FALSE,
                           colors = NULL,
                           plot.list = NULL,
                           points.list = NULL,
                           text.list = NULL) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {
    
    # If vectors rather than matrices are provided for tickers, intercepts, or slopes, convert to 2-row matrices
    if (is.vector(tickers)) {
      tickers <- matrix(tickers, nrow = 2)
    }
    if (is.vector(intercepts)) {
      intercepts <- matrix(intercepts, nrow = 2)
    }
    if (is.vector(slopes)) {
      slopes <- matrix(slopes, nrow = 2)
    }
    
    # If intercepts or slopes NULL, set to matrix of 0's and 1's, respectively
    if (is.null(intercepts)) {
      intercepts <- matrix(0, nrow = 2, ncol = ncol(tickers))
    }
    if (is.null(slopes)) {
      slopes <- matrix(1, nrow = 2, ncol = ncol(tickers))
    }
    
    # Create vector of "extra" tickers comprised of benchmark and reference tickers
    extra.tickers <- unique(c(benchmark.ticker, reference.tickers))
    
    # Calculate gains matrix
    tickers.vec <- c(as.vector(tickers), extra.tickers)
    intercepts.vec <- c(as.vector(intercepts), rep(0, length(extra.tickers)))
    slopes.vec <- c(as.vector(slopes), rep(1, length(extra.tickers)))
    gains <- load.gains(tickers = tickers.vec,
                        from = from, to = to, 
                        intercepts = intercepts.vec, slopes = slopes.vec,
                        time.scale = time.scale)
    
    # Update ticker names to show intercept/slope
    tickers <- matrix(colnames(gains)[1: length(tickers)], nrow = 2)
    
    # Separate benchmark gains, reference gains, and ticker gains
    tickers.gains <- gains[, 1: length(tickers)]
    if (!is.null(benchmark.ticker)) {
      benchmark.gains <- gains[, which(colnames(gains) == benchmark.ticker)[1]]
    }
    if (!is.null(reference.tickers)) {
      reference.gains <- gains[, (length(tickers) + ifelse(!is.null(benchmark.ticker), 1, 0)): ncol(gains), drop = F]
    }
    
  } else {
      
      # Figure out tickers from tickers.gains
      tickers <- matrix(colnames(tickers.gains), nrow = 2)
      
      # Convert reference.gains to matrix if necessary, and figure out reference.tickers
      if (is.vector(reference.gains)) {
        reference.gains <- matrix(reference.gains, ncol = 1)
        reference.tickers <- "REF"
      } else {
        reference.tickers <- colnames(reference.gains)
        if (is.null(reference.tickers)) {
          reference.tickers <- paste("REF", 1: ncol(reference.gains), sep = "")
        }
      }
    
      # If benchmark.gains is NULL, set reference.tickers to NULL
      if (is.null(benchmark.gains)) {
        benchmark.tickers <- NULL
      }
      
  }
  
  # Initialize list to store x-axis values and y-axis values for each pair
  x <- y <- portfolio.xy <- list()
  
  # Loop through fund pairs
  fund1.all <- seq(0, 1, step.data)
  fund2.all <- 1 - fund1.all
  fund.all <- cbind(fund1.all, fund2.all)
  num.points <- length(fund1.all)
  units.year <- ifelse(time.scale == "daily", 252, ifelse(time.scale == "monthly", 12, 1))
  
  for (ii in 1: ncol(tickers)) {
    
    # Get subset of tickers.gains matrix for tickers of interest
    columns <- c(ii * 2 - 1, ii * 2)
    tickers.gains.sub <- tickers.gains[, columns]
    
    # Calculate x-axis value for each allocation
    if (x.axis == "mean") {
      
      means <- apply(tickers.gains.sub, 2, mean)
      x[[ii]] <- (fund1.all * means[1] + fund2.all * means[2]) * 100
      
    } else if (x.axis == "sd") {
      
      vars <- var(tickers.gains.sub)
      x[[ii]] <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + fund1.all * fund2.all * vars[1, 2]) * 100
      
    } else if (x.axis == "cagr") {
      
      x[[ii]] <- apply(fund.all, 1, function(x) gains.rate(gains = tickers.gains.sub %*% x, units.rate = units.year)) * 100
      
    } else if (x.axis == "mdd") {
      
      x[[ii]] <- apply(fund.all, 1, function(x) mdd(gains = tickers.gains.sub %*% x)) * 100
      
    } else if (x.axis == "sharpe") {
      
      means <- apply(tickers.gains.sub, 2, mean)
      x1 <- fund.all %*% means
      vars <- var(tickers.gains.sub)
      x2 <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2])
      x[[ii]] <- x1 / x2
      
    } else if (x.axis == "sortino") {
      
      x[[ii]] <- apply(fund.all, 1, function(x) sortino.ratio(gains = tickers.gains.sub %*% x))
      
    } else if (x.axis == "alpha") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains)
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains)
      x[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100
      
    } else if (x.axis == "beta") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains)
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains)
      x[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]
      
    } else if (x.axis == "allocation") {
      
      x[[ii]] <- fund1.all * 100
      
    }
    
    # Calculate y-axis value for each allocation
    if (y.axis == "mean") {
      
      means <- apply(tickers.gains.sub, 2, mean)
      y[[ii]] <- (fund1.all * means[1] + fund2.all * means[2]) * 100
      
    } else if (y.axis == "sd") {
      
      vars <- var(tickers.gains.sub)
      y[[ii]] <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * 100
      
    } else if (y.axis == "cagr") {
      
      y[[ii]] <- apply(fund.all, 1, function(x) gains.rate(gains = tickers.gains.sub %*% x, units.rate = units.year)) * 100
      
    } else if (y.axis == "mdd") {
      
      y[[ii]] <- apply(fund.all, 1, function(x) mdd(gains = tickers.gains.sub %*% x)) * 100
      
    } else if (y.axis == "sharpe") {
      
      means <- apply(tickers.gains.sub, 2, mean)
      y1 <- fund1.all * means[1] + fund2.all * means[2]
      vars <- var(tickers.gains.sub)
      y2 <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2])
      y[[ii]] <- y1 / y2
      
    } else if (y.axis == "sortino") {
      
      y[[ii]] <- apply(fund.all, 1, function(x) sortino.ratio(gains = tickers.gains.sub %*% x))
      
    } else if (y.axis == "alpha") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains)
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains)
      y[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100
      
    } else if (y.axis == "beta") {
      
      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains)
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains)
      y[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]
      
    } else if (y.axis == "allocation") {
      
      y[[ii]] <- fund1.all * 100
      
    }
    
    # Combine x and y values into two-column matrix
    portfolio.xy[[ii]] <- cbind(x[[ii]], y[[ii]])
    
  }
  
  # Create variables for plot
  reference.y <- NULL
  if (y.axis == "mean") {
    y.title <- "Mean"
    y.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, mean) * 100
    }
  } else if (y.axis == "sd") {
    y.title <- "SD"
    y.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    y1 <- 0
    y2 <- max(unlist(y)) * 1.05
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, sd) * 100
    }
  } else if (y.axis == "cagr") {
    y.title <- "CAGR"
    y.label <- "CAGR (%)"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (y.axis == "mdd") {
    y.title <- "MDD"
    y.label <- "MDD (%)"
    y1 <- 0
    y2 <- max(unlist(y)) * 1.05
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
  } else if (y.axis == "sharpe") {
    y.title <- "Sharpe ratio"
    y.label <- "Sharpe ratio"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (y.axis == "sortino") {
    y.title <- "Sortino ratio"
    y.label <- "Sortino ratio"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sortino.ratio(gains = x))
    }
  } else if (y.axis == "alpha") {
    y.title <- "Alpha"
    y.label <- "Alpha (%)"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1]) * 100
    }
  } else if (y.axis == "beta") {
    y.title <- "Beta"
    y.label <- "Beta (%)"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    }
  }
  
  reference.x <- NULL
  if (x.axis == "mean") {
    x.title <- "Mean"
    x.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, mean) * 100
    }
  } else if (x.axis == "sd") {
    x.title <- "SD"
    x.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    x1 <- 0
    x2 <- max(unlist(x)) * 1.2
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, sd) * 100
    }
  } else if (x.axis == "cagr") {
    x.title <- "CAGR"
    x.label <- "CAGR (%)"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (x.axis == "mdd") {
    x.title <- "MDD"
    x.label <- "MDD (%)"
    x1 <- 0
    x2 <- max(unlist(x)) * 1.2
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
  } else if (x.axis == "sharpe") {
    x.title <- "Sharpe ratio"
    x.label <- "Sharpe ratio"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (x.axis == "sortino") {
    x.title <- "Sortino ratio"
    x.label <- "Sortino ratio"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (x.axis == "alpha") {
    x.title <- "Alpha"
    x.label <- "Alpha (%)"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1]) * 100
    }
  } else if (x.axis == "beta") {
    x.title <- "Beta"
    x.label <- "Beta (%)"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    }
  } else if (x.axis == "allocation") {
    x.title <- "Allocation"
    x.label <- "Allocation (%)"
    x1 <- -5
    x2 <- 105
  }
  
  # Figure out features of graph, based on user inputs where available
  if (is.null(plot.list)) {
    
    plot.list <- list(main = paste(y.title, " vs. ", x.title, sep = ""), cex.main = 1.25, 
                      xlab = x.label, ylab = y.label, 
                      xlim = c(x1, x2), ylim = c(y1, y2))
    
  } else {
    
    if (is.null(plot.list$main)) {
      plot.list$main <- paste(y.title, " vs. ", x.title, sep = "")
    }
    if (is.null(plot.list$cex.main)) {
      plot.list$cex.main <- 1.25
    }
    if (is.null(plot.list$xlab)) {
      plot.list$xlab <- x.label
    }
    if (is.null(plot.list$ylab)) {
      plot.list$ylab <- y.label
    }
    if (is.null(plot.list$xlim)) {
      plot.list$xlim <- c(x1, x2)
    }
    if (is.null(plot.list$ylim)) {
      plot.list$ylim <- c(y1, y2)
    }
    
  }
    
  if (is.null(points.list)) {
    
    points.list <- list(pch = 16, cex = 0.7)
    
  }  else {
    
    if (is.null(points.list$pch)) {
      points.list$pch <- 16
    }
    if (is.null(points.list$cex)) {
      points.list$cex <- 0.7
    }
    
  }
  
  if (is.null(text.list)) {
    
    text.list <- list(cex = 0.7)
    
  } else {
    
    if (is.null(text.list$cex)) {
      text.list$cex <- 0.7
    }
  }
  
  # Create color scheme for plot
  n.pairs <- ncol(tickers)
  if (is.null(colors)) {
    if (n.pairs == 1) {
      colors <- "black"
    } else if (n.pairs == 2) {
      colors <- c("blue", "red")
    } else if (n.pairs == 3) {
      colors <- c("blue", "red", "orange")
    } else if (n.pairs == 4) {
      colors <- c("blue", "red", "orange", "purple")
    } else if (n.pairs > 4) {
      colors <- colorRampPalette(c("blue", "red"))(n.pairs)
    }
  }
  
  # Create plot region
  if (! add.plot) {
    do.call(plot, c(list(x = 0, y = 0, type = "n"), plot.list))
  }
  
  # Figure out indices for data points
  if (!is.null(step.points)) {
    locs.points <- seq(1, num.points, step.points / step.data)
  } else {
    locs.points <- c(1, num.points)
  }
  y.offset <- (y2 - y1) / 40
  
  # Add curves for each pair
  for (ii in 1: n.pairs) {
    
    # Add colored curves and data points
    do.call(points, c(list(x = x[[ii]], y = y[[ii]], type = "l", col = colors[ii]), points.list))
    do.call(points, c(list(x = x[[ii]][locs.points], y = y[[ii]][locs.points], col = colors[ii]), points.list))
    
    # Figure out (x, y) coordinates for 100% fund 1 and 100% fund 2
    fund1.xy <- c(x[[ii]][num.points], y[[ii]][num.points])
    fund2.xy <- c(x[[ii]][1], y[[ii]][1])
    
    # Figure out y offsets for labels for 100% fund 1, 100% fund 2, and 100% fund 3
    max.y <- which.max(c(fund1.xy[2], fund2.xy[2]))
    if (max.y == 1) {
      y.offset.fund1 <- y.offset
      y.offset.fund2 <- -y.offset
    } else {
      y.offset.fund1 <- -y.offset
      y.offset.fund2 <- y.offset
    }
    
    # Add black data points at 100% fund 1 and 100% fund2
    do.call(points, c(list(x = fund1.xy[1], y = fund1.xy[2]), points.list))
    do.call(points, c(list(x = fund2.xy[1], y = fund2.xy[2]), points.list))
    
    # Add text labels if not already on plot
    if (ii == 1 | ! tickers[1, ii] %in% tickers[, -ii]) {
      do.call(text, c(list(x = fund1.xy[1], y = fund1.xy[2] + y.offset.fund1,
                           label = paste("100% ", tickers[1, ii], sep = "")), text.list))
    } 
    if (ii == 1 | ! tickers[2, ii] %in% tickers[, -ii]) {
      do.call(text, c(list(x = fund2.xy[1], y = fund2.xy[2] + y.offset.fund2,
                           label = paste("100% ", tickers[2, ii], sep = "")), text.list))
    } 
    
  }
  
  # Add data point for reference funds (if given)
  if (!is.null(reference.tickers)) {
    
    # Loop through and add data points for each reference fund
    for (ii in 1: ncol(reference.gains)) {
      
      if (x.axis != "allocation") {
        
        do.call(points, c(list(x = reference.x[ii], y = reference.y[ii], type = "p", col = "black"), points.list))
        if (! reference.tickers[ii] %in% tickers) {
          text(reference.x[ii], reference.y[ii] + y.offset, label = reference.tickers[ii], cex = 0.7)
        }
      } else {
        abline(h = reference.y[ii], lty = 2, col = "black")
        text(20, reference.y[ii] + y.offset, label = reference.tickers[ii], cex = 0.85)
      }
    }
    
  }
  
  # tickers.perf <- data.frame(tickers = tickers.vec, 
  #                            growth = apply(gains, 2, gains.rate),
  #                            cagr = apply(gains, 2, function(x) gains.rate(x, units.rate = units.year)),
  #                            mdd = apply(gains, 2, mdd), 
  #                            sharpe = apply(gains, 2, sharpe.ratio),
  #                            sortino = apply(gains, 2, sortino.ratio),
  #                            alpha = apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1]),
  #                            beta = apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2]))
  
  # Return portfolio.xy, mean for each fund and correlation matrix
  means <- apply(gains, 2, mean)
  corr.matrix <- cor(gains)
  return.list <- list(portfolio.xy = portfolio.xy, means = means, corr.matrix = corr.matrix)
  return(return.list)
  
}


threefunds.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                             benchmark.ticker = "VFINX", reference.tickers = benchmark.ticker,
                             tickers.gains = NULL, benchmark.gains = NULL, reference.gains = NULL,
                             from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                             step.data = 0.0025, step.points = 0.1, step.curves = 0.2,
                             y.axis = "mean", x.axis = "sd", 
                             add.plot = FALSE, 
                             colors = NULL,
                             plot.list = NULL,
                             points.list = NULL,
                             text.list = NULL) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {
    
    # If vectors rather than matrices are provided for tickers, intercepts, or slopes, convert to 2-row matrices
    if (is.vector(tickers)) {
      tickers <- matrix(tickers, nrow = 3)
    }
    if (is.vector(intercepts)) {
      intercepts <- matrix(intercepts, nrow = 3)
    }
    if (is.vector(slopes)) {
      slopes <- matrix(slopes, nrow = 3)
    }
    
    # If intercepts or slopes NULL, set to matrix of 0's and 1's, respectively
    if (is.null(intercepts)) {
      intercepts <- matrix(0, nrow = 3, ncol = ncol(tickers))
    }
    if (is.null(slopes)) {
      slopes <- matrix(1, nrow = 3, ncol = ncol(tickers))
    }
    
    # Create vector of "extra" tickers comprised of benchmark and reference tickers
    extra.tickers <- unique(c(benchmark.ticker, reference.tickers))
    
    # Calculate gains matrix
    tickers.vec <- c(as.vector(tickers), extra.tickers)
    intercepts.vec <- c(as.vector(intercepts), rep(0, length(extra.tickers)))
    slopes.vec <- c(as.vector(slopes), rep(1, length(extra.tickers)))
    gains <- load.gains(tickers = tickers.vec,
                        from = from, to = to, 
                        intercepts = intercepts.vec, slopes = slopes.vec,
                        time.scale = time.scale)
    
    # Update ticker names to show intercept/slope
    tickers <- matrix(colnames(gains)[1: length(tickers)], nrow = 3)
    
    # Get dates
    dates <- as.Date(rownames(gains))
    
    # Separate benchmark gains, reference gains, and ticker gains
    tickers.gains <- gains[, 1: length(tickers)]
    if (!is.null(benchmark.ticker)) {
      benchmark.gains <- gains[, which(colnames(gains) == benchmark.ticker)[1]]
    }
    if (!is.null(reference.tickers)) {
      reference.gains <- gains[, (length(tickers) + ifelse(!is.null(benchmark.ticker), 1, 0)): ncol(gains), drop = F]
    }
    
  } else {
    
    # Figure out tickers from tickers.gains
    tickers <- matrix(colnames(tickers.gains), nrow = 3)
    
    # Convert reference.gains to matrix if necessary, and figure out reference.tickers
    if (is.vector(reference.gains)) {
      reference.gains <- matrix(reference.gains, ncol = 1)
      reference.tickers <- "REF"
    } else {
      reference.tickers <- colnames(reference.gains)
      if (is.null(reference.tickers)) {
        reference.tickers <- paste("REF", 1: ncol(reference.gains), sep = "")
      }
    }
    
    # If benchmark.gains is NULL, set reference.tickers to NULL
    if (is.null(benchmark.gains)) {
      benchmark.tickers <- NULL
    }
    
  }
  
  # Initialize portfolio.xy to store data for each three-fund set
  portfolio.xy <- list()
  
  # Loop through three-fund sets
  fund1.all <- seq(0, 1, step.curves)
  fund2.all <- seq(0, 1, step.data)
  fund3.all <- 1 - fund2.all
  num.curves <- length(fund1.all)
  num.points <- length(fund2.all)
  units.year <- ifelse(time.scale == "daily", 252, ifelse(time.scale == "monthly", 12, 1))
  
  # Going in different direction. If doesn't work, revert back to version in exports file
  for (ii in 1: ncol(tickers)) {
    
    # Initialize list to store x-axis values and y-axis values for current three-fund set
    x <- y <- list()
    
    # Get subset of tickers.gains matrix for tickers of interest
    columns <- (ii * 3 - 2): (ii * 3)
    tickers.gains.sub <- tickers.gains[, columns]
    
    # Calculate x-axis value for each allocation
    if (x.axis == "mean") {
      
      means <- apply(tickers.gains.sub, 2, mean) * 100
      x <- lapply(fund1.all, function(x) x * means[1] + (1 - x) * fund2.all * means[2] + (1 - x) * fund3.all * means[3])
      
    } else if (x.axis == "sd") {
      
      vars <- var(tickers.gains.sub * 100)
      x <- lapply(fund1.all, function(x) {
        sqrt(x^2 * vars[1, 1] + ((1 - x) * fund2.all)^2 * vars[2, 2] + ((1 - x) * fund3.all)^2 * vars[3, 3] +
               2 * x * (1 - x) * fund2.all * vars[1, 2] + 2 * x * (1 - x) * fund3.all * vars[1, 3] +
               2 * (1 - x) * fund2.all * (1 - x) * fund3.all * vars[2, 3])
      })
      
    } else if (x.axis == "cagr") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) gains.rate(gains = x, units.rate = 252) * 100)
      })
      
    } else if (x.axis == "mdd") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) mdd(gains = x) * 100)
      })
      
    } else if (x.axis == "sharpe") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) sharpe.ratio(gains = x))
      })
      
    } else if (x.axis == "sortino") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) sortino.ratio(gains = x))
      })
      
    } else if (x.axis == "alpha") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
      })
      
    } else if (x.axis == "beta") {
      
      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) lm(x ~ benchmark.gains)$coef[2] * 100)
      })
      
    } else if (x.axis == "allocation") {
      
      x <- lapply(fund1.all, function(x) fund2.all * 100)
      
    }
    
    # Calculate y-axis value for each allocation
    if (y.axis == "mean") {
      
      means <- apply(tickers.gains.sub, 2, mean) * 100
      y <- lapply(fund1.all, function(x) x * means[1] + (1 - x) * fund2.all * means[2] + (1 - x) * fund3.all * means[3])
      
    } else if (y.axis == "sd") {
      
      vars <- var(tickers.gains.sub * 100)
      y <- lapply(fund1.all, function(x) {
        sqrt(x^2 * vars[1, 1] + ((1 - x) * fund2.all)^2 * vars[2, 2] + ((1 - x) * fund3.all)^2 * vars[3, 3] +
               2 * x * (1 - x) * fund2.all * vars[1, 2] + 2 * x * (1 - x) * fund3.all * vars[1, 3] +
               2 * (1 - x) * fund2.all * (1 - x) * fund3.all * vars[2, 3])
      })
      
    } else if (y.axis == "cagr") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) gains.rate(gains = x, units.rate = 252) * 100)
      })
      
    } else if (y.axis == "mdd") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) mdd(gains = x) * 100)
      })
      
    } else if (y.axis == "sharpe") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) sharpe.ratio(gains = x))
      })
      
    } else if (y.axis == "sortino") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) sortino.ratio(gains = x))
      })
      
    } else if (y.axis == "alpha") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
      })
      
    } else if (y.axis == "beta") {
      
      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all, 
                                             (1 - x) * fund3.all), 
                                           nrow = 3, byrow = TRUE), 
              2, function(x) lm(x ~ benchmark.gains)$coef[2] * 100)
      })
      
    }
    
    # Create list where each element is a two-column matrix of x and y values for a particular fund-1 allocation
    set.list <- mapply(function(x, y) list(cbind(unlist(x), unlist(y))), x, y, SIMPLIFY = TRUE)
    names(set.list) <- paste(fund1.all * 100, "% ", tickers[1, ii], sep = "")
    
    # Add set.list to portfolio.xy
    portfolio.xy[[ii]] <- set.list
    
  }
  
  # Create labels for 3-fund sets
  set.labels <- apply(tickers, 2, function(x) paste(x[1], "-", x[2], "-", x[3], sep = ""))
  
  # Extract all x and y values from portfolio.xy
  x <- lapply(portfolio.xy, function(x) lapply(x, function(x) x[, 1]))
  y <- lapply(portfolio.xy, function(x) lapply(x, function(x) x[, 2]))
  
  # Create variables for plot
  reference.y <- NULL
  if (y.axis == "mean") {
    y.title <- "Mean"
    y.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, mean) * 100
    }
  } else if (y.axis == "sd") {
    y.title <- "SD"
    y.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    y1 <- 0
    y2 <- max(unlist(y)) * 1.05
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, sd) * 100
    }
  } else if (y.axis == "cagr") {
    y.title <- "CAGR"
    y.label <- "CAGR (%)"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (y.axis == "mdd") {
    y.title <- "MDD"
    y.label <- "MDD (%)"
    y1 <- 0
    y2 <- max(unlist(y)) * 1.05
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
  } else if (y.axis == "sharpe") {
    y.title <- "Sharpe ratio"
    y.label <- "Sharpe ratio"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (y.axis == "sortino") {
    y.title <- "Sortino ratio"
    y.label <- "Sortino ratio"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sortino.ratio(gains = x))
    }
  } else if (y.axis == "alpha") {
    y.title <- "Alpha"
    y.label <- "Alpha (%)"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1]) * 100
    }
  } else if (y.axis == "beta") {
    y.title <- "Beta"
    y.label <- "Beta (%)"
    y1 <- min(min(unlist(y)) * 1.05, 0)
    y2 <- max(0, max(unlist(y)) * 1.05)
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    }
  }
  
  reference.x <- NULL
  if (x.axis == "mean") {
    x.title <- "Mean"
    x.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, mean) * 100
    }
  } else if (x.axis == "sd") {
    x.title <- "SD"
    x.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    x1 <- 0
    x2 <- max(unlist(x)) * 1.2
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, sd) * 100
    }
  } else if (x.axis == "cagr") {
    x.title <- "CAGR"
    x.label <- "CAGR (%)"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (x.axis == "mdd") {
    x.title <- "MDD"
    x.label <- "MDD (%)"
    x1 <- 0
    x2 <- max(unlist(x)) * 1.2
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
  } else if (x.axis == "sharpe") {
    x.title <- "Sharpe ratio"
    x.label <- "Sharpe ratio"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (x.axis == "sortino") {
    x.title <- "Sortino ratio"
    x.label <- "Sortino ratio"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (x.axis == "alpha") {
    x.title <- "Alpha"
    x.label <- "Alpha (%)"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1]) * 100
    }
  } else if (x.axis == "beta") {
    x.title <- "Beta"
    x.label <- "Beta (%)"
    x1 <- min(min(unlist(x)) * 1.2, 0)
    x2 <- max(0, max(unlist(x)) * 1.2)
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    }
  } else if (x.axis == "allocation") {
    x.title <- "Allocation"
    x.label <- "Allocation (%)"
    x1 <- -5
    x2 <- 105
  }
  
  # Figure out features of graph, based on user inputs where available
  if (is.null(plot.list)) {
    
    plot.list <- list(main = paste(y.title, " vs. ", x.title, sep = ""), cex.main = 1.25, 
                      xlab = x.label, ylab = y.label, 
                      xlim = c(x1, x2), ylim = c(y1, y2))
    
  } else {
    
    if (is.null(plot.list$main)) {
      plot.list$main <- paste(y.title, " vs. ", x.title, sep = "")
    }
    if (is.null(plot.list$cex.main)) {
      plot.list$cex.main <- 1.25
    }
    if (is.null(plot.list$xlab)) {
      plot.list$xlab <- x.label
    }
    if (is.null(plot.list$ylab)) {
      plot.list$ylab <- y.label
    }
    if (is.null(plot.list$xlim)) {
      plot.list$xlim <- c(x1, x2)
    }
    if (is.null(plot.list$ylim)) {
      plot.list$ylim <- c(y1, y2)
    }
    
  }
  
  if (is.null(points.list)) {
    
    points.list <- list(pch = 16, cex = 0.7)
    
  }  else {
    
    if (is.null(points.list$pch)) {
      points.list$pch <- 16
    }
    if (is.null(points.list$cex)) {
      points.list$cex <- 0.7
    }
    
  }
  
  if (is.null(text.list)) {
    
    text.list <- list(cex = 0.7)
    
  } else {
    
    if (is.null(text.list$cex)) {
      text.list$cex <- 0.7
    }
  }
  
  # Create color scheme for plot
  n.sets <- ncol(tickers)
  if (is.null(colors)) {
    if (n.sets == 1) {
      colors <- "black"
    } else if (n.sets == 2) {
      colors <- c("blue", "red")
    } else if (n.sets == 3) {
      colors <- c("blue", "red", "orange")
    } else if (n.sets == 4) {
      colors <- c("blue", "red", "orange", "purple")
    } else if (n.sets > 4) {
      colors <- colorRampPalette(c("blue", "red"))(n.sets)
    }
  }
  
  # Create plot region
  if (! add.plot) {
    do.call(plot, c(list(x = 0, y = 0, type = "n"), plot.list))
  }
  
  # Figure out indices for data points
  if (!is.null(step.points)) {
    locs.points <- seq(1, num.points, step.points / step.data)
  } else {
    locs.points <- c(1, num.points)
  }
  y.offset <- (y2 - y1) / 40
  
  # Add curves for each set
  for (ii in 1: n.sets) {
    
    # Add colored curves and data points
    lapply(portfolio.xy[[ii]], function(x) {
      do.call(points, c(list(x = x[, 1], y = x[, 2], type = "l", col = colors[ii]), points.list))
      do.call(points, c(list(x = x[locs.points, 1], y = x[locs.points, 2], col = colors[ii]), points.list))
    })
    
    # Figure out (x, y) coordinates for 100% fund 1, 100% fund 2, and 100% fund 3
    fund1.xy <- portfolio.xy[[ii]][[num.curves]][1, 1: 2]
    fund2.xy <- portfolio.xy[[ii]][[1]][num.points, 1: 2]
    fund3.xy <- portfolio.xy[[ii]][[1]][1, 1: 2]
    
    # Figure out y offsets for labels for 100% fund 1, 100% fund 2, and 100% fund 3
    max.y <- which.max(c(fund1.xy[2], fund2.xy[2], fund3.xy[2]))
    if (max.y == 1) {
      y.offset.fund1 <- y.offset
      y.offset.fund2 <- -y.offset
      y.offset.fund3 <- -y.offset
    } else if (max.y == 2) {
      y.offset.fund1 <- -y.offset
      y.offset.fund2 <- y.offset
      y.offset.fund3 <- -y.offset
    } else {
      y.offset.fund1 <- -y.offset
      y.offset.fund2 <- -y.offset
      y.offset.fund3 <- y.offset
    }
    
    # Add black data points at 100% fund 1, 100% fund2, and 100% fund 3
    do.call(points, c(list(x = fund1.xy[1], y = fund1.xy[2]), points.list))
    do.call(points, c(list(x = fund2.xy[1], y = fund2.xy[2]), points.list))
    do.call(points, c(list(x = fund3.xy[1], y = fund3.xy[2]), points.list))
    
    # Add text labels if not already on plot
    if (ii == 1 | ! tickers[1, ii] %in% tickers[, -ii]) {
      do.call(text, c(list(x = fund1.xy[1], y = fund1.xy[2] + y.offset.fund1,
                           label = paste("100% ", tickers[1, ii], sep = "")), text.list))
    } 
    if (ii == 1 | ! tickers[2, ii] %in% tickers[, -ii]) {
      do.call(text, c(list(x = fund2.xy[1], y = fund2.xy[2] + y.offset.fund2,
                           label = paste("100% ", tickers[2, ii], sep = "")), text.list))
    } 
    if (ii == 1 | ! tickers[3, ii] %in% tickers[, -ii]) {
      do.call(text, c(list(x = fund3.xy[1], y = fund3.xy[2] + y.offset.fund3,
                           label = paste("100% ", tickers[3, ii], sep = "")), text.list))
    } 
  }
  
  # Add data point for reference funds (if given)
  if (!is.null(reference.tickers)) {
    
    # Loop through and add data points for each reference fund
    for (ii in 1: ncol(reference.gains)) {
      
      if (x.axis != "allocation") {
        
        do.call(points, c(list(x = reference.x[ii], y = reference.y[ii], type = "p", col = "black"), points.list))
        if (! reference.tickers[ii] %in% tickers) {
          do.call(text, c(list(x = reference.x[ii], y = reference.y[ii] + y.offset, label = reference.tickers[ii]), 
                          text.list))
        }
      } else {
        abline(h = reference.y[ii], lty = 2, col = "black")
        do.call(text, c(list(x = 20, y = reference.y[ii] + y.offset, label = reference.tickers[ii]), 
                        text.list))
      }
    }
    
  }
  
  # Return portfolio.xy, mean for each fund, and correlation matrix
  means <- apply(gains, 2, mean)
  corr.matrix <- cor(gains)
  return.list <- list(portfolio.xy = portfolio.xy, means = means, corr.matrix = corr.matrix)
  return(return.list)
  
}


growth.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                         prices = NULL, 
                         from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                         initial = 10000,
                         add.plot = FALSE, 
                         colors = NULL, 
                         plot.list = NULL,
                         points.list = NULL) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {
    
    # # If intercepts or slopes NULL, set to matrix of 0's and 1's, respectively
    # if (is.null(intercepts)) {
    #   intercepts <- rep(0, length(tickers))
    # }
    # if (is.null(slopes)) {
    #   slopes <- rep(1, length(tickers))
    # }
    
    # Obtain matrix of prices for each fund
    prices <- load.prices(tickers = tickers, intercepts = intercepts, slopes = slopes, 
                          from = from, to = to,
                          time.scale = time.scale, initial = initial)
    
  }
  
  # Update tickers vector
  tickers <- colnames(prices)
  if (is.null(tickers)) {
    tickers <- paste("Fund", 1: ncol(prices))
  }
  
  # Get dates
  rows <- rownames(prices)
  if (!is.null(rows)) {
    dates <- as.Date(rows)
  } else {
    dates <- 1: nrow(prices)
  }
  
  # Create color scheme for plot
  n.tickers <- length(tickers)
  if (is.null(colors)) {
    if (n.tickers == 1) {
      colors <- "black"
    } else if (n.tickers == 2) {
      colors <- c("blue", "red")
    } else if (n.tickers == 3) {
      colors <- c("blue", "red", "orange")
    } else if (n.tickers == 4) {
      colors <- c("blue", "red", "orange", "purple")
    } else if (n.tickers > 4) {
      colors <- colorRampPalette(c("blue", "red"))(n.tickers)
    }
  }
  
  # Figure out features of graph, based on user inputs where available
  if (is.null(plot.list)) {
    
    plot.list <- list(main = paste("Growth of $", initial, sep = ""), cex.main = 1.25, 
                      xlab = "Date", ylab = "Balance ($)",
                      xlim = range(dates), ylim = c(0, max(prices)))
    
  } else {
    
    if (is.null(plot.list$main)) {
      plot.list$main <- paste("Growth of $", initial, sep = "")
    }
    if (is.null(plot.list$cex.main)) {
      plot.list$cex.main <- 1.25
    }
    if (is.null(plot.list$xlab)) {
      plot.list$xlab <- "Date"
    }
    if (is.null(plot.list$ylab)) {
      plot.list$ylab <- "Balance ($)"
    }
    if (is.null(plot.list$xlim)) {
      plot.list$xlim <- range(dates)
    }
    if (is.null(plot.list$ylim)) {
      plot.list$ylim <- c(0, max(prices))
    }
    
  }
  
  # Create plot region
  if (! add.plot) {
    do.call(plot, c(list(x = dates, y = prices[, 1], type = "n"), plot.list))
  }
  
  # Add lines for each fund
  for (ii in 1: ncol(prices)) {
    do.call(points, c(list(x = dates, y = prices[, ii], type = "l", col = colors[ii]), points.list))
  }
  
  # Add legend
  legend("topleft", lty = 1, col = colors, legend = tickers)
  
  # Return prices matrix, mean of gains for each fund, and correlation matrix
  gains <- apply(prices, 2, pchanges) * 100
  means <- apply(gains, 2, mean)
  corr.matrix <- cor(gains)
  return.list <- list(prices = prices, means = means, corr.matrix = corr.matrix)
  return(return.list)
  
}

# PLANS: 

# 1. Make function that plots a perf. metric over time, for single or two fund portfolios. Maybe just give it gains for one or more funds/portfolios.
# 2. Make GIF version that shows graph over time. List of lists, etc.
# 3. write article on pairing Vanguard bond funds with S&P (and maybe UPRO). mean vs. sd and cagr vs. mdd.