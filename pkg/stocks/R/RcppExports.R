list.override <- function(list1, list2) {
  
  # Get names of elements of list1 and list2
  names.list1 <- names(list1)
  names.list2 <- names(list2)
  
  # Loop through elements of list 2. If in list 1, remove, then add; if not in list 1, add.
  for (ii in 1: length(list2)) {
    
    element.name <- names.list2[ii]
    loc.list1 <- which(names.list1 == element.name)
    if (length(loc.list1) > 0) {
      list1[loc.list1] <- list2[ii]
    } else {
      list1 <- c(list1, list2[ii])
    }
    
  }
  
  # Return list1, which has its original elements plus any extras/overrides from list2
  return(list1)
  
}


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
                    days = unlist(lapply(prices, function(x) nrow(x))),
                    stringsAsFactors = FALSE)
  return(ret)
  
}


load.gains <- function(tickers, intercepts = NULL, slopes = NULL, 
                       from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                       earliest.subset = TRUE) {
  
  # # Remove CASH if included in tickers
  # cash.included <- "CASH" %in% tickers
  # if (cash.included) {
  #   loc.cash <- which(tickers == "CASH")
  #   tickers <- tickers[-loc.cash]
  # }
  
  # Download stock prices for tickers from Yahoo! Finance, using the quantmod package
  prices <- list()
  for (ii in 1: length(tickers)) {
    prices.fund <- try(as.matrix(getSymbols(Symbols = tickers[ii], from = from, to = to, auto.assign = FALSE, warnings = FALSE)), silent = TRUE)
    if (class(prices.fund) == "try-error") {
      prices[[ii]] <- NULL
    } else {
      prices[[ii]] <- prices.fund
      if (! earliest.subset) {
        from <- max(as.Date(from), as.Date(rownames(prices.fund[1, , drop = F])))
      }
    }
  }
  
  # Drop tickers that could not be loaded
  locs <- sapply(prices, function(x) !is.null(x))
  if (!all(locs)) {
    tickers <- tickers[locs]
    prices <- prices[locs]
    intercepts <- intercepts[locs]
    slopes <- slopes[locs]
  }
  
  # If more than 1 fund, align prices
  if (length(tickers) > 1) {
    
    # Align start dates
    start.dates <- as.Date(unlist(lapply(prices, function(x) rownames(x[1, , drop = F]))))
    if (earliest.subset) {
      earliest.startdate <- min(start.dates)
      locs.earliest <- which(start.dates == earliest.startdate)
      tickers <- tickers[locs.earliest]
      start.dates <- start.dates[locs.earliest]
      prices <- prices[locs.earliest]
    } else {
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
        if (slopes[ii] != 1) {
          tickers[ii] <- paste(slopes[ii], "x ", tickers[ii], sep = "")
        }
        
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
  # if (cash.included) {
  #   if (loc.cash == 1) {
  #     gains.mat <- cbind(rep(0, nrow(gains.mat)), gains.mat)
  #     colnames(gains.mat) <- c("CASH", tickers)
  #   } else {
  #     gains.mat <- cbind(gains.mat[, 1: (loc.cash - 1)], rep(0, nrow(gains.mat)), 
  #                        gains.mat[, loc.cash: ncol(gains.mat)])
  #     colnames(gains.mat) <- c(tickers[1: (loc.cash - 1)], "CASH", tickers[loc.cash: length(tickers)])
  #   } 
  # } else {
  #   colnames(gains.mat) <- tickers
  # }
  colnames(gains.mat) <- tickers
  rownames(gains.mat) <- as.character(dates)[-1]
  return(gains.mat)
  
}


load.prices <- function(tickers, intercepts = NULL, slopes = NULL, 
                        from = "1900-01-01", to = Sys.Date(), time.scale = "daily", 
                        initial = NULL, 
                        earliest.subset = TRUE) {
  
  # # Remove CASH if included in tickers
  # cash.included <- "CASH" %in% tickers
  # if (cash.included) {
  #   loc.cash <- which(tickers == "CASH")
  #   tickers <- tickers[-loc.cash]
  # }
  
  # Download stock prices for tickers from Yahoo! Finance, using the quantmod package
  prices <- list()
  for (ii in 1:length(tickers)) {
    prices.fund <- try(as.matrix(getSymbols(Symbols = tickers[ii], from = from, to = to, auto.assign = FALSE, warnings = FALSE)), silent = TRUE)
    if (class(prices.fund) == "try-error") {
      prices[[ii]] <- NULL
    } else {
      prices[[ii]] <- prices.fund
      if (! earliest.subset) {
        from <- max(as.Date(from), as.Date(rownames(prices.fund[1, , drop = F])))
      }
    }
  }
  
  # Drop tickers that could not be loaded
  locs <- sapply(prices, function(x) !is.null(x))
  if (!all(locs)) {
    tickers <- tickers[locs]
    prices <- prices[locs]
    intercepts <- intercepts[locs]
    slopes <- slopes[locs]
  }
  
  # If more than 1 fund, align prices
  if (length(tickers) > 1) {
    
    # Align start dates
    start.dates <- as.Date(unlist(lapply(prices, function(x) rownames(x[1, , drop = F]))))
    if (earliest.subset) {
      earliest.startdate <- min(start.dates)
      locs.earliest <- which(start.dates == earliest.startdate)
      tickers <- tickers[locs.earliest]
      start.dates <- start.dates[locs.earliest]
      prices <- prices[locs.earliest]
    } else {
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
  }
  
  # Output message indicating date range
  message(paste("Results are for ", dates[1], " to " , dates[length(dates)], sep = ""))
  
  # Create matrix of closing prices
  closing.prices <- matrix(unlist(lapply(prices, function(x) x[, 6])), byrow = F, ncol = length(tickers))
  # if (cash.included) {
  #   if (loc.cash == 1) {
  #     closing.prices <- cbind(rep(1, nrow(closing.prices)), closing.prices)
  #     colnames(closing.prices) <- c("CASH", tickers)
  #   } else {
  #     closing.prices <- cbind(closing.prices[, 1: (loc.cash - 1)], rep(1, nrow(closing.prices)), 
  #                             closing.prices[, loc.cash: ncol(closing.prices)])
  #     colnames(closing.prices) <- c(tickers[1: (loc.cash - 1)], "CASH", tickers[loc.cash: length(tickers)])
  #   }
  # } else {
  #   colnames(closing.prices) <- tickers
  # }
  colnames(closing.prices) <- tickers
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
        if (slopes[ii] != 1) {
          tickers[ii] <- paste(slopes[ii], "x ", tickers[ii], sep = "")
        }
      }
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
                    from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                    earliest.subset = TRUE,
                    perf.metrics = c("mean", "sd", "growth", "cagr", "mdd", "sharpe",
                                     "sortino", "alpha", "beta", "r.squared", 
                                     "pearson", "spearman", "auto.pearson", "auto.spearman")) {
  
  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {
    
    # Obtain matrix of gains for each fund
    gains <- load.gains(tickers = tickers, from = from, to = to,
                        intercepts = intercepts, slopes = slopes, 
                        time.scale = time.scale,
                        earliest.subset = earliest.subset)
    
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
  p.metrics <- data.frame(ticker = tickers, stringsAsFactors = FALSE)
  if ("mean" %in% perf.metrics) {
    p.metrics$mean <- apply(gains, 2, mean)
  }
  if ("sd" %in% perf.metrics) {
    p.metrics$sd <- apply(gains, 2, sd)
  }
  if ("growth" %in% perf.metrics) {
    p.metrics$growth <- apply(gains, 2, function(x) gains.rate(gains = x))
  }
  if ("cagr" %in% perf.metrics) {
    p.metrics$cagr <- apply(gains, 2, function(x) gains.rate(gains = x, units.rate = units.year))
  }
  if ("mdd" %in% perf.metrics) {
    p.metrics$mdd <- apply(gains, 2, function(x) mdd(gains = x))
  }
  if ("sharpe" %in% perf.metrics) {
    p.metrics$sharpe <- apply(gains, 2, function(x) sharpe.ratio(gains = x))
  }
  if ("sortino" %in% perf.metrics) {
    p.metrics$sortino <- apply(gains, 2, function(x) sortino.ratio(gains = x))
  }
  if ("alpha" %in% perf.metrics) {
    p.metrics$alpha <- apply(gains, 2, function(x) lm(x ~ gains[, 1])$coef[1])
  }
  if ("beta" %in% perf.metrics) {
    p.metrics$beta <- apply(gains, 2, function(x) lm(x ~ gains[, 1])$coef[2])
  }
  if ("r.squared" %in% perf.metrics) {
    p.metrics$r.squared <- apply(gains, 2, function(x) summary(lm(x ~ gains[, 1]))$r.squared)
  }
  if ("pearson" %in% perf.metrics) {
    p.metrics$pearson <- apply(gains, 2, function(x) cor(x, gains[, 1]))
  }
  if ("spearman" %in% perf.metrics) {
    p.metrics$spearman <- apply(gains, 2, function(x) cor(x, gains[, 1], method = "spearman"))
  }
  if ("auto.pearson" %in% perf.metrics) {
    p.metrics$auto.pearson <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1]))
  }
  if ("auto.spearman" %in% perf.metrics) {
    p.metrics$auto.spearman <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
  }
  
  # Calculate correlation matrix
  cor.mat <- cor(gains)
  
  # Return performance metrics and and correlation matrix
  return.list <- list(perf.metrics = p.metrics, cor.mat = cor.mat)
  return(return.list)
  
}


twofunds.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                           benchmark.tickers = NULL, reference.tickers = NULL,
                           tickers.gains = NULL, benchmark.gains = NULL, reference.gains = NULL,
                           from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                           step.data = 0.0025, step.points = 0.1,
                           x.metric = "sd", y.metric = "mean",
                           tickerlabel.offsets = NULL,
                           reflabel.offsets = NULL,
                           add.plot = FALSE,
                           colors = NULL,
                           plot.list = NULL,
                           points.list = NULL,
                           text.list = NULL,
                           pdf.list = NULL,
                           bmp.list = NULL,
                           jpeg.list = NULL,
                           png.list = NULL,
                           tiff.list = NULL) {

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
    extra.tickers <- unique(c(benchmark.tickers, reference.tickers))

    # Calculate gains matrix
    tickers.vec <- c(as.vector(tickers), extra.tickers)
    intercepts.vec <- c(as.vector(intercepts), rep(0, length(extra.tickers)))
    slopes.vec <- c(as.vector(slopes), rep(1, length(extra.tickers)))
    gains <- load.gains(tickers = tickers.vec,
                        from = from, to = to,
                        intercepts = intercepts.vec, slopes = slopes.vec,
                        time.scale = time.scale,
                        earliest.subset = FALSE)

    # Update ticker names to show intercept/slope
    tickers <- matrix(colnames(gains)[1: length(tickers)], nrow = 2)

    # Separate benchmark gains, reference gains, and ticker gains
    tickers.gains <- gains[, 1: length(tickers), drop = F]
    extra.gains <- gains[, -c(1: length(tickers)), drop = F]
    if (!is.null(benchmark.tickers)) {
      benchmark.gains <- extra.gains[, 1: length(benchmark.tickers), drop = F]
      extra.gains <- extra.gains[, -c(1: length(benchmark.tickers)), drop = F]
    }
    if (!is.null(reference.tickers)) {
      reference.gains <- extra.gains
    }
    
  } else {
    
    # Figure out tickers from tickers.gains
    tickers <- colnames(tickers.gains)
    if (is.null(tickers)) {
      tickers <- paste("FUND", 1: ncol(tickers.gains), sep = "")
    }
    tickers <- matrix(tickers, nrow = 2)
    
    # Convert reference.gains to matrix if necessary, and figure out reference.tickers
    if (is.vector(reference.gains)) {
      reference.gains <- matrix(reference.gains, ncol = 1)
      reference.tickers <- "REF"
    } else if (is.matrix(reference.gains)) {
      reference.tickers <- colnames(reference.gains)
      if (is.null(reference.tickers)) {
        reference.tickers <- paste("REF", 1: ncol(reference.gains), sep = "")
      }
    }
    
    # Convert benchmark.gains to matrix if necessary, and figure out benchmark.tickers
    if (is.vector(benchmark.gains)) {
      benchmark.gains <- matrix(benchmark.gains, ncol = 1)
      benchmark.tickers <- "BENCH"
    } else if (is.matrix(benchmark.gains)) {
      benchmark.tickers <- colnames(benchmark.gains)
      if (is.null(benchmark.tickers)) {
        benchmark.tickers <- paste("BENCH", 1: ncol(benchmark.gains), sep = "")
      }
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
    if (x.metric == "mean") {

      means <- apply(tickers.gains.sub, 2, mean)
      x[[ii]] <- (fund1.all * means[1] + fund2.all * means[2]) * 100

    } else if (x.metric == "sd") {

      vars <- var(tickers.gains.sub)
      x[[ii]] <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + fund1.all * fund2.all * vars[1, 2]) * 100

    } else if (x.metric == "growth") {

      x[[ii]] <- apply(fund.all, 1, function(x) gains.rate(gains = tickers.gains.sub %*% x)) * 100

    } else if (x.metric == "cagr") {

      x[[ii]] <- apply(fund.all, 1, function(x) gains.rate(gains = tickers.gains.sub %*% x, units.rate = units.year)) * 100

    } else if (x.metric == "mdd") {

      x[[ii]] <- apply(fund.all, 1, function(x) mdd(gains = tickers.gains.sub %*% x)) * 100

    } else if (x.metric == "sharpe") {

      means <- apply(tickers.gains.sub, 2, mean)
      x1 <- fund.all %*% means
      vars <- var(tickers.gains.sub)
      x2 <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2])
      x[[ii]] <- x1 / x2

    } else if (x.metric == "sortino") {

      x[[ii]] <- apply(fund.all, 1, function(x) sortino.ratio(gains = tickers.gains.sub %*% x))

    } else if (x.metric == "alpha") {

      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 1])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 1])
      x[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100

    } else if (x.metric == "alpha2") {

      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 2])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 2])
      x[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100

    } else if (x.metric == "beta") {

      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 1])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 1])
      x[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]

    } else if (x.metric == "beta2") {

      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 2])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 2])
      x[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]

    } else if (x.metric == "r.squared") {

      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 1]))
      x[[ii]] <- ((fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3]))^2

    } else if (x.metric == "r.squared2") {

      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 2]))
      x[[ii]] <- ((fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
                    sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3]))^2

    } else if (x.metric == "pearson") {

      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 1]))
      x[[ii]] <- (fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3])

    } else if (x.metric == "pearson2") {

      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 2]))
      x[[ii]] <- (fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3])

    } else if (x.metric == "spearman") {

      x[[ii]] <- apply(fund.all, 1, function(x) cor(tickers.gains.sub %*% x, benchmark.gains[, 1], method = "spearman"))

    } else if (x.metric == "spearman2") {

      x[[ii]] <- apply(fund.all, 1, function(x) cor(tickers.gains.sub %*% x, benchmark.gains[, 2], method = "spearman"))

    } else if (x.metric == "auto.pearson") {

      vars <- var(cbind(tickers.gains.sub[1: (nrow(tickers.gains.sub) - 1), ], tickers.gains.sub[2: nrow(tickers.gains.sub), ]))
      x[[ii]] <- (fund1.all^2 * vars[1, 3] + fund1.all * fund2.all * vars[1, 4] + fund1.all * fund2.all * vars[2, 3] + fund2.all^2 * vars[2, 4]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) *
               (fund1.all^2 * vars[3, 3] + fund2.all^2 * vars[4, 4] + 2 * fund1.all * fund2.all * vars[3, 4]))

    } else if (x.metric == "auto.spearman") {

      num.gains <- nrow(tickers.gains.sub)
      x[[ii]] <- apply(fund.all, 1, function(x) cor((tickers.gains.sub %*% x)[-num.gains], (tickers.gains.sub %*% x)[-1], method = "spearman"))

    } else if (x.metric == "allocation") {

      x[[ii]] <- fund1.all * 100

    }

    # Calculate y-axis value for each allocation
    if (y.metric == "mean") {

      means <- apply(tickers.gains.sub, 2, mean)
      y[[ii]] <- (fund1.all * means[1] + fund2.all * means[2]) * 100

    } else if (y.metric == "sd") {

      vars <- var(tickers.gains.sub)
      y[[ii]] <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * 100

    } else if (y.metric == "growth") {

      y[[ii]] <- apply(fund.all, 1, function(x) gains.rate(gains = tickers.gains.sub %*% x)) * 100

    } else if (y.metric == "cagr") {

      y[[ii]] <- apply(fund.all, 1, function(x) gains.rate(gains = tickers.gains.sub %*% x, units.rate = units.year)) * 100

    } else if (y.metric == "mdd") {

      y[[ii]] <- apply(fund.all, 1, function(x) mdd(gains = tickers.gains.sub %*% x)) * 100

    } else if (y.metric == "sharpe") {

      means <- apply(tickers.gains.sub, 2, mean)
      y1 <- fund1.all * means[1] + fund2.all * means[2]
      vars <- var(tickers.gains.sub)
      y2 <- sqrt(fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2])
      y[[ii]] <- y1 / y2

    } else if (y.metric == "sortino") {

      y[[ii]] <- apply(fund.all, 1, function(x) sortino.ratio(gains = tickers.gains.sub %*% x))

    } else if (y.metric == "alpha") {

      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 1])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 1])
      y[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100

    } else if (y.metric == "alpha2") {

      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 2])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 2])
      y[[ii]] <- (fund1.all * fit1$coef[1] + fund2.all * fit2$coef[1]) * 100

    } else if (y.metric == "beta") {

      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 1])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 1])
      y[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]

    } else if (y.metric == "beta2") {

      fit1 <- lm(tickers.gains.sub[, 1] ~ benchmark.gains[, 2])
      fit2 <- lm(tickers.gains.sub[, 2] ~ benchmark.gains[, 2])
      y[[ii]] <- fund1.all * fit1$coef[2] + fund2.all * fit2$coef[2]

    } else if (y.metric == "r.squared") {

      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 1]))
      y[[ii]] <- ((fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
                    sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3]))^2

    } else if (y.metric == "r.squared2") {

      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 2]))
      y[[ii]] <- ((fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
                    sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3]))^2

    } else if (y.metric == "pearson") {

      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 1]))
      y[[ii]] <- (fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3])

    } else if (y.metric == "pearson2") {

      vars <- var(cbind(tickers.gains.sub, benchmark.gains[, 2]))
      y[[ii]] <- (fund1.all * vars[1, 3] + fund2.all * vars[2, 3]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) * vars[3, 3])

    } else if (y.metric == "spearman") {

      y[[ii]] <- apply(fund.all, 1, function(x) cor(tickers.gains.sub %*% x, benchmark.gains[, 1], method = "spearman"))

    } else if (y.metric == "spearman2") {

      y[[ii]] <- apply(fund.all, 1, function(x) cor(tickers.gains.sub %*% x, benchmark.gains[, 2], method = "spearman"))

    } else if (y.metric == "auto.pearson") {

      vars <- var(cbind(tickers.gains.sub[1: (nrow(tickers.gains.sub) - 1), ], tickers.gains.sub[2: nrow(tickers.gains.sub), ]))
      y[[ii]] <- (fund1.all^2 * vars[1, 3] + fund1.all * fund2.all * vars[1, 4] + fund1.all * fund2.all * vars[2, 3] + fund2.all^2 * vars[2, 4]) /
        sqrt((fund1.all^2 * vars[1, 1] + fund2.all^2 * vars[2, 2] + 2 * fund1.all * fund2.all * vars[1, 2]) *
               (fund1.all^2 * vars[3, 3] + fund2.all^2 * vars[4, 4] + 2 * fund1.all * fund2.all * vars[3, 4]))

    } else if (y.metric == "auto.spearman") {

      num.gains <- nrow(tickers.gains.sub)
      y[[ii]] <- apply(fund.all, 1, function(x) cor((tickers.gains.sub %*% x)[-num.gains], (tickers.gains.sub %*% x)[-1], method = "spearman"))

    } else if (y.metric == "allocation") {

      y[[ii]] <- fund1.all * 100

    }

    # Combine x and y values into two-column matrix
    portfolio.xy[[ii]] <- cbind(x[[ii]], y[[ii]])

  }

  # Create variables for plot
  x1 <- x2 <- y1 <- y2 <- NULL
  reference.y <- NULL
  if (y.metric == "mean") {
    plot.title <- paste("Mean of ", capitalize(time.scale), " Gains vs. ", sep = "")
    y.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, mean) * 100
    }
  } else if (y.metric == "sd") {
    plot.title <- paste("SD of ", capitalize(time.scale), " Gains vs. ", sep = "")
    y.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, sd) * 100
    }
    y1 <- 0
  } else if (y.metric == "growth") {
    plot.title <- "Total Growth vs. "
    y.label <- "Growth (%)"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) gains.rate(gains = x)) * 100
    }
  } else if (y.metric == "cagr") {
    plot.title <- "CAGR vs. "
    y.label <- "CAGR (%)"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (y.metric == "mdd") {
    plot.title <- "MDD vs. "
    y.label <- "MDD (%)"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
    y1 <- 0
  } else if (y.metric == "sharpe") {
    plot.title <- "Sharpe Ratio vs. "
    y.label <- "Sharpe ratio"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (y.metric == "sortino") {
    plot.title <- "Sharpe Ratio vs. "
    y.label <- "Sortino ratio"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sortino.ratio(gains = x))
    }
  } else if (y.metric == "alpha") {
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark.tickers[1], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 1])$coef[1]) * 100
    }
  } else if (y.metric == "alpha2") {
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark.tickers[2], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 2])$coef[1]) * 100
    }
  } else if (y.metric == "beta") {
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 1])$coef[2])
    }
  } else if (y.metric == "beta2") {
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 2])$coef[2])
    }
  } else if (y.metric == "r.squared") {
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) summary(lm(x ~ benchmark.gains[, 1]))$r.squared)
    }
    y1 <- 0
  } else if (y.metric == "r.squared2") {
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) summary(lm(x ~ benchmark.gains[, 2]))$r.squared)
    }
    y1 <- 0
  } else if (y.metric == "pearson") {
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 1]))
    }
  } else if (y.metric == "pearson2") {
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 2]))
    }
  } else if (y.metric == "spearman") {
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 1], method = "spearman"))
    }
  } else if (y.metric == "spearman2") {
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 2], method = "spearman"))
    }
  } else if (y.metric == "auto.pearson") {
    plot.title <- "Autocorrelation vs. "
    y.label <- "Pearson autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x[-length(x)], x[-1]))
    }
  } else if (y.metric == "auto.spearman") {
    plot.title <- "Autocorrelation vs. "
    y.label <- "Spearman autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
    }
  } else if (y.metric == "allocation") {
    plot.title <- "Allocation vs. "
    y.label <- "Allocation (%)"
    y1 <- -5
    y2 <- 105
  }

  reference.x <- NULL
  if (x.metric == "mean") {
    plot.title <- paste(plot.title, "Mean of ", capitalize(time.scale), " Gains", sep = "")
    x.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, mean) * 100
    }
  } else if (x.metric == "sd") {
    plot.title <- paste(plot.title, "SD of ", capitalize(time.scale), " Gains", sep = "")
    x.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, sd) * 100
    }
    x1 <- 0
  } else if (x.metric == "growth") {
    plot.title <- paste(plot.title, "Total Growth", sep = "")
    x.label <- "Growth (%)"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) gains.rate(gains = x)) * 100
    }
  } else if (x.metric == "cagr") {
    plot.title <- paste(plot.title, "CAGR", sep = "")
    x.label <- "CAGR (%)"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (x.metric == "mdd") {
    plot.title <- paste(plot.title, "MDD", sep = "")
    x.label <- "MDD (%)"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
    x1 <- 0
  } else if (x.metric == "sharpe") {
    plot.title <- paste(plot.title, "Sharpe Ratio", sep = "")
    x.label <- "Sharpe ratio"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (x.metric == "sortino") {
    plot.title <- paste(plot.title, "Sortino Ratio", sep = "")
    x.label <- "Sortino ratio"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (x.metric == "alpha") {
    plot.title <- paste(plot.title, "Alpha")
    x.label <- paste("Alpha w/ ", benchmark.tickers[1], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1]) * 100
    }
  } else if (x.metric == "alpha2") {
    plot.title <- paste(plot.title, "Alpha")
    x.label <- paste("Alpha w/ ", benchmark.tickers[2], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1]) * 100
    }
  } else if (x.metric == "beta") {
    plot.title <- paste(plot.title, "Beta")
    x.label <- paste("Beta w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    }
  } else if (x.metric == "beta2") {
    plot.title <- paste(plot.title, "Beta")
    x.label <- paste("Beta w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    }
  } else if (x.metric == "r.squared") {
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
    }
    x1 <- 0
  } else if (x.metric == "r.squared2") {
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
    }
    x1 <- 0
  } else if (x.metric == "pearson") {
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains))
    }
  } else if (x.metric == "pearson2") {
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains))
    }
  } else if (x.metric == "spearman") {
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains, method = "spearman"))
    }
  } else if (x.metric == "spearman") {
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains, method = "spearman"))
    }
  } else if (x.metric == "auto.pearson") {
    plot.title <- paste(plot.title, "Autocorrelation", "")
    x.label <- "Pearson autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x[-length(x)], x[-1]))
    }
  } else if (x.metric == "auto.spearman") {
    plot.title <- paste(plot.title, "Autocorrelation", sep = "")
    x.label <- "Spearman autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
    }
  } else if (x.metric == "allocation") {
    plot.title <- paste(plot.title, "Allocation")
    x.title <- "Allocation"
    x.label <- "Allocation (%)"
    x1 <- -5
    x2 <- 105
  }
  
  # If NULL, set appropriate values for xlim and ylim ranges
  if (is.null(x1) | is.null(x2) | is.null(y1) | is.null(y2)) {
    xvals <- c(unlist(x), reference.x)
    xvals.range <- range(xvals)
    yvals <- c(unlist(y), reference.y)
    yvals.range <- range(yvals)
    if (is.null(x1)) {
      x1 <- xvals.range[1] - 0.05 * diff(xvals.range)
    }
    if (is.null(x2)) {
      x2 <- xvals.range[2] + 0.05 * diff(xvals.range)
    }
    if (is.null(y1)) {
      y1 <- yvals.range[1] - 0.05 * diff(yvals.range)
    }
    if (is.null(y2)) {
      y2 <- yvals.range[2] + 0.05 * diff(yvals.range)
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

  # Figure out features of graph, based on user inputs where available
  plot.list <- list.override(list1 = list(x = 0, y = 0, type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xlab = x.label, ylab = y.label,
                                          xlim = c(x1, x2), ylim = c(y1, y2)),
                             list2 = plot.list)
  points.list <- list.override(list1 = list(pch = 16, cex = 0.7),
                               list2 = points.list)
  text.list <- list.override(list1 = list(cex = 0.7),
                             list2 = text.list)

  # Figure out positioning of ticker labels for 100% allocation to each fund
  if (is.null(tickerlabel.offsets)) {

    tickerlabel.offsets <- cbind(rep(0, n.pairs * 2), rep(NA, n.pairs * 2))
    y.offset.mag <- (y2 - y1) / 40
    for (ii in 1: ncol(tickers)) {

      # Put label for ticker with higher y-value above its data point, and label for other ticker below its data point
      fund1.xy <- c(x[[ii]][num.points], y[[ii]][num.points])
      fund2.xy <- c(x[[ii]][1], y[[ii]][1])
      whichmax.y <- which.max(c(fund1.xy[2], fund2.xy[2]))
      if (whichmax.y == 1) {
        tickerlabel.offsets[(ii * 2 - 1), 2] <- y.offset.mag
        tickerlabel.offsets[ii * 2, 2] <- -y.offset.mag
      } else {
        tickerlabel.offsets[(ii * 2 - 1), 2] <- -y.offset.mag
        tickerlabel.offsets[ii * 2, 2] <- y.offset.mag
      }
    }

  } else if (length(tickerlabel.offsets) == 2) {
    tickerlabel.offsets <- cbind(rep(tickerlabel.offsets[1], n.pairs * 2), rep(tickerlabel.offsets[2], n.pairs * 2))
  }
  if (is.null(reflabel.offsets) & !is.null(reference.tickers)) {
    reflabel.offsets <- cbind(rep(0, length(reference.tickers)), rep((y2 - y1) / 40, length(reference.tickers)))
  } else if (length(tickerlabel.offsets) == 2) {
    tickerlabel.offsets <- cbind(rep(tickerlabel.offsets[1], n.pairs * 2), rep(tickerlabel.offsets[2], n.pairs * 2))
  }

  # If pdf.list is not NULL, call pdf
  if (!is.null(pdf.list)) {
    if (is.null(pdf.list$file)) {
      pdf.list$file <- "figure1.pdf"
    }
    do.call(pdf, pdf.list)
  }

  # If bmp.list is not NULL, call bmp
  if (!is.null(bmp.list)) {
    if (is.null(bmp.list$file)) {
      bmp.list$file <- "figure1.bmp"
    }
    do.call(bmp, bmp.list)
  }

  # If jpeg.list is not NULL, call jpeg
  if (!is.null(jpeg.list)) {
    if (is.null(jpeg.list$file)) {
      jpeg.list$file <- "figure1.jpg"
    }
    do.call(jpeg, jpeg.list)
  }

  # If png.list is not NULL, call png
  if (!is.null(png.list)) {
    if (is.null(png.list$file)) {
      png.list$file <- "figure1.png"
    }
    do.call(png, png.list)
  }

  # If tiff.list is not NULL, call tiff
  if (!is.null(tiff.list)) {
    if (is.null(tiff.list$file)) {
      tiff.list$file <- "figure1.tif"
    }
    do.call(tiff, tiff.list)
  }

  # Create plot region
  if (! add.plot) {
    do.call(plot, plot.list)
  }

  # Add horizontal/vertical lines if useful for requested metrics
  if (y.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2", "beta", "beta2", "pearson", "pearson2",
                      "spearman", "spearman2", "auto.pearson", "auto.spearman", "growth", "cagr")) {
    abline(h = 0, lty = 2)
  } else if (y.metric %in% c("r.squared", "r.squared2")) {
    abline(h = 1, lty = 2)
  }
  if (x.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2", "beta", "beta2", "pearson", "pearson2",
                      "spearman", "spearman2", "auto.pearson", "auto.spearman", "growth", "cagr")) {
    abline(v = 0, lty = 2)
  } else if (x.metric %in% c("r.squared", "r.squared2")) {
    abline(v = 1, lty = 2)
  }

  # Figure out indices for data points
  if (!is.null(step.points)) {
    locs.points <- seq(1, num.points, step.points / step.data)
  } else {
    locs.points <- c(1, num.points)
  }

  # Change scaled thing! Make it 2x VFINX, for example. Also add input for user override.
  # Add curves for each pair
  for (ii in 1: n.pairs) {

    # Add colored curves and data points
    do.call(points, c(list(x = x[[ii]], y = y[[ii]], type = "l", col = colors[ii]), points.list))
    do.call(points, c(list(x = x[[ii]][locs.points], y = y[[ii]][locs.points], col = colors[ii]), points.list))

    # Figure out (x, y) coordinates for 100% fund 1 and 100% fund 2
    fund1.xy <- c(x[[ii]][num.points], y[[ii]][num.points])
    fund2.xy <- c(x[[ii]][1], y[[ii]][1])

    # Add black data points at 100% fund 1 and 100% fund2
    do.call(points, c(list(x = fund1.xy[1], y = fund1.xy[2]), points.list))
    do.call(points, c(list(x = fund2.xy[1], y = fund2.xy[2]), points.list))

    # Add text labels if not already on plot
    if (ii == 1 | ! tickers[1, ii] %in% tickers[, 1: (ii - 1)]) {
      do.call(text, c(list(x = fund1.xy[1] + tickerlabel.offsets[(ii * 2 - 1), 1],
                           y = fund1.xy[2] + tickerlabel.offsets[(ii * 2 - 1), 2],
                           label = paste("100% ", tickers[1, ii], sep = "")), text.list))
    }
    if (ii == 1 | ! tickers[2, ii] %in% tickers[, 1: (ii - 1)]) {
      do.call(text, c(list(x = fund2.xy[1] + tickerlabel.offsets[ii * 2, 1],
                           y = fund2.xy[2] + tickerlabel.offsets[ii * 2, 2],
                           label = paste("100% ", tickers[2, ii], sep = "")), text.list))
    }

  }

  # Add data point for reference funds (if given)
  if (!is.null(reference.tickers)) {

    # Loop through and add data points for each reference fund
    for (ii in 1: ncol(reference.gains)) {

      if (x.metric != "allocation") {

        do.call(points, c(list(x = reference.x[ii], y = reference.y[ii], type = "p", col = "black"), points.list))
        if (! reference.tickers[ii] %in% tickers) {
          do.call(text, c(list(x = reference.x[ii] + reflabel.offsets[ii, 1],
                               y = reference.y[ii] + reflabel.offsets[ii, 2],
                               label = reference.tickers[ii]),
                          text.list))
        }
      } else {
        abline(h = reference.y[ii], lty = 2, col = "black")
        do.call(text, c(list = c(list(x = 20,
                                      y = reference.y[ii] + reflabel.offsets[ii, 2],
                                      label = reference.tickers[ii]),
                                 text.list)))
      }
    }

  }

  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }

  # Return portfolio.xy, mean for each fund and correlation matrix
  means <- apply(gains, 2, mean)
  corr.matrix <- cor(gains)
  return.list <- list(portfolio.xy = portfolio.xy, means = means, corr.matrix = corr.matrix)
  return(return.list)

}


threefunds.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                             benchmark.tickers = NULL, reference.tickers = NULL,
                             tickers.gains = NULL, benchmark.gains = NULL, reference.gains = NULL,
                             from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                             step.data = 0.0025, step.points = 0.1, step.curves = 0.2,
                             x.metric = "sd", y.metric = "mean",
                             tickerlabel.offsets = NULL,
                             reflabel.offsets = NULL,
                             add.plot = FALSE,
                             colors = NULL,
                             plot.list = NULL,
                             points.list = NULL,
                             text.list = NULL,
                             pdf.list = NULL,
                             bmp.list = NULL,
                             jpeg.list = NULL,
                             png.list = NULL,
                             tiff.list = NULL) {

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
    extra.tickers <- unique(c(benchmark.tickers, reference.tickers))

    # Calculate gains matrix
    tickers.vec <- c(as.vector(tickers), extra.tickers)
    intercepts.vec <- c(as.vector(intercepts), rep(0, length(extra.tickers)))
    slopes.vec <- c(as.vector(slopes), rep(1, length(extra.tickers)))
    gains <- load.gains(tickers = tickers.vec,
                        from = from, to = to,
                        intercepts = intercepts.vec, slopes = slopes.vec,
                        time.scale = time.scale,
                        earliest.subset = FALSE)

    # Update ticker names to show intercept/slope
    tickers <- matrix(colnames(gains)[1: length(tickers)], nrow = 3)

    # Separate benchmark gains, reference gains, and ticker gains
    tickers.gains <- gains[, 1: length(tickers), drop = F]
    extra.gains <- gains[, -c(1: length(tickers)), drop = F]
    if (!is.null(benchmark.tickers)) {
      benchmark.gains <- extra.gains[, 1: length(benchmark.tickers), drop = F]
      extra.gains <- extra.gains[, -c(1: length(benchmark.tickers)), drop = F]
    }
    if (!is.null(reference.tickers)) {
      reference.gains <- extra.gains
    }

  } else {

    # Figure out tickers from tickers.gains
    tickers <- colnames(tickers.gains)
    if (is.null(tickers)) {
      tickers <- paste("FUND", 1: ncol(tickers.gains), sep = "")
    }
    tickers <- matrix(tickers, nrow = 3)

    # Convert reference.gains to matrix if necessary, and figure out reference.tickers
    if (is.vector(reference.gains)) {
      reference.gains <- matrix(reference.gains, ncol = 1)
      reference.tickers <- "REF"
    } else if (is.matrix(reference.gains)) {
      reference.tickers <- colnames(reference.gains)
      if (is.null(reference.tickers)) {
        reference.tickers <- paste("REF", 1: ncol(reference.gains), sep = "")
      }
    }

    # Convert benchmark.gains to matrix if necessary, and figure out benchmark.tickers
    if (is.vector(benchmark.gains)) {
      benchmark.gains <- matrix(benchmark.gains, ncol = 1)
      benchmark.tickers <- "BENCH"
    } else if (is.matrix(benchmark.gains)) {
      benchmark.tickers <- colnames(benchmark.gains)
      if (is.null(benchmark.tickers)) {
        benchmark.tickers <- paste("BENCH", 1: ncol(benchmark.gains), sep = "")
      }
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
    if (x.metric == "mean") {

      means <- apply(tickers.gains.sub, 2, mean) * 100
      x <- lapply(fund1.all, function(x) x * means[1] + (1 - x) * fund2.all * means[2] + (1 - x) * fund3.all * means[3])

    } else if (x.metric == "sd") {

      vars <- var(tickers.gains.sub * 100)
      x <- lapply(fund1.all, function(x) {
        sqrt(x^2 * vars[1, 1] + ((1 - x) * fund2.all)^2 * vars[2, 2] + ((1 - x) * fund3.all)^2 * vars[3, 3] +
               2 * x * (1 - x) * fund2.all * vars[1, 2] + 2 * x * (1 - x) * fund3.all * vars[1, 3] +
               2 * (1 - x) * fund2.all * (1 - x) * fund3.all * vars[2, 3])
      })

    } else if (x.metric == "growth") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) gains.rate(gains = x) * 100)
      })

    } else if (x.metric == "cagr") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) gains.rate(gains = x, units.rate = units.year) * 100)
      })

    } else if (x.metric == "mdd") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) mdd(gains = x) * 100)
      })

    } else if (x.metric == "sharpe") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) sharpe.ratio(gains = x))
      })

    } else if (x.metric == "sortino") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) sortino.ratio(gains = x))
      })

    } else if (x.metric == "alpha") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 1])$coef[1] * 100)
      })

    } else if (x.metric == "alpha2") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 2])$coef[1] * 100)
      })

    } else if (x.metric == "beta") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 1])$coef[2] * 100)
      })

    } else if (x.metric == "beta2") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 2])$coef[2] * 100)
      })

    } else if (x.metric == "r.squared") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) summary(lm(x ~ benchmark.gains[, 1]))$r.squared)
      })

    } else if (x.metric == "r.squared2") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) summary(lm(x ~ benchmark.gains[, 2]))$r.squared)
      })

    } else if (x.metric == "pearson") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 1]))
      })

    } else if (x.metric == "pearson2") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 2]))
      })

    } else if (x.metric == "spearman") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 1], method = "spearman"))
      })

    } else if (x.metric == "spearman2") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 2], method = "spearman"))
      })

    } else if (x.metric == "auto.pearson") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x[-length(x)], x[-1]))
      })

    } else if (x.metric == "auto.spearman") {

      x <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
      })

    } else if (x.metric == "allocation") {

      x <- lapply(fund1.all, function(x) fund2.all * 100)

    }

    # Calculate y-axis value for each allocation
    if (y.metric == "mean") {

      means <- apply(tickers.gains.sub, 2, mean) * 100
      y <- lapply(fund1.all, function(x) x * means[1] + (1 - x) * fund2.all * means[2] + (1 - x) * fund3.all * means[3])

    } else if (y.metric == "sd") {

      vars <- var(tickers.gains.sub * 100)
      y <- lapply(fund1.all, function(x) {
        sqrt(x^2 * vars[1, 1] + ((1 - x) * fund2.all)^2 * vars[2, 2] + ((1 - x) * fund3.all)^2 * vars[3, 3] +
               2 * x * (1 - x) * fund2.all * vars[1, 2] + 2 * x * (1 - x) * fund3.all * vars[1, 3] +
               2 * (1 - x) * fund2.all * (1 - x) * fund3.all * vars[2, 3])
      })

    } else if (y.metric == "growth") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) gains.rate(gains = x) * 100)
      })

    } else if (y.metric == "cagr") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) gains.rate(gains = x, units.rate = units.year) * 100)
      })

    } else if (y.metric == "mdd") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) mdd(gains = x) * 100)
      })

    } else if (y.metric == "sharpe") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) sharpe.ratio(gains = x))
      })

    } else if (y.metric == "sortino") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) sortino.ratio(gains = x))
      })

    } else if (y.metric == "alpha") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 1])$coef[1] * 100)
      })

    } else if (y.metric == "alpha2") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 2])$coef[1] * 100)
      })

    } else if (y.metric == "beta") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 1])$coef[2] * 100)
      })

    } else if (y.metric == "beta2") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) lm(x ~ benchmark.gains[, 2])$coef[2] * 100)
      })

    } else if (y.metric == "r.squared") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) summary(lm(x ~ benchmark.gains[, 1]))$r.squared)
      })

    } else if (y.metric == "r.squared2") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) summary(lm(x ~ benchmark.gains[, 2]))$r.squared)
      })

    } else if (y.metric == "pearson") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 1]))
      })

    } else if (y.metric == "pearson2") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 2]))
      })

    } else if (y.metric == "spearman") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 1], method = "spearman"))
      })

    } else if (y.metric == "spearman2") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x, benchmark.gains[, 2], method = "spearman"))
      })

    } else if (y.metric == "auto.pearson") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x[-length(x)], x[-1]))
      })

    } else if (y.metric == "auto.spearman") {

      y <- lapply(fund1.all, function(x) {
        apply(tickers.gains.sub %*% matrix(c(rep(x, num.points),
                                             (1 - x) * fund2.all,
                                             (1 - x) * fund3.all),
                                           nrow = 3, byrow = TRUE),
              2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
      })

    } else if (y.metric == "allocation") {

      y <- lapply(fund1.all, function(x) fund2.all * 100)

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
  x1 <- x2 <- y1 <- y2 <- NULL
  reference.y <- NULL
  if (y.metric == "mean") {
    plot.title <- paste("Mean of ", capitalize(time.scale), " Gains vs. ", sep = "")
    y.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, mean) * 100
    }
  } else if (y.metric == "sd") {
    plot.title <- paste("SD of ", capitalize(time.scale), " Gains vs. ", sep = "")
    y.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, sd) * 100
    }
    y1 <- 0
  } else if (y.metric == "growth") {
    plot.title <- "Total Growth vs. "
    y.label <- "Growth (%)"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) gains.rate(gains = x)) * 100
    }
  } else if (y.metric == "cagr") {
    plot.title <- "CAGR vs. "
    y.label <- "CAGR (%)"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (y.metric == "mdd") {
    plot.title <- "MDD vs. "
    y.label <- "MDD (%)"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
    y1 <- 0
  } else if (y.metric == "sharpe") {
    plot.title <- "Sharpe Ratio vs. "
    y.label <- "Sharpe ratio"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (y.metric == "sortino") {
    plot.title <- "Sortino Ratio vs. "
    y.label <- "Sortino ratio"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) sortino.ratio(gains = x))
    }
  } else if (y.metric == "alpha") {
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark.tickers[1], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 1])$coef[1]) * 100
    }
  } else if (y.metric == "alpha2") {
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark.tickers[2], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 2])$coef[1]) * 100
    }
  } else if (y.metric == "beta") {
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 1])$coef[2])
    }
  } else if (y.metric == "beta2") {
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 2])$coef[2])
    }
  } else if (y.metric == "r.squared") {
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) summary(lm(x ~ benchmark.gains[, 1]))$r.squared)
    }
    y1 <- 0
  } else if (y.metric == "r.squared2") {
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) summary(lm(x ~ benchmark.gains[, 2]))$r.squared)
    }
    y1 <- 0
  } else if (y.metric == "pearson") {
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 1]))
    }
  } else if (y.metric == "pearson2") {
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 2]))
    }
  } else if (y.metric == "spearman") {
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 1], method = "spearman"))
    }
  } else if (y.metric == "spearman2") {
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 2], method = "spearman"))
    }
  } else if (y.metric == "auto.pearson") {
    plot.title <- "Autocorrelation vs. "
    y.label <- "Pearson autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x[-length(x)], x[-1]))
    }
  } else if (y.metric == "auto.spearman") {
    plot.title <- "Autocorrelation vs. "
    y.label <- "Spearman autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.y <- apply(reference.gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
    }
  } else if (y.metric == "allocation") {
    plot.title <- "Allocation vs. "
    y.label <- "Allocation (%)"
    y1 <- -5
    y2 <- 105
  }

  reference.x <- NULL
  if (x.metric == "mean") {
    plot.title <- paste(plot.title, "Mean of ", capitalize(time.scale), " Gains", sep = "")
    x.label <- paste("Mean of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, mean) * 100
    }
  } else if (x.metric == "sd") {
    plot.title <- paste(plot.title, "SD of ", capitalize(time.scale), " Gains", sep = "")
    x.label <- paste("SD of ", time.scale, " gains (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, sd) * 100
    }
    x1 <- 0
  } else if (x.metric == "growth") {
    plot.title <- paste(plot.title, "Total Growth", sep = "")
    x.label <- "Growth (%)"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) gains.rate(gains = x)) * 100
    }
  } else if (x.metric == "cagr") {
    plot.title <- paste(plot.title, "CAGR", sep = "")
    x.label <- "CAGR (%)"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    }
  } else if (x.metric == "mdd") {
    plot.title <- paste(plot.title, "MDD", sep = "")
    x.label <- "MDD (%)"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) mdd(gains = x)) * 100
    }
    x1 <- 0
  } else if (x.metric == "sharpe") {
    plot.title <- paste(plot.title, "Sharpe Ratio", sep = "")
    x.label <- "Sharpe ratio"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (x.metric == "sortino") {
    plot.title <- paste(plot.title, "Sortino Ratio", sep = "")
    x.label <- "Sortino ratio"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) sharpe.ratio(gains = x))
    }
  } else if (x.metric == "alpha") {
    plot.title <- paste(plot.title, "Alpha", sep = "")
    x.label <- paste("Alpha w/ ", benchmark.tickers[1], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 1])$coef[1]) * 100
    }
  } else if (x.metric == "alpha2") {
    plot.title <- paste(plot.title, "Alpha", sep = "")
    x.label <- paste("Alpha w/ ", benchmark.tickers[2], " (%)", sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 2])$coef[1]) * 100
    }
  } else if (x.metric == "beta") {
    plot.title <- paste(plot.title, "Beta", sep = "")
    x.label <- paste("Beta w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 1])$coef[2])
    }
  } else if (x.metric == "beta2") {
    plot.title <- paste(plot.title, "Beta", sep = "")
    x.label <- paste("Beta w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) lm(x ~ benchmark.gains[, 2])$coef[2])
    }
  } else if (x.metric == "r.squared") {
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) summary(lm(x ~ benchmark.gains[, 1]))$r.squared)
    }
    x1 <- 0
  } else if (x.metric == "r.squared2") {
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) summary(lm(x ~ benchmark.gains[, 2]))$r.squared)
    }
    x1 <- 0
  } else if (x.metric == "pearson") {
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 1]))
    }
  } else if (x.metric == "pearson") {
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 2]))
    }
  } else if (x.metric == "spearman") {
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.tickers[1], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 1], method = "spearman"))
    }
  } else if (x.metric == "spearman") {
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.tickers[2], sep = "")
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x, benchmark.gains[, 2], method = "spearman"))
    }
  } else if (x.metric == "auto.pearson") {
    plot.title <- paste(plot.title, "Autocorrelation", sep = "")
    x.label <- "Pearson autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x[-length(x)], x[-1]))
    }
  } else if (x.metric == "auto.spearman") {
    plot.title <- paste(plot.title, "Autocorrelation", sep = "")
    x.label <- "Spearman autocorrelation"
    if (!is.null(reference.tickers)) {
      reference.x <- apply(reference.gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
    }
  } else if (x.metric == "allocation") {
    plot.title <- paste(plot.title, "Allocation", sep = "")
    x.label <- "Allocation (%)"
    x1 <- -5
    x2 <- 105
  }
  
  # If NULL, set appropriate values for xlim and ylim ranges
  if (is.null(x1) | is.null(x2) | is.null(y1) | is.null(y2)) {
    xvals <- c(unlist(x), reference.x)
    xvals.range <- range(xvals)
    yvals <- c(unlist(y), reference.y)
    yvals.range <- range(yvals)
    if (is.null(x1)) {
      x1 <- xvals.range[1] - 0.05 * diff(xvals.range)
    }
    if (is.null(x2)) {
      x2 <- xvals.range[2] + 0.05 * diff(xvals.range)
    }
    if (is.null(y1)) {
      y1 <- yvals.range[1] - 0.05 * diff(yvals.range)
    }
    if (is.null(y2)) {
      y2 <- yvals.range[2] + 0.05 * diff(yvals.range)
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

  # Figure out features of graph, based on user inputs where available
  plot.list <- list.override(list1 = list(x = 0, y = 0, type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xlab = x.label, ylab = y.label,
                                          xlim = c(x1, x2), ylim = c(y1, y2)),
                             list2 = plot.list)
  points.list <- list.override(list1 = list(pch = 16, cex = 0.7),
                               list2 = points.list)
  text.list <- list.override(list1 = list(cex = 0.7),
                             list2 = text.list)

  # Figure out positioning of ticker labels for 100% allocation to each fund
  if (is.null(tickerlabel.offsets)) {

    tickerlabel.offsets <- cbind(rep(0, n.sets * 3), rep(NA, n.sets * 3))
    y.offset.mag <- (y2 - y1) / 40
    for (ii in 1: ncol(tickers)) {

      # Put label for ticker with higher y-value above its data point, and label for other ticker below its data point
      fund1.xy <- portfolio.xy[[ii]][[num.curves]][1, 1: 2]
      fund2.xy <- portfolio.xy[[ii]][[1]][num.points, 1: 2]
      fund3.xy <- portfolio.xy[[ii]][[1]][1, 1: 2]
      whichmax.y <- which.max(c(fund1.xy[2], fund2.xy[2], fund3.xy[2]))
      if (whichmax.y == 1) {
        tickerlabel.offsets[(ii * 3 - 2), 2] <- y.offset.mag
        tickerlabel.offsets[(ii * 3 - 1), 2] <- -y.offset.mag
        tickerlabel.offsets[ii * 3, 2] <- -y.offset.mag
      } else if (whichmax.y == 2) {
        tickerlabel.offsets[(ii * 3 - 2), 2] <- -y.offset.mag
        tickerlabel.offsets[(ii * 3 - 1), 2] <- y.offset.mag
        tickerlabel.offsets[ii * 3, 2] <- -y.offset.mag
      } else {
        tickerlabel.offsets[(ii * 3 - 2), 2] <- -y.offset.mag
        tickerlabel.offsets[(ii * 3 - 1), 2] <- -y.offset.mag
        tickerlabel.offsets[ii * 3, 2] <- y.offset.mag
      }
    }

  } else if (length(tickerlabel.offsets) == 3) {
    tickerlabel.offsets <- cbind(rep(tickerlabel.offsets[1], n.sets * 3), rep(tickerlabel.offsets[2], n.sets * 3))
  }
  if (is.null(reflabel.offsets) & !is.null(reference.tickers)) {
    reflabel.offsets <- cbind(rep(0, length(reference.tickers)), rep((y2 - y1) / 40, length(reference.tickers)))
  } else if (length(tickerlabel.offsets) == 2) {
    tickerlabel.offsets <- cbind(rep(tickerlabel.offsets[1], n.sets * 3), rep(tickerlabel.offsets[2], n.sets * 3))
  }

  # If pdf.list is not NULL, call pdf
  if (!is.null(pdf.list)) {
    if (is.null(pdf.list$file)) {
      pdf.list$file <- "figure1.pdf"
    }
    do.call(pdf, pdf.list)
  }

  # If bmp.list is not NULL, call bmp
  if (!is.null(bmp.list)) {
    if (is.null(bmp.list$file)) {
      bmp.list$file <- "figure1.bmp"
    }
    do.call(bmp, bmp.list)
  }

  # If jpeg.list is not NULL, call jpeg
  if (!is.null(jpeg.list)) {
    if (is.null(jpeg.list$file)) {
      jpeg.list$file <- "figure1.jpg"
    }
    do.call(jpeg, jpeg.list)
  }

  # If png.list is not NULL, call png
  if (!is.null(png.list)) {
    if (is.null(png.list$file)) {
      png.list$file <- "figure1.png"
    }
    do.call(png, png.list)
  }

  # If tiff.list is not NULL, call tiff
  if (!is.null(tiff.list)) {
    if (is.null(tiff.list$file)) {
      tiff.list$file <- "figure1.tif"
    }
    do.call(tiff, tiff.list)
  }

  # Create plot region
  if (! add.plot) {
    do.call(plot, plot.list)
  }

  # Add horizontal/vertical lines if useful for requested metrics
  if (y.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2", "beta", "beta2", "pearson", "pearson2",
                      "spearman", "spearman2", "auto.pearson", "auto.spearman", "growth", "cagr")) {
    abline(h = 0, lty = 2)
  } else if (y.metric %in% c("r.squared", "r.squared2")) {
    abline(h = 1, lty = 2)
  }
  if (x.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2", "beta", "beta2", "pearson", "pearson2",
                      "spearman", "spearman2", "auto.pearson", "auto.spearman", "growth", "cagr")) {
    abline(v = 0, lty = 2)
  } else if (x.metric %in% c("r.squared", "r.squared2")) {
    abline(v = 1, lty = 2)
  }

  # Figure out indices for data points
  if (!is.null(step.points)) {
    locs.points <- seq(1, num.points, step.points / step.data)
  } else {
    locs.points <- c(1, num.points)
  }

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

    # Add black data points at 100% fund 1, 100% fund2, and 100% fund 3
    do.call(points, c(list(x = fund1.xy[1], y = fund1.xy[2]), points.list))
    do.call(points, c(list(x = fund2.xy[1], y = fund2.xy[2]), points.list))
    do.call(points, c(list(x = fund3.xy[1], y = fund3.xy[2]), points.list))

    # Add text labels if not already on plot
    if (ii == 1 | ! tickers[1, ii] %in% tickers[, 1: (ii - 1)]) {
      do.call(text, c(list(x = fund1.xy[1] + tickerlabel.offsets[(ii * 3 - 2), 1],
                           y = fund1.xy[2] + tickerlabel.offsets[(ii * 3 - 2), 2],
                           label = paste("100% ", tickers[1, ii], sep = "")), text.list))
    }
    if (ii == 1 | ! tickers[2, ii] %in% tickers[, 1: (ii - 1)]) {
      do.call(text, c(list(x = fund2.xy[1] + tickerlabel.offsets[(ii * 3 - 1), 1],
                           y = fund2.xy[2] + tickerlabel.offsets[(ii * 3 - 1), 2],
                           label = paste("100% ", tickers[2, ii], sep = "")), text.list))
    }
    if (ii == 1 | ! tickers[3, ii] %in% tickers[, 1: (ii - 1)]) {
      do.call(text, c(list(x = fund3.xy[1] + tickerlabel.offsets[(ii * 3), 1],
                           y = fund3.xy[2] + tickerlabel.offsets[ii * 3, 2],
                           label = paste("100% ", tickers[3, ii], sep = "")), text.list))
    }

  }

  # Add data point for reference funds (if given)
  if (!is.null(reference.tickers)) {

    # Loop through and add data points for each reference fund
    for (ii in 1: ncol(reference.gains)) {

      if (x.metric != "allocation") {

        do.call(points, c(list(x = reference.x[ii], y = reference.y[ii], type = "p", col = "black"), points.list))
        if (! reference.tickers[ii] %in% tickers) {
          do.call(text, c(list(x = reference.x[ii] + reflabel.offsets[ii, 1],
                               y = reference.y[ii] + reflabel.offsets[ii, 2],
                               label = reference.tickers[ii]),
                          text.list))
        }
      } else {
        abline(h = reference.y[ii], lty = 2, col = "black")
        do.call(text, c(list(x = 20,
                             y = reference.y[ii] + reflabel.offsets[ii, 2],
                             label = reference.tickers[ii]),
                        text.list))
      }
    }

  }

  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
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
                         earliest.subset = TRUE,
                         initial = 10000,
                         add.plot = FALSE,
                         colors = NULL,
                         plot.list = NULL,
                         points.list = NULL,
                         grid.list = NULL,
                         legend.list = NULL,
                         pdf.list = NULL,
                         bmp.list = NULL,
                         jpeg.list = NULL,
                         png.list = NULL,
                         tiff.list = NULL) {

  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {

    # Obtain matrix of prices for each fund
    prices <- load.prices(tickers = tickers, intercepts = intercepts, slopes = slopes,
                          from = from, to = to,
                          time.scale = time.scale,
                          earliest.subset = earliest.subset,
                          initial = initial)

  }

  # Set tickers to column names of prices matrix; if NULL, use Fund 1, Fund 2, ...
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
  plot.list <- list.override(list1 = list(x = dates, y = prices[, 1], type = "n",
                                          main = paste("Growth of $", initial, sep = ""), cex.main = 1.25,
                                          xlab = "Date", ylab = "Balance ($)",
                                          xlim = range(dates), ylim = c(0, max(prices) * 1.05)),
                             list2 = plot.list)
  legend.list <- list.override(list1 = list(x = "topleft", lty = 1, col = colors, legend = tickers),
                               list2 = legend.list)
  grid.list <- list.override(list1 = list(nx = 0, ny = NULL),
                             list2 = grid.list)

  # If pdf.list is not NULL, call pdf
  if (!is.null(pdf.list)) {
    if (is.null(pdf.list$file)) {
      pdf.list$file <- "figure1.pdf"
    }
    do.call(pdf, pdf.list)
  }

  # If bmp.list is not NULL, call bmp
  if (!is.null(bmp.list)) {
    if (is.null(bmp.list$file)) {
      bmp.list$file <- "figure1.bmp"
    }
    do.call(bmp, bmp.list)
  }

  # If jpeg.list is not NULL, call jpeg
  if (!is.null(jpeg.list)) {
    if (is.null(jpeg.list$file)) {
      jpeg.list$file <- "figure1.jpg"
    }
    do.call(jpeg, jpeg.list)
  }

  # If png.list is not NULL, call png
  if (!is.null(png.list)) {
    if (is.null(png.list$file)) {
      png.list$file <- "figure1.png"
    }
    do.call(png, png.list)
  }

  # If tiff.list is not NULL, call tiff
  if (!is.null(tiff.list)) {
    if (is.null(tiff.list$file)) {
      tiff.list$file <- "figure1.tif"
    }
    do.call(tiff, tiff.list)
  }


  # Create plot region
  if (! add.plot) {
    do.call(plot, plot.list)
  }

  # Add lines for each fund
  for (ii in 1: ncol(prices)) {
    do.call(points, c(list(x = dates, y = prices[, ii], type = "l", col = colors[ii]), points.list))
  }
  
  # Add grid lines
  do.call(grid, grid.list)

  # Add legend
  do.call(legend, legend.list)

  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }

  # Return prices matrix, mean of gains for each fund, and correlation matrix
  gains <- apply(prices, 2, pchanges) * 100
  means <- apply(gains, 2, mean)
  corr.matrix <- cor(gains)
  return.list <- list(prices = prices, means = means, corr.matrix = corr.matrix)
  return(return.list)

}


gains.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                        gains = NULL, prices = NULL,
                        from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                        earliest.subset = TRUE,
                        orders = 1,
                        add.plot = FALSE,
                        colors = NULL,
                        plot.list = NULL,
                        points.list = NULL,
                        legend.list = NULL,
                        pdf.list = NULL,
                        bmp.list = NULL,
                        jpeg.list = NULL,
                        png.list = NULL,
                        tiff.list = NULL) {

  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {

    # Obtain matrix of gains for each fund
    gains <- load.gains(tickers = tickers, from = from, to = to,
                        intercepts = intercepts, slopes = slopes,
                        time.scale = time.scale,
                        earliest.subset = FALSE) * 100

  } else if (!is.null(prices)) {

    # Calculate gains based on price data
    gains <- apply(prices, 2, pchanges) * 100

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

  # Create color scheme for plot
  n.tickers <- length(tickers)
  if (is.null(colors)) {
    if (n.tickers == 2) {
      colors <- "black"
    } else if (n.tickers == 3) {
      colors <- c("blue", "red")
    } else if (n.tickers == 4) {
      colors <- c("blue", "red", "orange")
    } else if (n.tickers == 5) {
      colors <- c("blue", "red", "orange", "purple")
    } else if (n.tickers > 5) {
      colors <- colorRampPalette(c("blue", "red"))(n.tickers - 1)
    }
  }

  # Figure out features of graph, based on user inputs where available
  time.scale.cap <- capitalize(time.scale)
  plot.list <- list.override(list1 = list(x = 0, y = 0, type = "n",
                                          main = paste("Scatterplot of", time.scale.cap, "Gains"),
                                          cex.main = 1.25,
                                          xlab = paste(time.scale.cap, "gains for", tickers[1], "(%)"),
                                          ylab = ifelse(n.tickers == 2, paste(time.scale.cap, "gains for", tickers[2]), paste(time.scale.cap, "gains (%)")),
                                          xlim = range(gains[, 1]) * 1.05,
                                          ylim = range(gains[, -1]) * 1.05),
                             list2 = plot.list)
  points.list <- list.override(list1 = list(cex = 0.7),
                               list2 = points.list)

  # If pdf.list is not NULL, call pdf
  if (!is.null(pdf.list)) {
    if (is.null(pdf.list$file)) {
      pdf.list$file <- "figure1.pdf"
    }
    do.call(pdf, pdf.list)
  }

  # If bmp.list is not NULL, call bmp
  if (!is.null(bmp.list)) {
    if (is.null(bmp.list$file)) {
      bmp.list$file <- "figure1.bmp"
    }
    do.call(bmp, bmp.list)
  }

  # If jpeg.list is not NULL, call jpeg
  if (!is.null(jpeg.list)) {
    if (is.null(jpeg.list$file)) {
      jpeg.list$file <- "figure1.jpg"
    }
    do.call(jpeg, jpeg.list)
  }

  # If png.list is not NULL, call png
  if (!is.null(png.list)) {
    if (is.null(png.list$file)) {
      png.list$file <- "figure1.png"
    }
    do.call(png, png.list)
  }

  # If tiff.list is not NULL, call tiff
  if (!is.null(tiff.list)) {
    if (is.null(tiff.list$file)) {
      tiff.list$file <- "figure1.tif"
    }
    do.call(tiff, tiff.list)
  }

  # Create plot region
  if (! add.plot) {
    do.call(plot, plot.list)
  }

  # If orders is NULL, set to 1's; if scalar, extend to vector
  if (is.null(orders)) {
    orders <- rep(1, n.tickers - 1)
  } else if (length(orders) == 1 & n.tickers > 2) {
    orders <- rep(orders, n.tickers - 1)
  }

  # Add dotted lines at x = 0 and at y = 0
  abline(h = 0, lty = 2)
  abline(v = 0, lty = 2)

  # Add points and regression line for each fund
  lm.fits <- list()
  legend.entries <- list()
  for (ii in 1: (n.tickers - 1)) {
    do.call(points, c(list(x = gains[, 1], y = gains[, ii + 1], col = colors[ii]), points.list))
    if (orders[ii] == 1) {
      fit <- lm(gains[, (ii + 1)] ~ gains[, 1])
      legend.entries[[ii]] <- bquote(.(tickers[ii + 1]): Y ==
                                     .(paste(sprintf("%.3f", fit$coef[1]),
                                             ifelse(fit$coef[2] > 0, " + ", " - "), sprintf("%.3f", abs(fit$coef[2])), "X", sep = "")) ~
                                       .("(") * R^2 == .(paste(sprintf("%.2f", summary(fit)$r.squared), ")", sep = "")))

    } else {
      fit <- lm(gains[, (ii + 1)] ~ poly(gains[, 1], orders[ii], raw = TRUE))
      if (orders[ii] == 2) {
        legend.entries[[ii]] <- bquote(.(tickers[ii + 1]): Y ==
                                       .(paste(sprintf("%.3f", fit$coef[1]),
                                               ifelse(fit$coef[2] > 0, " + ", " - "), sprintf("%.3f", abs(fit$coef[2])), "X",
                                               ifelse(fit$coef[3] > 0, " + ", " - "), sprintf("%.3f", abs(fit$coef[3])), sep = "")) * X^2 ~
                                         .("(") * R^2 == .(paste(sprintf("%.2f", summary(fit)$r.squared), ")", sep = "")))
      } else {
        legend.entries[[ii]] <- tickers[ii + 1]
      }
    }
    xy <- cbind(gains[, 1], predict(fit))
    xy <- xy[order(xy[, 1]), ]
    do.call(points, c(list(x = xy[, 1], y = xy[, 2], type = "l", col = colors[ii]),
                      points.list))
    lm.fits[[ii]] <- fit
  }

  # Add legend
  legend.list <- list.override(list1 = list(x = "topleft", lty = 1, col = colors, cex = 0.7,
                                            legend = sapply(legend.entries, as.expression)),
                               list2 = legend.list)
  do.call(legend, legend.list)

  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }

  # Return fitted models
  return(lm.fits)

}


onemetric.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                            gains = NULL, prices = NULL,
                            from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                            earliest.subset = FALSE,
                            y.metric = "cagr",
                            add.plot = FALSE,
                            sort.tickers = TRUE,
                            plot.list = NULL,
                            points.list = NULL,
                            axis.list = NULL,
                            pdf.list = NULL,
                            bmp.list = NULL,
                            jpeg.list = NULL,
                            png.list = NULL,
                            tiff.list = NULL) {

  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {

    # Obtain matrix of gains for each fund
    gains <- load.gains(tickers = tickers, from = from, to = to,
                        intercepts = intercepts, slopes = slopes,
                        time.scale = time.scale,
                        earliest.subset = earliest.subset)

  } else if (!is.null(prices)) {

    # Calculate gains based on price data
    gains <- apply(prices, 2, pchanges)

  } else if (is.null(gains)) {

    stop("You must specify one of the following inputs: tickers, gains, or prices")

  }

  # If y.metric requires a benchmark, split gains matrix into ticker gains and benchmark gains
  if (y.metric %in% c("alpha", "beta", "r.squared", "pearson", "spearman")) {
    benchmark.gains <- gains[, 1, drop = F]
    benchmark.ticker <- colnames(benchmark.gains)
    if (is.null(benchmark.ticker)) {
      benchmark.ticker <- "BENCH"
    }
    gains <- gains[, -1, drop = F]
  }

  # Set tickers to column names of gains matrix; if NULL, use Fund 1, Fund 2, ...
  tickers <- colnames(gains)
  if (is.null(tickers)) {
    tickers <- paste("Fund", 1: ncol(gains))
  }

  # Calculate performance metrics
  if (y.metric == "mean") {
    y <- apply(gains, 2, mean) * 100
    plot.title <- paste("Mean of ", capitalize(time.scale), " Gains", sep = "")
    y.label <- "Mean (%)"
  } else if (y.metric == "sd") {
    y <- apply(gains, 2, sd) * 100
    plot.title <- paste("SD of ", capitalize(time.scale), " Gains", sep = "")
    y.label <- "Standard deviation (%)"
  } else if (y.metric == "growth") {
    y <- apply(gains, 2, function(x) gains.rate(gains = x)) * 100
    plot.title <- "Total Growth"
    y.label <- "Growth (%)"
  } else if (y.metric == "cagr") {
    units.year <- ifelse(time.scale == "daily", 252, ifelse(time.scale == "monthly", 12, 1))
    y <- apply(gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    plot.title <- "Compound Annualized Growth Rate"
    y.label <- "CAGR (%)"
  } else if (y.metric == "mdd") {
    y <- apply(gains, 2, function(x) mdd(gains = x)) * 100
    plot.title <- "Maximum Drawdown"
    y.label <- "MDD (%)"
  } else if (y.metric == "sharpe") {
    y <- apply(gains, 2, function(x) sharpe.ratio(gains = x))
    plot.title <- "Sharpe Ratio"
    y.label <- "Sharpe ratio"
  } else if (y.metric == "sortino") {
    y <- apply(gains, 2, function(x) sortino.ratio(gains = x))
    plot.title <- "Sortino Ratio"
    y.label <- "Sortino ratio"
  } else if (y.metric == "alpha") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
    plot.title <- paste("Alpha w/ ", benchmark.ticker, sep = "")
    y.label <- "Alpha (%)"
  } else if (y.metric == "beta") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    plot.title <- paste("Beta w/ ", benchmark.ticker, sep = "")
    y.label <- "Beta"
  } else if (y.metric == "r.squared") {
    y <- apply(gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
    plot.title <- paste("R-squared w/ ", benchmark.ticker, sep = "")
    y.label <- "R-squared"
  } else if (y.metric == "pearson") {
    y <- apply(gains, 2, function(x) cor(x, benchmark.gains))
    plot.title <- paste("Pearson cor. w/ ", benchmark.ticker, sep = "")
    y.label <- "Pearson correlation"
  } else if (y.metric == "spearman") {
    y <- apply(gains, 2, function(x) cor(x, benchmark.gains, method = "spearman"))
    plot.title <- paste("Spearman cor. w/ ", benchmark.ticker, sep = "")
    y.label <- "Spearman correlation"
  } else if (y.metric == "auto.pearson") {
    y <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1]))
    plot.title <- "Autocorrelation"
    y.label <- paste("Pearson cor. for adjacent ", time.scale, " gains", sep = "")
  } else if (y.metric == "auto.spearman") {
    y <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
    plot.title <- "Autocorrelation"
    y.label <- paste("Spearman cor. for adjacent ", time.scale, " gains", sep = "")
  }

  # Sort tickers by y.metric, if requested
  if (sort.tickers) {
    order.funds <- order(y, decreasing = TRUE)
    tickers <- tickers[order.funds]
    y <- y[order.funds]
  }

  # Figure out features of graph, based on user inputs where available
  plot.list <- list.override(list1 = list(x = 1: length(tickers),
                                          y = y, type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xaxt = "n",
                                          xlab = "Fund", ylab = y.label),
                                          #xlim = c(0.5, length(tickers) + 0.5))
                             list2 = plot.list)
  points.list <- list.override(list1 = list(x = 1: length(tickers), y = y,
                                            cex = 1, pch = 16),
                               list2 = points.list)
  axis.list <- list.override(list1 = list(side = 1, at = 1: length(tickers), labels = tickers),
                             list2 = axis.list)

  # If pdf.list is not NULL, call pdf
  if (!is.null(pdf.list)) {
    if (is.null(pdf.list$file)) {
      pdf.list$file <- "figure1.pdf"
    }
    do.call(pdf, pdf.list)
  }

  # If bmp.list is not NULL, call bmp
  if (!is.null(bmp.list)) {
    if (is.null(bmp.list$file)) {
      bmp.list$file <- "figure1.bmp"
    }
    do.call(bmp, bmp.list)
  }

  # If jpeg.list is not NULL, call jpeg
  if (!is.null(jpeg.list)) {
    if (is.null(jpeg.list$file)) {
      jpeg.list$file <- "figure1.jpg"
    }
    do.call(jpeg, jpeg.list)
  }

  # If png.list is not NULL, call png
  if (!is.null(png.list)) {
    if (is.null(png.list$file)) {
      png.list$file <- "figure1.png"
    }
    do.call(png, png.list)
  }

  # If tiff.list is not NULL, call tiff
  if (!is.null(tiff.list)) {
    if (is.null(tiff.list$file)) {
      tiff.list$file <- "figure1.tif"
    }
    do.call(tiff, tiff.list)
  }

  # Create plot region
  if (! add.plot) {
    do.call(plot, plot.list)
  }

  # Add horizontal/vertical lines if useful for requested metrics
  if (y.metric %in% c("mean", "sharpe", "sortino", "alpha", "beta", "pearson",
                      "spearman", "auto.pearson", "auto.spearman", "growth", "cagr")) {
    abline(h = 0, lty = 2)
  } else if (y.metric == "r.squared") {
    abline(h = 1, lty = 2)
  }

  # Add points
  do.call(points, points.list)

  # Add fund labels
  do.call(axis, axis.list)

  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }

  # Return data frame containing tickers and metrics
  return(data.frame(ticker = tickers,
                    y.metric = y, 
                    row.names = NULL, stringsAsFactors = FALSE))

}


twometrics.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                             gains = NULL, prices = NULL,
                             from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                             earliest.subset = FALSE,
                             x.metric = "mdd", y.metric = "cagr",
                             tickerlabel.offsets = NULL,
                             add.plot = FALSE,
                             colors = NULL,
                             plot.list = NULL,
                             points.list = NULL,
                             text.list = NULL,
                             pdf.list = NULL,
                             bmp.list = NULL,
                             jpeg.list = NULL,
                             png.list = NULL,
                             tiff.list = NULL) {

  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {

    # Obtain matrix of gains for each fund
    gains <- load.gains(tickers = tickers, from = from, to = to,
                        intercepts = intercepts, slopes = slopes,
                        time.scale = time.scale,
                        earliest.subset = earliest.subset)

  } else if (!is.null(prices)) {

    # Calculate gains based on price data
    gains <- apply(prices, 2, pchanges)

  } else if (is.null(gains)) {

    stop("You must specify one of the following inputs: tickers, gains, or prices")

  }

  # If x.metric or y.metric requires one or two benchmarks, split gains matrix into ticker gains and benchmark gains
  if (x.metric %in% c("alpha", "beta", "r.squared", "pearson", "spearman") |
      y.metric %in% c("alpha", "beta", "r.squared", "pearson", "spearman")) {
    benchmark.gains <- gains[, 1, drop = F]
    benchmark.ticker <- colnames(benchmark.gains)
    if (is.null(benchmark.ticker)) {
      benchmark.ticker <- "BENCH"
    }
    gains <- gains[, -1, drop = F]
  }
  if (x.metric %in% c("alpha2", "beta2", "r.squared2", "pearson2", "spearman2") |
      y.metric %in% c("alpha2", "beta2", "r.squared2", "pearson2", "spearman2")) {
    benchmark2.gains <- gains[, 1, drop = F]
    benchmark2.ticker <- colnames(benchmark2.gains)
    if (is.null(benchmark2.ticker)) {
      benchmark2.ticker <- "BENCH 2"
    }
    gains <- gains[, -1, drop = F]
  }

  # Set tickers to column names of gains matrix; if NULL, use Fund 1, Fund 2, ...
  tickers <- colnames(gains)
  if (is.null(tickers)) {
    tickers <- paste("Fund", 1: ncol(gains))
  }

  # Calculate performance metrics
  x1 <- x2 <- y1 <- y2 <- NULL
  if (y.metric == "mean") {
    y <- apply(gains, 2, mean) * 100
    plot.title <- paste("Mean of ", capitalize(time.scale), " Gains vs. ", sep = "")
    y.label <- "Mean (%)"
  } else if (y.metric == "sd") {
    y <- apply(gains, 2, sd) * 100
    plot.title <- paste("SD of ", capitalize(time.scale), " Gains vs. ", sep = "")
    y.label <- "Standard deviation (%)"
    y1 <- 0
  } else if (y.metric == "growth") {
    y <- apply(gains, 2, function(x) gains.rate(gains = x)) * 100
    plot.title <- "Total Growth vs. "
    y.label <- "Growth (%)"
  } else if (y.metric == "cagr") {
    units.year <- ifelse(time.scale == "daily", 252, ifelse(time.scale == "monthly", 12, 1))
    y <- apply(gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
    plot.title <- "CAGR vs. "
    y.label <- "CAGR (%)"
  } else if (y.metric == "mdd") {
    y <- apply(gains, 2, function(x) mdd(gains = x)) * 100
    plot.title <- "Maximum Drawdown vs. "
    y.label <- "MDD (%)"
    y1 <- 0
  } else if (y.metric == "sharpe") {
    y <- apply(gains, 2, function(x) sharpe.ratio(gains = x))
    plot.title <- "Sharpe Ratio vs. "
    y.label <- "Sharpe ratio"
  } else if (y.metric == "sortino") {
    y <- apply(gains, 2, function(x) sortino.ratio(gains = x))
    plot.title <- "Sortino Ratio vs. "
    y.label <- "Sortino ratio"
  } else if (y.metric == "alpha") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark.ticker, " (%)", sep = "")
  } else if (y.metric == "alpha2") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[1] * 100)
    plot.title <- "Alpha vs. "
    y.label <- paste("Alpha w/ ", benchmark2.ticker, " (%)", sep = "")
  } else if (y.metric == "beta") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark.ticker, sep = "")
  } else if (y.metric == "beta2") {
    y <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[2])
    plot.title <- "Beta vs. "
    y.label <- paste("Beta w/ ", benchmark2.ticker, sep = "")
  } else if (y.metric == "r.squared") {
    y <- apply(gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/ ", benchmark.ticker, sep = "")
    y1 <- 0
  } else if (y.metric == "r.squared2") {
    y <- apply(gains, 2, function(x) summary(lm(x ~ benchmark2.gains))$r.squared)
    plot.title <- "R-squared vs. "
    y.label <- paste("R-squared w/ ", benchmark2.ticker, sep = "")
    y1 <- 0
  } else if (y.metric == "pearson") {
    y <- apply(gains, 2, function(x) cor(x, benchmark.gains))
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark.ticker, sep = "")
    y1 <- -1.05
    y2 <- 1.05
  } else if (y.metric == "pearson2") {
    y <- apply(gains, 2, function(x) cor(x, benchmark2.gains))
    plot.title <- "Pearson Cor. vs. "
    y.label <- paste("Pearson cor. w/ ", benchmark2.ticker, sep = "")
    y1 <- -1.05
    y2 <- 1.05
  } else if (y.metric == "spearman") {
    y <- apply(gains, 2, function(x) cor(x, benchmark.gains, method = "spearman"))
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark.ticker, sep = "")
    y1 <- -1.05
    y2 <- 1.05
  } else if (y.metric == "spearman2") {
    y <- apply(gains, 2, function(x) cor(x, benchmark2.gains, method = "spearman"))
    plot.title <- "Spearman Cor. vs. "
    y.label <- paste("Spearman cor. w/ ", benchmark2.ticker, sep = "")
    y1 <- -1.05
    y2 <- 1.05
  } else if (y.metric == "auto.pearson") {
    y <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1]))
    plot.title <- "Autocorrelation vs. "
    y.label <- "Pearson autocorrelation"
  } else if (y.metric == "auto.spearman") {
    y <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
    plot.title <- "Autocorrelation vs. "
    y.label <- "Spearman autocorrelation"
  }

  if (x.metric == "mean") {
    x <- apply(gains, 2, mean) * 100
    plot.title <- paste(plot.title, "Mean of ", capitalize(time.scale), " Gains", sep = "")
    x.label <- "Mean (%)"
  } else if (x.metric == "sd") {
    x <- apply(gains, 2, sd) * 100
    plot.title <- paste(plot.title, "SD of ", capitalize(time.scale), " Gains", sep = "")
    x.label <- "Standard deviation (%)"
    x1 <- 0
  } else if (x.metric == "growth") {
    x <- apply(gains, 2, function(x) gains.rate(gains = x) * 100)
    plot.title <- paste(plot.title, "Total Growth", sep = "")
    x.label <- "CAGR (%)"
  } else if (x.metric == "cagr") {
    units.year <- ifelse(time.scale == "daily", 252, ifelse(time.scale == "monthly", 12, 1))
    x <- apply(gains, 2, function(x) gains.rate(gains = x, units.rate = units.year) * 100)
    plot.title <- paste(plot.title, "CAGR", sep = "")
    x.label <- "CAGR (%)"
  } else if (x.metric == "mdd") {
    x <- apply(gains, 2, function(x) mdd(gains = x)) * 100
    plot.title <- paste(plot.title, "MDD", sep = "")
    x.label <- "MDD (%)"
    x1 <- 0
  } else if (x.metric == "sharpe") {
    x <- apply(gains, 2, function(x) sharpe.ratio(gains = x))
    plot.title <- paste(plot.title, "Sharpe Ratio", sep = "")
    x.label <- "Sharpe ratio"
  } else if (x.metric == "sortino") {
    x <- apply(gains, 2, function(x) sortino.ratio(gains = x))
    plot.title <- paste(plot.title, "Sortino Ratio", sep = "")
    x.label <- "Sortino ratio"
  } else if (x.metric == "alpha") {
    x <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
    plot.title <- paste(plot.title, "Alpha", sep = "")
    x.label <- paste("Alpha w/ ", benchmark.ticker, " (%)", sep = "")
  } else if (x.metric == "alpha2") {
    x <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[1] * 100)
    plot.title <- paste(plot.title, "Alpha", sep = "")
    x.label <- paste("Alpha w/ ", benchmark2.ticker, " (%)", sep = "")
  } else if (x.metric == "beta") {
    x <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
    plot.title <- paste(plot.title, "Beta", sep = "")
    x.label <- paste("Beta w/ ", benchmark.ticker, sep = "")
  } else if (x.metric == "beta2") {
    x <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[2])
    plot.title <- paste(plot.title, "Beta", sep = "")
    x.label <- paste("Beta w/ ", benchmark2.ticker, sep = "")
  } else if (x.metric == "r.squared") {
    x <- apply(gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark.ticker, sep = "")
    x1 <- 0
  } else if (x.metric == "r.squared2") {
    x <- apply(gains, 2, function(x) summary(lm(x ~ benchmark2.gains))$r.squared)
    plot.title <- paste(plot.title, "R-squared", sep = "")
    x.label <- paste("R-squared w/ ", benchmark2.ticker, sep = "")
    x1 <- 0
  } else if (x.metric == "pearson") {
    x <- apply(gains, 2, function(x) cor(x, benchmark.gains))
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark.ticker, sep = "")
    x1 <- -1.05
    x2 <- 1.05
  } else if (x.metric == "pearson2") {
    x <- apply(gains, 2, function(x) cor(x, benchmark2.gains))
    plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
    x.label <- paste("Pearson cor. w/ ", benchmark2.ticker, sep = "")
    x1 <- -1.05
    x2 <- 1.05
  } else if (x.metric == "spearman") {
    x <- apply(gains, 2, function(x) cor(x, benchmark.gains, method = "spearman"))
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark.ticker, sep = "")
    x1 <- -1.05
    x2 <- 1.05
  } else if (x.metric == "spearman2") {
    x <- apply(gains, 2, function(x) cor(x, benchmark2.gains, method = "spearman"))
    plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
    x.label <- paste("Spearman cor. w/ ", benchmark2.ticker, sep = "")
    x1 <- -1.05
    x2 <- 1.05
  } else if (x.metric == "auto.pearson") {
    x <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1]))
    plot.title <- paste(plot.title, "Autocorrelation", sep = "")
    x.label <- "Pearson autocorrelation"
  } else if (x.metric == "auto.spearman") {
    x <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
    plot.title <- paste(plot.title, "Autocorrelation", sep = "")
    x.label <- "Spearman autocorrelation"
  }

  # If NULL, set appropriate values for xlim and ylim ranges
  if (is.null(x1) | is.null(x2) | is.null(y1) | is.null(y2)) {
    x.range <- range(x)
    y.range <- range(y)
    if (is.null(x1)) {
      x1 <- x.range[1] - 0.05 * diff(x.range)
    }
    if (is.null(x2)) {
      x2 <- x.range[2] + 0.05 * diff(x.range)
    }
    if (is.null(y1)) {
      y1 <- y.range[1] - 0.05 * diff(y.range)
    }
    if (is.null(y2)) {
      y2 <- y.range[2] + 0.05 * diff(y.range)
    }
  }

  # Create color scheme for plot
  n.funds <- length(tickers)
  if (is.null(colors)) {
    if (n.funds == 1) {
      colors <- "black"
    } else if (n.funds == 2) {
      colors <- c("blue", "red")
    } else if (n.funds == 3) {
      colors <- c("blue", "red", "orange")
    } else if (n.funds == 4) {
      colors <- c("blue", "red", "orange", "purple")
    } else if (n.funds > 4) {
      #colors <- distinctColorPalette(n.funds)
      colors <- colorRampPalette(c("blue", "red", "darkgreen"))(n.funds)
    }
  }

  # Figure out features of graph, based on user inputs where available
  plot.list <- list.override(list1 = list(x = x,
                                          y = y, type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xlab = x.label, ylab = y.label,
                                          xlim = c(x1, x2), ylim = c(y1, y2)),
                             list2 = plot.list)
  points.list <- list.override(list1 = list(x = x, y = y,
                                            col = colors,
                                            cex = 1, pch = 16),
                               list2 = points.list)
  if (is.null(tickerlabel.offsets)) {
    tickerlabel.offsets.dat <- data.frame(ticker = tickers,
                                          x.offset = rep(0, n.funds),
                                          y.offset = rep((y2 - y1) / 40, n.funds),
                                          stringsAsFactors = FALSE)
  } else if (is.vector(tickerlabel.offsets) & length(tickerlabel.offsets) == 2) {
    tickerlabel.offsets.dat <- data.frame(ticker = tickers,
                                    x.offset = rep(tickerlabel.offsets[1], n.funds),
                                    y.offset = rep(tickerlabel.offsets[2], n.funds),
                                    stringsAsFactors = FALSE)
  } else if (is.matrix(tickerlabel.offsets)) {
    tickerlabel.offsets.dat <- data.frame(ticker = tickers,
                                          x.offset = tickerlabel.offsets[, 1],
                                          y.offset = tickerlabel.offsets[, 2],
                                          stringsAsFactors = FALSE)
  } else if (is.data.frame(tickerlabel.offsets) & nrow(tickerlabel.offsets) < n.funds) {
    tickerlabel.offsets.dat <- data.frame(ticker = tickers,
                                          x.offset = rep(0, n.funds),
                                          y.offset = rep((y2 - y1) / 40, n.funds),
                                          stringsAsFactors = FALSE)
    for (ii in 1: nrow(tickerlabel.offsets)) {
      loc <- which(tickerlabel.offsets.dat[, 1] == tickerlabel.offsets[ii, 1])
      tickerlabel.offsets.dat[loc, 2: 3] <- tickerlabel.offsets.dat[loc, 2: 3] + tickerlabel.offsets[ii, 2: 3]
    }
  }
  text.list <- list.override(list1 = list(x = x + tickerlabel.offsets.dat[, 2], y = y + tickerlabel.offsets.dat[, 3],
                                          labels = tickers,
                                          col = colors, cex = 0.7),
                             list2 = text.list)

  # If pdf.list is not NULL, call pdf
  if (!is.null(pdf.list)) {
    if (is.null(pdf.list$file)) {
      pdf.list$file <- "figure1.pdf"
    }
    do.call(pdf, pdf.list)
  }

  # If bmp.list is not NULL, call bmp
  if (!is.null(bmp.list)) {
    if (is.null(bmp.list$file)) {
      bmp.list$file <- "figure1.bmp"
    }
    do.call(bmp, bmp.list)
  }

  # If jpeg.list is not NULL, call jpeg
  if (!is.null(jpeg.list)) {
    if (is.null(jpeg.list$file)) {
      jpeg.list$file <- "figure1.jpg"
    }
    do.call(jpeg, jpeg.list)
  }

  # If png.list is not NULL, call png
  if (!is.null(png.list)) {
    if (is.null(png.list$file)) {
      png.list$file <- "figure1.png"
    }
    do.call(png, png.list)
  }

  # If tiff.list is not NULL, call tiff
  if (!is.null(tiff.list)) {
    if (is.null(tiff.list$file)) {
      tiff.list$file <- "figure1.tif"
    }
    do.call(tiff, tiff.list)
  }

  # Create plot region
  if (! add.plot) {
    do.call(plot, plot.list)
  }

  # Add horizontal/vertical lines if useful for requested metrics
  if (y.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2", "beta", "beta2", "pearson", "pearson2",
                      "spearman", "spearman2", "auto.pearson", "auto.spearman", "growth", "cagr")) {
    abline(h = 0, lty = 2)
  } else if (y.metric %in% c("r.squared", "r.squared2")) {
    abline(h = 1, lty = 2)
  }
  if (x.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2", "beta", "beta2", "pearson", "pearson2",
                      "spearman", "spearman2", "auto.pearson", "auto.spearman", "growth", "cagr")) {
    abline(v = 0, lty = 2)
  } else if (x.metric %in% c("r.squared", "r.squared2")) {
    abline(v = 1, lty = 2)
  }

  # Add points
  do.call(points, points.list)

  # Add fund labels
  do.call(text, text.list)

  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }

  # Return data frame containing tickers and metrics
  return(data.frame(ticker = tickers,
                    x.metric = x,
                    y.metric = y,
                    row.names = NULL, stringsAsFactors = FALSE))

}

# # Working on this one now
# tickers = NULL; intercepts = NULL; slopes = NULL;
# gains = NULL; prices = NULL;
# from = "1900-01-01"; to = Sys.Date(); time.scale = "daily";
# earliest.subset = FALSE;
# x.metric = "mdd"; y.metric = "cagr"; point.size = "sharpe";
# tickerlabel.offsets = NULL;
# add.plot = FALSE; 
# colors = NULL;
# plot.list = NULL;
# points.list = NULL;
# text.list = NULL;
# pdf.list = NULL;
# bmp.list = NULL;
# jpeg.list = NULL;
# png.list = NULL;
# tiff.list = NULL
# tickers <- c("VFINX", "VBLTX", "VWEHX")
# threemetrics.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
#                                gains = NULL, prices = NULL,
#                                from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
#                                earliest.subset = FALSE,
#                                x.metric = "mdd", y.metric = "cagr", size.metric = "sharpe",
#                                tickerlabel.offsets = NULL,
#                                add.plot = FALSE, 
#                                colors = NULL,
#                                plot.list = NULL,
#                                points.list = NULL,
#                                text.list = NULL,
#                                pdf.list = NULL,
#                                bmp.list = NULL,
#                                jpeg.list = NULL,
#                                png.list = NULL,
#                                tiff.list = NULL) {
#   
#   # If tickers specified, load various historical prices from Yahoo! Finance
#   if (!is.null(tickers)) {
#     
#     # Obtain matrix of gains for each fund
#     gains <- load.gains(tickers = tickers, from = from, to = to,
#                         intercepts = intercepts, slopes = slopes, 
#                         time.scale = time.scale,
#                         earliest.subset = earliest.subset)
#     
#   } else if (!is.null(prices)) {
#     
#     # Calculate gains based on price data
#     gains <- apply(prices, 2, pchanges)
#     
#   } else if (is.null(gains)) {
#     
#     stop("You must specify one of the following inputs: tickers, gains, or prices")
#     
#   }
#   
#   # If x.metric or y.metric requires one or two benchmarks, split gains matrix into ticker gains and benchmark gains
#   if (x.metric %in% c("alpha", "beta", "r.squared", "pearson", "spearman") |
#       y.metric %in% c("alpha", "beta", "r.squared", "pearson", "spearman")) {
#     benchmark.gains <- gains[, 1, drop = F]
#     benchmark.ticker <- colnames(benchmark.gains)
#     if (is.null(benchmark.ticker)) {
#       benchmark.ticker <- "BENCH"
#     }
#     gains <- gains[, -1, drop = F]
#   }
#   if (x.metric %in% c("alpha2", "beta2", "r.squared2", "pearson2", "spearman2") |
#       y.metric %in% c("alpha2", "beta2", "r.squared2", "pearson2", "spearman2")) {
#     benchmark2.gains <- gains[, 1, drop = F]
#     benchmark2.ticker <- colnames(benchmark2.gains)
#     if (is.null(benchmark2.ticker)) {
#       benchmark2.ticker <- "BENCH 2"
#     }
#     gains <- gains[, -1, drop = F]
#   }
#   
#   # Set tickers to column names of gains matrix; if NULL, use Fund 1, Fund 2, ...
#   tickers <- colnames(gains)
#   if (is.null(tickers)) {
#     tickers <- paste("Fund", 1: ncol(gains))
#   }
#   
#   # Calculate performance metrics
#   x1 <- x2 <- y1 <- y2 <- NULL
#   if (y.metric == "mean") {
#     y <- apply(gains, 2, mean) * 100
#     plot.title <- paste("Mean of ", capitalize(time.scale), " Gains vs. ", sep = "")
#     y.label <- "Mean (%)"
#   } else if (y.metric == "sd") {
#     y <- apply(gains, 2, sd) * 100
#     plot.title <- paste("SD of ", capitalize(time.scale), " Gains vs. ", sep = "")
#     y.label <- "Standard deviation (%)"
#     y1 <- 0
#   } else if (y.metric == "growth") {
#     y <- apply(gains, 2, function(x) gains.rate(gains = x)) * 100
#     plot.title <- "Total Growth vs. "
#     y.label <- "Growth (%)"
#   } else if (y.metric == "cagr") {
#     units.year <- ifelse(time.scale == "daily", 252, ifelse(time.scale == "monthly", 12, 1))
#     y <- apply(gains, 2, function(x) gains.rate(gains = x, units.rate = units.year)) * 100
#     plot.title <- "CAGR vs. "
#     y.label <- "CAGR (%)"
#   } else if (y.metric == "mdd") {
#     y <- apply(gains, 2, function(x) mdd(gains = x)) * 100
#     plot.title <- "Maximum Drawdown vs. "
#     y.label <- "MDD (%)"
#     y1 <- 0
#   } else if (y.metric == "sharpe") {
#     y <- apply(gains, 2, function(x) sharpe.ratio(gains = x))
#     plot.title <- "Sharpe Ratio vs. "
#     y.label <- "Sharpe ratio"
#   } else if (y.metric == "sortino") {
#     y <- apply(gains, 2, function(x) sortino.ratio(gains = x))
#     plot.title <- "Sortino Ratio vs. "
#     y.label <- "Sortino ratio"
#   } else if (y.metric == "alpha") {
#     y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
#     plot.title <- "Alpha vs. "
#     y.label <- paste("Alpha w/ ", benchmark.ticker, " (%)", sep = "")
#   } else if (y.metric == "alpha2") {
#     y <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[1] * 100)
#     plot.title <- "Alpha vs. "
#     y.label <- paste("Alpha w/ ", benchmark2.ticker, " (%)", sep = "")
#   } else if (y.metric == "beta") {
#     y <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
#     plot.title <- "Beta vs. "
#     y.label <- paste("Beta w/ ", benchmark.ticker, sep = "")
#   } else if (y.metric == "beta2") {
#     y <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[2])
#     plot.title <- "Beta vs. "
#     y.label <- paste("Beta w/ ", benchmark2.ticker, sep = "")
#   } else if (y.metric == "r.squared") {
#     y <- apply(gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
#     plot.title <- "R-squared vs. "
#     y.label <- paste("R-squared w/ ", benchmark.ticker, sep = "")
#     y1 <- 0
#   } else if (y.metric == "r.squared2") {
#     y <- apply(gains, 2, function(x) summary(lm(x ~ benchmark2.gains))$r.squared)
#     plot.title <- "R-squared vs. "
#     y.label <- paste("R-squared w/ ", benchmark2.ticker, sep = "")
#     y1 <- 0
#   } else if (y.metric == "pearson") {
#     y <- apply(gains, 2, function(x) cor(x, benchmark.gains))
#     plot.title <- "Pearson Cor. vs. "
#     y.label <- paste("Pearson cor. w/ ", benchmark.ticker, sep = "")
#     y1 <- -1.05
#     y2 <- 1.05
#   } else if (y.metric == "pearson2") {
#     y <- apply(gains, 2, function(x) cor(x, benchmark2.gains))
#     plot.title <- "Pearson Cor. vs. "
#     y.label <- paste("Pearson cor. w/ ", benchmark2.ticker, sep = "")
#     y1 <- -1.05
#     y2 <- 1.05
#   } else if (y.metric == "spearman") {
#     y <- apply(gains, 2, function(x) cor(x, benchmark.gains, method = "spearman"))
#     plot.title <- "Spearman Cor. vs. "
#     y.label <- paste("Spearman cor. w/ ", benchmark.ticker, sep = "")
#     y1 <- -1.05
#     y2 <- 1.05
#   } else if (y.metric == "spearman2") {
#     y <- apply(gains, 2, function(x) cor(x, benchmark2.gains, method = "spearman"))
#     plot.title <- "Spearman Cor. vs. "
#     y.label <- paste("Spearman cor. w/ ", benchmark2.ticker, sep = "")
#     y1 <- -1.05
#     y2 <- 1.05
#   } else if (y.metric == "auto.pearson") {
#     y <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1]))
#     plot.title <- "Autocorrelation vs. "
#     y.label <- "Pearson autocorrelation"
#   } else if (y.metric == "auto.spearman") {
#     y <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
#     plot.title <- "Autocorrelation vs. "
#     y.label <- "Spearman autocorrelation"
#   }
#   
#   if (x.metric == "mean") {
#     x <- apply(gains, 2, mean) * 100
#     plot.title <- paste(plot.title, "Mean of ", capitalize(time.scale), " Gains", sep = "")
#     x.label <- "Mean (%)"
#   } else if (x.metric == "sd") {
#     x <- apply(gains, 2, sd) * 100
#     plot.title <- paste(plot.title, "SD of ", capitalize(time.scale), " Gains", sep = "")
#     x.label <- "Standard deviation (%)"
#     x1 <- 0
#   } else if (x.metric == "growth") {
#     x <- apply(gains, 2, function(x) gains.rate(gains = x) * 100)
#     plot.title <- paste(plot.title, "Total Growth", sep = "")
#     x.label <- "CAGR (%)"
#   } else if (x.metric == "cagr") {
#     units.year <- ifelse(time.scale == "daily", 252, ifelse(time.scale == "monthly", 12, 1))
#     x <- apply(gains, 2, function(x) gains.rate(gains = x, units.rate = units.year) * 100)
#     plot.title <- paste(plot.title, "CAGR", sep = "")
#     x.label <- "CAGR (%)"
#   } else if (x.metric == "mdd") {
#     x <- apply(gains, 2, function(x) mdd(gains = x)) * 100
#     plot.title <- paste(plot.title, "MDD", sep = "")
#     x.label <- "MDD (%)"
#     x1 <- 0
#   } else if (x.metric == "sharpe") {
#     x <- apply(gains, 2, function(x) sharpe.ratio(gains = x))
#     plot.title <- paste(plot.title, "Sharpe Ratio", sep = "")
#     x.label <- "Sharpe ratio"
#   } else if (x.metric == "sortino") {
#     x <- apply(gains, 2, function(x) sortino.ratio(gains = x))
#     plot.title <- paste(plot.title, "Sortino Ratio", sep = "")
#     x.label <- "Sortino ratio"
#   } else if (x.metric == "alpha") {
#     x <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[1] * 100)
#     plot.title <- paste(plot.title, "Alpha", sep = "")
#     x.label <- paste("Alpha w/ ", benchmark.ticker, " (%)", sep = "")
#   } else if (x.metric == "alpha2") {
#     x <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[1] * 100)
#     plot.title <- paste(plot.title, "Alpha", sep = "")
#     x.label <- paste("Alpha w/ ", benchmark2.ticker, " (%)", sep = "")
#   } else if (x.metric == "beta") {
#     x <- apply(gains, 2, function(x) lm(x ~ benchmark.gains)$coef[2])
#     plot.title <- paste(plot.title, "Beta", sep = "")
#     x.label <- paste("Beta w/ ", benchmark.ticker, sep = "")
#   } else if (x.metric == "beta2") {
#     x <- apply(gains, 2, function(x) lm(x ~ benchmark2.gains)$coef[2])
#     plot.title <- paste(plot.title, "Beta", sep = "")
#     x.label <- paste("Beta w/ ", benchmark2.ticker, sep = "")
#   } else if (x.metric == "r.squared") {
#     x <- apply(gains, 2, function(x) summary(lm(x ~ benchmark.gains))$r.squared)
#     plot.title <- paste(plot.title, "R-squared", sep = "")
#     x.label <- paste("R-squared w/ ", benchmark.ticker, sep = "")
#     x1 <- 0
#   } else if (x.metric == "r.squared2") {
#     x <- apply(gains, 2, function(x) summary(lm(x ~ benchmark2.gains))$r.squared)
#     plot.title <- paste(plot.title, "R-squared", sep = "")
#     x.label <- paste("R-squared w/ ", benchmark2.ticker, sep = "")
#     x1 <- 0
#   } else if (x.metric == "pearson") {
#     x <- apply(gains, 2, function(x) cor(x, benchmark.gains))
#     plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
#     x.label <- paste("Pearson cor. w/ ", benchmark.ticker, sep = "")
#     x1 <- -1.05
#     x2 <- 1.05
#   } else if (x.metric == "pearson2") {
#     x <- apply(gains, 2, function(x) cor(x, benchmark2.gains))
#     plot.title <- paste(plot.title, "Pearson Cor.", sep = "")
#     x.label <- paste("Pearson cor. w/ ", benchmark2.ticker, sep = "")
#     x1 <- -1.05
#     x2 <- 1.05
#   } else if (x.metric == "spearman") {
#     x <- apply(gains, 2, function(x) cor(x, benchmark.gains, method = "spearman"))
#     plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
#     x.label <- paste("Spearman cor. w/ ", benchmark.ticker, sep = "")
#     x1 <- -1.05
#     x2 <- 1.05
#   } else if (x.metric == "spearman2") {
#     x <- apply(gains, 2, function(x) cor(x, benchmark2.gains, method = "spearman"))
#     plot.title <- paste(plot.title, "Spearman Cor.", sep = "")
#     x.label <- paste("Spearman cor. w/ ", benchmark2.ticker, sep = "")
#     x1 <- -1.05
#     x2 <- 1.05
#   } else if (x.metric == "auto.pearson") {
#     x <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1]))
#     plot.title <- paste(plot.title, "Autocorrelation", sep = "")
#     x.label <- "Pearson autocorrelation"
#   } else if (x.metric == "auto.spearman") {
#     x <- apply(gains, 2, function(x) cor(x[-length(x)], x[-1], method = "spearman"))
#     plot.title <- paste(plot.title, "Autocorrelation", sep = "")
#     x.label <- "Spearman autocorrelation"
#   }
#   
#   # If NULL, set appropriate values for xlim and ylim ranges
#   if (is.null(x1)) {
#     x1 <- min(x) * ifelse(min(x) > 0, 1.05, 0.95)
#   } 
#   if (is.null(x2)) {
#     x2 <- max(x) * ifelse(max(x) > 0, 1.05, 0.95)
#   }
#   if (is.null(y1)) {
#     y1 <- min(y) * ifelse(min(y) > 0, 1.05, 0.95)
#   }
#   if (is.null(y2)) {
#     y2 <- max(y) * ifelse(max(y) > 0, 1.05, 0.95)
#   }
#   
#   # Create color scheme for plot
#   n.funds <- length(tickers)
#   if (is.null(colors)) {
#     if (n.funds == 1) {
#       colors <- "black"
#     } else if (n.funds == 2) {
#       colors <- c("blue", "red")
#     } else if (n.funds == 3) {
#       colors <- c("blue", "red", "orange")
#     } else if (n.funds == 4) {
#       colors <- c("blue", "red", "orange", "purple")
#     } else if (n.funds > 4) {
#       #colors <- distinctColorPalette(n.funds)
#       colors <- colorRampPalette(c("blue", "red", "darkgreen"))(n.funds)
#     }
#   }
#   
#   # Figure out features of graph, based on user inputs where available
#   plot.list <- list.override(list1 = list(x = x, 
#                                           y = y, type = "n",
#                                           main = plot.title, cex.main = 1.25,
#                                           xlab = x.label, ylab = y.label,
#                                           xlim = c(x1, x2), ylim = c(y1, y2)),
#                              list2 = plot.list)
#   points.list <- list.override(list1 = list(x = x, y = y, 
#                                             col = colors,
#                                             cex = 1, pch = 16),
#                                list2 = points.list)
#   if (is.null(tickerlabel.offsets)) {
#     tickerlabel.offsets.dat <- data.frame(ticker = tickers,
#                                           x.offset = rep(0, n.funds),
#                                           y.offset = rep((y2 - y1) / 40, n.funds),
#                                           stringsAsFactors = FALSE)
#   } else if (is.vector(tickerlabel.offsets) & length(tickerlabel.offsets) == 2) {
#     tickerlabel.offsets.dat <- data.frame(ticker = tickers, 
#                                           x.offset = rep(tickerlabel.offsets[1], n.funds),
#                                           y.offset = rep(tickerlabel.offsets[2], n.funds), 
#                                           stringsAsFactors = FALSE)
#   } else if (is.matrix(tickerlabel.offsets)) {
#     tickerlabel.offsets.dat <- data.frame(ticker = tickers, 
#                                           x.offset = tickerlabel.offsets[, 1], 
#                                           y.offset = tickerlabel.offsets[, 2], 
#                                           stringsAsFactors = FALSE)
#   } else if (is.data.frame(tickerlabel.offsets) & nrow(tickerlabel.offsets) < n.funds) {
#     tickerlabel.offsets.dat <- data.frame(ticker = tickers, 
#                                           x.offset = rep(0, n.funds), 
#                                           y.offset = rep((y2 - y1) / 40, n.funds),
#                                           stringsAsFactors = FALSE)
#     for (ii in 1: nrow(tickerlabel.offsets)) {
#       loc <- which(tickerlabel.offsets.dat[, 1] == tickerlabel.offsets[ii, 1])
#       tickerlabel.offsets.dat[loc, 2: 3] <- tickerlabel.offsets.dat[loc, 2: 3] + tickerlabel.offsets[ii, 2: 3]
#     }
#   }
#   text.list <- list.override(list1 = list(x = x + tickerlabel.offsets.dat[, 2], y = y + tickerlabel.offsets.dat[, 3], 
#                                           labels = tickers,
#                                           col = colors, cex = 0.7),
#                              list2 = text.list)
#   
#   # If pdf.list is not NULL, call pdf
#   if (!is.null(pdf.list)) {
#     if (is.null(pdf.list$file)) {
#       pdf.list$file <- "figure1.pdf"
#     }
#     do.call(pdf, pdf.list)
#   }
#   
#   # If bmp.list is not NULL, call bmp
#   if (!is.null(bmp.list)) {
#     if (is.null(bmp.list$file)) {
#       bmp.list$file <- "figure1.bmp"
#     }
#     do.call(bmp, bmp.list)
#   }
#   
#   # If jpeg.list is not NULL, call jpeg
#   if (!is.null(jpeg.list)) {
#     if (is.null(jpeg.list$file)) {
#       jpeg.list$file <- "figure1.jpg"
#     }
#     do.call(jpeg, jpeg.list)
#   }
#   
#   # If png.list is not NULL, call png
#   if (!is.null(png.list)) {
#     if (is.null(png.list$file)) {
#       png.list$file <- "figure1.png"
#     }
#     do.call(png, png.list)
#   }
#   
#   # If tiff.list is not NULL, call tiff
#   if (!is.null(tiff.list)) {
#     if (is.null(tiff.list$file)) {
#       tiff.list$file <- "figure1.tif"
#     }
#     do.call(tiff, tiff.list)
#   }
#   
#   # Create plot region
#   if (! add.plot) {
#     do.call(plot, plot.list)
#   }
#   
#   # Add horizontal/vertical lines if useful for requested metrics
#   if (y.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2", "pearson", "pearson2", 
#                       "spearman", "spearman2", "auto.pearson", "auto.spearman", "growth", "cagr")) {
#     abline(h = 0, lty = 2)
#   } else if (y.metric %in% c("beta", "beta2", "r.squared", "r.squared2")) {
#     abline(h = 1, lty = 2)
#   }
#   if (x.metric %in% c("mean", "sd", "sharpe", "sortino", "alpha", "alpha2", "pearson", "pearson2", 
#                       "spearman", "spearman2", "auto.pearson", "auto.spearman", "growth", "cagr")) {
#     abline(v = 0, lty = 2)
#   } else if (x.metric %in% c("beta", "beta2", "r.squared", "r.squared2")) {
#     abline(v = 1, lty = 2)
#   }
#   
#   # Add points
#   do.call(points, points.list)
#   
#   # Add fund labels
#   do.call(text, text.list)
#   
#   # Close graphics device if necessary
#   if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
#       !is.null(png.list) | !is.null(tiff.list)) {
#     dev.off()
#   }
#   
#   # Return data frame containing tickers and metrics
#   return(data.frame(ticker = tickers, 
#                     x.metric = x, 
#                     y.metric = y, 
#                     row.names = NULL))
#   
# }
# 
# 

onemetric.overtime.graph <- function(tickers = NULL, intercepts = NULL, slopes = NULL,
                                     gains = NULL, prices = NULL,
                                     from = "1900-01-01", to = Sys.Date(), time.scale = "daily",
                                     earliest.subset = FALSE,
                                     y.metric = "cagr",
                                     window.units = 50,
                                     add.plot = FALSE,
                                     colors = NULL,
                                     plot.list = NULL,
                                     points.list = NULL,
                                     legend.list = NULL,
                                     pdf.list = NULL,
                                     bmp.list = NULL,
                                     jpeg.list = NULL,
                                     png.list = NULL,
                                     tiff.list = NULL) {

  # If tickers specified, load various historical prices from Yahoo! Finance
  if (!is.null(tickers)) {

    # Obtain matrix of gains for each fund
    gains <- load.gains(tickers = tickers, from = from, to = to,
                        intercepts = intercepts, slopes = slopes,
                        time.scale = time.scale,
                        earliest.subset = earliest.subset)

  } else if (!is.null(prices)) {

    # Calculate gains based on price data
    gains <- apply(prices, 2, pchanges)

  } else if (is.null(gains)) {

    stop("You must specify one of the following inputs: tickers, gains, or prices")

  }

  # If y.metric requires a benchmark, split gains matrix into ticker gains and benchmark gains
  if (y.metric %in% c("alpha", "beta", "r.squared", "pearson", "spearman")) {
    benchmark.gains <- gains[, 1, drop = F]
    benchmark.ticker <- colnames(benchmark.gains)
    if (is.null(benchmark.ticker)) {
      benchmark.ticker <- "BENCH"
    }
    gains <- gains[, -1, drop = F]
  }

  # Set tickers to column names of gains matrix; if NULL, use Fund 1, Fund 2, ...
  tickers <- colnames(gains)
  if (is.null(tickers)) {
    tickers <- paste("Fund", 1: ncol(gains))
  }

  # Get dates
  rows <- rownames(gains)[-c(1: (window.units - 1))]
  if (!is.null(rows)) {
    dates <- as.Date(rows)
  } else {
    dates <- 1: nrow(gains)
  }
  if (y.metric %in% c("auto.pearson", "auto.spearman")) {
    dates <- dates[-1]
  }

  # Calculate performance metrics
  y1 <- y2 <- NULL
  if (y.metric == "mean") {
    y <- rollapply(gains, width = window.units,
                   FUN = mean, by.column = TRUE) * 100
    plot.title <- paste("Mean of ", capitalize(time.scale), " Gains", sep = "")
    y.label <- "Mean (%)"
  } else if (y.metric == "sd") {
    y <- rollapply(gains, width = window.units,
                   FUN = sd, by.column = TRUE) * 100
    plot.title <- paste("SD of ", capitalize(time.scale), " Gains", sep = "")
    y.label <- "Standard deviation (%)"
    y1 <- 0
  } else if (y.metric == "growth") {
    y <- rollapply(gains, width = window.units,
                   FUN = function(x) gains.rate(gains = x) * 100, by.column = TRUE)
    plot.title <- "Total Growth"
    y.label <- "Growth (%)"
  } else if (y.metric == "cagr") {
    units.year <- ifelse(time.scale == "daily", 252, ifelse(time.scale == "monthly", 12, 1))
    y <- rollapply(gains, width = window.units,
                   FUN = function(x) gains.rate(gains = x, units.rate = units.year) * 100, by.column = TRUE)
    plot.title <- "Compound Annualized Growth Rate"
    y.label <- "CAGR (%)"
  } else if (y.metric == "mdd") {
    y <- rollapply(gains, width = window.units,
                   FUN = function(x) mdd(gains = x) * 100, by.column = TRUE)
    plot.title <- "Maximum Drawdown"
    y.label <- "MDD (%)"
    y1 <- 0
  } else if (y.metric == "sharpe") {
    y <- rollapply(gains, width = window.units,
                   FUN = sharpe.ratio, by.column = TRUE)
    plot.title <- "Sharpe Ratio"
    y.label <- "Sharpe ratio"
  } else if (y.metric == "sortino") {
    y <- rollapply(gains, width = window.units,
                   FUN = sortino.ratio, by.column = TRUE)
    plot.title <- "Sortino Ratio"
    y.label <- "Sortino ratio"
  } else if (y.metric == "alpha") {
    y <- matrix(NA, ncol = length(tickers), nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: ncol(gains)) {
        y[(ii - window.units + 1), jj] <- lm(gains[locs, jj] ~ benchmark.gains[locs])$coef[1] * 100
      }
    }
    plot.title <- paste("Alpha w/ ", benchmark.ticker, sep = "")
    y.label <- "Alpha (%)"
  } else if (y.metric == "beta") {
    y <- matrix(NA, ncol = length(tickers), nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: ncol(gains)) {
        y[(ii - window.units + 1), jj] <- lm(gains[locs, jj] ~ benchmark.gains[locs])$coef[2]
      }
    }
    plot.title <- paste("Beta w/ ", benchmark.ticker, sep = "")
    y.label <- "Beta"
  } else if (y.metric == "r.squared") {
    y <- matrix(NA, ncol = length(tickers), nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: ncol(gains)) {
        y[(ii - window.units + 1), jj] <- summary(lm(gains[locs, jj] ~ benchmark.gains[locs]))$r.squared
      }
    }
    plot.title <- paste("R-squared w/ ", benchmark.ticker, sep = "")
    y.label <- "R-squared"
    y1 <- 0
  } else if (y.metric == "pearson") {
    y <- matrix(NA, ncol = length(tickers), nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: ncol(gains)) {
        y[(ii - window.units + 1), jj] <- cor(gains[locs, jj], benchmark.gains[locs])
      }
    }
    plot.title <- paste("Pearson Cor. w/ ", benchmark.ticker, sep = "")
    y.label <- "Pearson correlation"
  } else if (y.metric == "spearman") {
    y <- matrix(NA, ncol = length(tickers), nrow = nrow(gains) - window.units + 1)
    for (ii in (window.units: nrow(gains))) {
      locs <- (ii - window.units + 1): ii
      for (jj in 1: ncol(gains)) {
        y[(ii - window.units + 1), jj] <- cor(gains[locs, jj], benchmark.gains[locs], method = "spearman")
      }
    }
    plot.title <- paste("Spearman Cor. w/ ", benchmark.ticker, sep = "")
    y.label <- "Spearman correlation"
  } else if (y.metric == "auto.pearson") {
    y <- rollapply(gains, width = window.units + 1,
                   FUN = function(x) cor(x[-length(x)], x[-1]), by.column = TRUE)
    plot.title <- "Autocorrelation"
    y.label <- paste("Pearson cor. for adjacent ", time.scale, " gains", sep = "")
  } else if (y.metric == "auto.spearman") {
    y <- rollapply(gains, width = window.units + 1,
                   FUN = function(x) cor(x[-length(x)], x[-1], method = "spearman"), by.column = TRUE)
    plot.title <- "Autocorrelation"
    y.label <- paste("Spearman cor. for adjacent ", time.scale, " gains", sep = "")
  }

  # If NULL, set appropriate values for ylim range
  if (is.null(y1)) {
    y1 <- min(y) * ifelse(min(y) > 0, 0.95, 1.05)
  }
  if (is.null(y2)) {
    y2 <- max(y) * ifelse(max(y) > 0, 1.05, 0.95)
  }

  # Create color scheme for plot
  n.funds <- length(tickers)
  if (is.null(colors)) {
    if (n.funds == 1) {
      colors <- "black"
    } else if (n.funds == 2) {
      colors <- c("blue", "red")
    } else if (n.funds == 3) {
      colors <- c("blue", "red", "orange")
    } else if (n.funds == 4) {
      colors <- c("blue", "red", "orange", "purple")
    } else if (n.funds > 4) {
      #colors <- distinctColorPalette(n.funds)
      colors <- colorRampPalette(c("blue", "red", "darkgreen"))(n.funds)
    }
  }

  # Figure out features of graph, based on user inputs where available
  plot.list <- list.override(list1 = list(x = dates,
                                          y = y[, 1], type = "n",
                                          main = plot.title, cex.main = 1.25,
                                          xlab = "Date", ylab = y.label,
                                          ylim = c(y1, y2)),
                             list2 = plot.list)
  points.list <- list.override(list1 = list(pch = 16),
                               list2 = points.list)
  legend.list <- list.override(list1 = list(x = "topleft", lty = 1, col = colors, legend = tickers),
                               list2 = legend.list)

  # If pdf.list is not NULL, call pdf
  if (!is.null(pdf.list)) {
    if (is.null(pdf.list$file)) {
      pdf.list$file <- "figure1.pdf"
    }
    do.call(pdf, pdf.list)
  }

  # If bmp.list is not NULL, call bmp
  if (!is.null(bmp.list)) {
    if (is.null(bmp.list$file)) {
      bmp.list$file <- "figure1.bmp"
    }
    do.call(bmp, bmp.list)
  }

  # If jpeg.list is not NULL, call jpeg
  if (!is.null(jpeg.list)) {
    if (is.null(jpeg.list$file)) {
      jpeg.list$file <- "figure1.jpg"
    }
    do.call(jpeg, jpeg.list)
  }

  # If png.list is not NULL, call png
  if (!is.null(png.list)) {
    if (is.null(png.list$file)) {
      png.list$file <- "figure1.png"
    }
    do.call(png, png.list)
  }

  # If tiff.list is not NULL, call tiff
  if (!is.null(tiff.list)) {
    if (is.null(tiff.list$file)) {
      tiff.list$file <- "figure1.tif"
    }
    do.call(tiff, tiff.list)
  }

  # Create plot region
  if (! add.plot) {
    do.call(plot, plot.list)
  }

  # Add horizontal/vertical lines if useful for requested metrics
  if (y.metric %in% c("mean", "sd", "growth", "cagr", "sharpe", "sortino", "alpha", "beta", "pearson",
                      "spearman", "auto.pearson", "auto.spearman")) {
    abline(h = 0, lty = 2)
  } else if (y.metric == "r.squared") {
    abline(h = 1, lty = 2)
  }

  # Add curves for each fund
  for (ii in 1: n.funds) {

    # Add colored curves and data points
    do.call(points, c(list(x = dates, y = y[, ii], type = "l", col = colors[ii]), points.list))

  }

  # Add legend
  if (length(tickers) > 1) {
    do.call(legend, legend.list)
  }

  # Close graphics device if necessary
  if (!is.null(pdf.list) | !is.null(bmp.list) | !is.null(jpeg.list) |
      !is.null(png.list) | !is.null(tiff.list)) {
    dev.off()
  }

  # Return matrix of y values
  return(y)

}

# PLANS: 

# 1. Make function that plots a perf. metric over time, for single or two fund portfolios. Maybe just give it gains for one or more funds/portfolios. Size or color of data points indicate time...
# 2. Make GIF version that shows graph over time. List of lists, etc.
# 3. write article on pairing Vanguard bond funds with S&P (and maybe UPRO). mean vs. sd and cagr vs. mdd.

# threemetrics.graph
# twometrics.overtime.graph if feasible!
# Trigger