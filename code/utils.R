epi_trend <- function(x, smooth = TRUE, smooth_window = 6, trend_window = smooth_window,
                      threshold = 0.03) {
  if (smooth) {
    x <- zoo::rollapply(x,
                        width = smooth_window,
                        FUN = mean,
                        na.rm = TRUE,
                        align = 'center',
                        fill = NA,
                        partial = TRUE)
  }

  out <- trend(x,
               n = trend_window,
               thresh = threshold)

  out <- switch(out,
                decreasing = 'Baisse',
                increasing = 'Hausse',
                stable = 'Stable',
                Unknown = 'Inconnu')

  return(out)
}


confirm_outbreak <- function(region_zone, start, end, lab) {
  confirmations <- lab %>%
                     filter.(.data$reg_zone == region_zone &
                             .data$date >= start - (7 * 4) &
                             .data$date <= end) %>%
                     summarize.(measles = .sum(measles == 'positif'),
                                .by = date)
  confirmed <- any(confirmations$measles > 1)
  return(confirmed)
}

confirm_alert <- function(df, region_zone) {
  tmp <- df %>%
           filter.(reg_zone == region_zone) %>%
           mutate.(date = as.Date(date))

  start_index <- purrr::detect_index(tmp$alert,
                                     function(x) !x,
                                     .dir = 'backward')

  confirmed <- any(tmp$measles_4w[start_index:nrow(tmp)] > 1)
  return(confirmed)
}

score_prevent <- function(vax, year_last, neighborbreaks) {
  rules <- list()

  rules$vax1 <-  vax < 0.70
  rules$vax2 <- vax < 0.50
  rules$last <- (year_last < 2019) * 2
  rules$neigbhor1 <- neighborbreaks >= 1
  rules$neightbor2 <- neighborbreaks >= 2

  score <- sum(unlist(rules), na.rm = TRUE)
  score <- as.integer(score)

  return(score)
}

#score_riposte <- function(measles, tests, 



trend <- function(x, n = 6, mode = 'exponential', thresh, max_na = 3, abs_chg = 5) {
  if (length(x) < n) {
    return("Unknown")
  }
  
  x <- x[(length(x)-n+1):length(x)] #select last n
  y <- seq_along(x)
  
  if (sum(is.na(x)) > max_na) {
    return("Unknown") #maximal number of na within the n
  }
  
  #limit on absolute change
  if (abs(min(x, na.rm = TRUE) - max(x, na.rm = TRUE)) < abs_chg) {
    return("stable")
  }
  
  if (mode == "exponential") {
    return(trend_exp(x, y, thresh))
  } else if (mode == "linear") {
    return(trend_lin(x, y, thresh))
  } else {
    stop("mode unknown")
  }
}


trend_lin <- function(x,y,thresh) {
  l <- lm(x~y)
  pente <- l$coefficients[["y"]]  
  
  if (is.na(pente)) {
    return("unknown") 
  } else if ((pente > thresh)) {
    return("increasing")
  } else if ((pente < -thresh)) {
    return("decreasing")
  } else {
    return("stable")
  }
}


trend_exp <- function(x,y,thresh) {
  
  if (all(x == 0)) {
    return("stable")
  }
  
  growth_rate <- get_exp_growth_rate(y, x)

  if (is.na(growth_rate)) {
    return("unknown")
  } else if ((growth_rate > thresh)) {
    return("increasing")
  } else if ((growth_rate < -thresh)) {
    return("decreasing")
  } else {
    return("stable")
  }
}



doubling_time <- function(growth_rate) {

  res <- log(2)/growth_rate
  
  return(res)
}

doubling_time <- Vectorize(doubling_time)

rep_number_normal_si <- function(growth_rate, si_mn, si_sd) {
  #from Wallinga & Lipsitch doi 10.1098/rspb.2006.3754
  #This only applies to normally distributed serial interval!
  #other distributions are available in the paper.
  R <- exp(growth_rate*si_mn - 0.5*(growth_rate*si_sd)^2)
  return(R)
}

rep_number_normal_si <- Vectorize(rep_number_normal_si)


get_exp_growth_rate <- function(time, cases, min_data = 3, ci = FALSE) {

  if (!is.numeric(time)) {
    stop("time must be numeric")
  }
  
  data <- data.frame(x = time, y = cases)
  data <- dplyr::filter(data, !is.na(y), y > 0)
  
  if (nrow(data) < min_data) {
    if (ci) {
      res <- c(m = NA_real_, low = NA_real_, high = NA_real_)
    } else {
      res <- NA_real_
    }
    return(res)
  }
  
  model <- lm(log(y) ~ x, data=data)
  growth_rate <- coef(model)[2]
  
  res <- growth_rate[[1]]

  return(res)
}

get_exp_growth_rate_simp <- function(x) {
  return(get_exp_growth_rate(1:length(x), x))
}

get_abs_chg <- function(x) {
  return(abs(min(x, na.rm = TRUE) - max(x, na.rm = TRUE)))
}
  
  
