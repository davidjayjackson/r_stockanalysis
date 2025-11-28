process_stock <- function(dfname) {
  # Check that the data frame exists
  if (!exists(dfname, envir = .GlobalEnv)) {
    stop("Error: The data frame does not exist in the global environment.")
  }
  
  # Retrieve the data frame by name
  stocks <- get(dfname, envir = .GlobalEnv)
  
  # Calculate Rolling Means
  stocks$SMA7  <- stats::filter(stocks$Close, rep(1/7, 7), sides = 1)
  stocks$SMA20 <- stats::filter(stocks$Close, rep(1/20, 20), sides = 1)
  stocks$SMA50 <- stats::filter(stocks$Close, rep(1/50, 50), sides = 1)
  
  # High–Low Difference and Rolling Average
  stocks$highlo      <- stocks$High - stocks$Low
  stocks$hilorolling <- stats::filter(stocks$highlo, rep(1/20, 20), sides = 1)
  
  # Daily and Cumulative Returns
  stocks$Daily <- (stocks$Close / c(NA, head(stocks$Close, -1))) - 1
  stocks$Daily[1] <- 0
  stocks$Cumulative <- cumprod(1 + stocks$Daily) - 1
  
  # Bollinger Bands
  stocks$SD20 <- sqrt(
    stats::filter((stocks$Close - stocks$SMA20)^2,
                  rep(1/20, 20), sides = 1)
  )
  stocks$Upper <- stocks$SMA20 + 2 * stocks$SD20
  stocks$Lower <- stocks$SMA20 - 2 * stocks$SD20
  
  # RSI Calculations
  stocks$Change <- c(NA, diff(stocks$Close))
  stocks$Gain   <- ifelse(stocks$Change > 0,  stocks$Change, 0)
  stocks$Loss   <- ifelse(stocks$Change < 0, -stocks$Change, 0)
  
  n <- 14
  stocks$AvgGain <- stats::filter(stocks$Gain, rep(1/n, n), sides = 1)
  stocks$AvgLoss <- stats::filter(stocks$Loss, rep(1/n, n), sides = 1)
  
  stocks$RS  <- stocks$AvgGain / stocks$AvgLoss
  stocks$RSI <- 100 - (100 / (1 + stocks$RS))
  
  # Optional: Last 100 days (not returned unless you want it)
  last_date <- max(stocks$Date, na.rm = TRUE)
  df_last100 <- stocks[stocks$Date >= last_date - 100, ]
  
  # FIXED: return the processed data frame
  return(stocks)
}

