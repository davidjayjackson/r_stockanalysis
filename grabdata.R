## Stock Analysis With Base R

## Read Data: Enter  CSV filename  and names of date and close column
 grabdata <- function(file){
stocks <- read.csv("file")
# Rename columns
colnames(stocks) <- c("Date","Close","Open","High","Low", "Volume","Change")
stocks$Date <- as.Date(stocks$Date,format="%m/%d/%Y")

# ---
  
# Calculate Rolling Means
# 7 Days
stocks$SMA7 <- stats::filter(stocks$Close,
                             rep(1/7, 7),
                             sides = 1)

# 20 Days
stocks$SMA20<- stats::filter(stocks$Close, rep(1/20, 20), sides = 1)

# 50 Days
stocks$SMA50 <- stats::filter(stocks$Close,
                              rep(1/50, 50),
                              sides = 1)

# Calculate difference between high and low and calculate 20 moving average.
stocks$highlo <- stocks$High - stocks$Low
stocks$hilorolling <- stats::filter(stocks$highlo, rep(1/20, 20), sides = 1)

## Daily and Cumulative Returns
stocks$Daily <- (stocks$Close / c(NA, head(stocks$Close, -1))) - 1
stocks$Daily[1] <- 0
stocks$Cumulative <- cumprod(1 + stocks$Daily) - 1


# Bollinger Bands

# --- 1. Compute the 20-day SMA ---
# stocks$SMA20 <- stats::filter(stocks$Close, rep(1/20, 20), sides = 1)

# --- 2. Compute the 20-day rolling standard deviation (base R) ---
stocks$SD20 <- sqrt(
  stats::filter((stocks$Close - stocks$SMA20)^2,
                rep(1/20, 20),
                sides = 1)
)

# --- 3. Upper & Lower Bollinger Bands ---
stocks$Upper <- stocks$SMA20 + 2 * stocks$SD20
stocks$Lower <- stocks$SMA20 - 2 * stocks$SD20

# RSI Plot Calculations


# --- 1. Calculate price changes ---
stocks$Change <- c(NA, diff(stocks$Close))

# --- 2. Separate gains and losses ---
stocks$Gain <- ifelse(stocks$Change > 0, stocks$Change, 0)
stocks$Loss <- ifelse(stocks$Change < 0, -stocks$Change, 0)

# --- 3. Compute 14-period averages (base R rolling mean) ---
n <- 14

stocks$AvgGain <- stats::filter(stocks$Gain, rep(1/n, n), sides = 1)
stocks$AvgLoss <- stats::filter(stocks$Loss, rep(1/n, n), sides = 1)

# --- 4. Compute RS and RSI ---
stocks$RS  <- stocks$AvgGain / stocks$AvgLoss
stocks$RSI <- 100 - (100 / (1 + stocks$RS))



# Pull Last 100 Days Data
last_date <- max(stocks$Date, na.rm = TRUE)
df_last60 <- stocks[stocks$Date >= last_date - 100, ]

}
