rm(list=ls())
stocks <- read.csv("./Ford.csv")
stocks$Date <- as.Date(stocks$Date,format="%m/%d/%Y")
# Rename columns
colnames(stocks) <- c("Date","Close","Open","High","Low", "Volume","Change")

# Calculate Rolling Means



# 5 Days
stocks$SMA5 <- stats::filter(stocks$Close,
                             rep(1/5, 5),sides = 1)
# 7 Days
stocks$SMA7 <- stats::filter(stocks$Close,
                             rep(1/7, 7),sides = 1)
# 9 Days
stocks$SMA9 <- stats::filter(stocks$Close,
                             rep(1/9, 9),sides = 1)

# 20 Days
stocks$SMA20 <- stats::filter(stocks$Close, rep(1/20, 20), sides = 1)

# 50 Days
stocks$SMA50 <- stats::filter(stocks$Close,
                              rep(1/50, 50),
                              sides = 1)

# 100 Days
stocks$SMA100 <- stats::filter(stocks$Close,
                              rep(1/100, 100),
                              sides = 1)

# 200 Days
stocks$SMA200 <- stats::filter(stocks$Close,
                              rep(1/200, 200),
                              sides = 1)
# Plot close and Volume
quarter <- subset(stocks,Date >='2025-09-01')
# Two-panel layout
par(mfrow = c(2,1), mar = c(3,4,2,2))

# --- Top Panel: Closing Price Line ---
plot(
  quarter$Date,
  quarter$Close,
  type = "l",
  col = "black",
  lwd = 2,
  main = "Closing Price",
  xlab = "",
  ylab = "Price"
)
lines(quarter$Date,quarter$SMA9,col="red")
lines(quarter$Date,quarter$SMA20,col="green")
grid()

# --- Bottom Panel: Volume Bars ---
barplot(
  quarter$Volume,
  names.arg = quarter$Date,
  col = "red",
  border = NA,
  main = "Volume",
  xlab = "Date",
  ylab = "Volume",
  axes = TRUE
)
grid()