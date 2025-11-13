## Stock Analysis With Base R

## Read Data: Enter  CSV filename  and names of date and close column

rm(list=ls())
stocks <- read.csv("./IBM.csv")
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

# Pull Last 100 Days Data
last_date <- max(stocks$Date, na.rm = TRUE)
df_last60 <- stocks[stocks$Date >= last_date - 100, ]



# 1. Plot the closing price
plot(df_last60$Close,
     type = "l",
     col = "black",
     lwd = 2,
     main = "Closing Price with 50-Day Moving Average",
     xlab = "Index",
     ylab = "Price")

# 2. Add the 20-day moving average line
lines(df_last60$SMA50,
      col = "red",
      lwd = 2)

# 3. Add a legend
legend("topleft",
       legend = c("Close", "50-Day MA"),
       col = c("black", "red"),
       lwd = 2,
       bty = "n")
grid()



## Daily and Cumulative Returns
### Daily

stocks$Daily <- (stocks$Close / c(NA, head(stocks$Close, -1))) - 1
stocks$Daily[1] <- 0
stocks$Cumulative <- cumprod(1 + stocks$Daily) - 1


### Plot Daily Returns
plot(stocks$Date,stocks$Daily,type='l',main="Daily Returns")


### Cumulative Returns Plot


plot(stocks$Date,stocks$Cumulative,type='l',main=" Cumulative Returns",xlab="Date",ylab="Return %")
grid()
