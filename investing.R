source('./stockhistory.R')
source('wrangle.R')
stocks <- load_stock("./IBM.csv")
stocks <- process_stock("stocks")
head(stocks)

plot(stocks$Date, stocks$Close,
     type = "l", col = "black", lwd = 2,
     main = "stocks Bollinger Bands (Base R)",
     xlab = "Date", ylab = "Price")

lines(stocks$Date, stocks$SMA20, col = "blue", lwd = 2)        # Middle band
lines(stocks$Date, stocks$Upper, col = "red",  lwd = 2)        # Upper band
lines(stocks$Date, stocks$Lower, col = "red",  lwd = 2)        # Lower band

legend("topleft",
       legend = c("Close", "SMA20", "Upper Band", "Lower Band"),
       col = c("black", "blue", "red", "red"),
       lwd = 2,
       bty = "n")