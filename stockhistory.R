load_stock <- function(file) {
  # Check that the name ends with .csv
  if (!grepl("\\.csv$", file, ignore.case = TRUE)) {
    stop("Error: The file name must end with .csv")
  }
  
  # Read the CSV
  stocks <- read.csv(file, stringsAsFactors = FALSE)
  
  # Convert Date column (adjust format if needed)
  stocks$Date <- as.Date(stocks$Date, format = "%m/%d/%Y")
  
  # Rename columns (if the CSV has exactly these 7 columns)
  colnames(stocks) <- c("Date","Close","Open","High","Low","Volume","Change")
  
  return(stocks)
}
