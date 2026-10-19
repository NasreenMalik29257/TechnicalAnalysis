library(quantmod)

import_portfolio_data <- function(file_path = "portfolio.txt") {
  tickers <- trimws(readLines(file_path, warn = FALSE))
  tickers <- unique(tickers[nzchar(tickers)])
  
  market_data <- list()
  
  for (ticker in tickers) {
    cat("Downloading data for:", ticker, "\n")
    
    stock <- tryCatch(
      getSymbols(ticker,
                 src = "yahoo",
                 from = "2025-01-01",
                 auto.assign = FALSE),
      error = function(e) {
        message("Unable to download ", ticker, ": ",
                conditionMessage(e))
        NULL
      }
    )
    
    if (!is.null(stock)) {
      market_data[[ticker]] <- data.frame(
        Date = as.Date(index(stock)),
        Closing_Price = as.numeric(Cl(stock))
      )
      cat("Imported:", ticker, "\n")
    }
  }
  
  return(market_data)
}

nasreen_market_data <- import_portfolio_data()

print(lapply(nasreen_market_data, head))

summarize_stock_prices <- function(stock_df) {
  prices <- na.omit(stock_df$Closing_Price)
  
  price_frequency <- table(prices)
  most_common_price <- as.numeric(
    names(price_frequency)[which.max(price_frequency)]
  )
  
  ma30 <- TTR::SMA(prices, n = 30)
  
  data.frame(
    Average_Price = mean(prices),
    Median_Price = median(prices),
    Mode_Price = most_common_price,
    Price_StdDev = sd(prices),
    Latest_MA30 = tail(na.omit(ma30), 1)
  )
}

nasreen_statistics <- lapply(
  nasreen_market_data,
  summarize_stock_prices
)

print(nasreen_statistics)

library(ggplot2)

# Visualize stock prices and 30-day trends
for (ticker in names(nasreen_market_data)) {
  
  price_data <- nasreen_market_data[[ticker]]
  
  price_data$MA30 <- TTR::SMA(
    price_data$Closing_Price, n = 30
  )
  
  stock_plot <- ggplot(price_data, aes(x = Date)) +
    geom_line(
      aes(y = Closing_Price, color = "Daily Closing Price"),
      linewidth = 0.6
    ) +
    geom_line(
      aes(y = MA30, color = "30-Day Moving Average"),
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    labs(
      title = paste("Market Trend Analysis:", ticker),
      subtitle = "Historical closing prices and 30-day trend",
      x = "Trading Date",
      y = "Closing Price (USD)",
      color = "Price Indicator"
    ) +
    theme_classic()
  
  print(stock_plot)
  
  ggsave(
    filename = paste0(ticker, "_market_trend.png"),
    plot = stock_plot,
    width = 10,
    height = 6,
    dpi = 300
  )
}