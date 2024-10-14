library(dplyr)
library(lubridate)
library(quantmod)
library(ggplot2)



source("Market Return Distribution and Probability.R")

# ===== WITH ANNUAL RE-BALANCE AND MULTIPLE DELTAS =====
delta_hedge_protection <- function(delta, breakeven, data, assetp_size, hedgep_size, p_rebalance) {
  # Takes 6 arguments
  # delta (δ): a vector containing integers of the profit for every additional 1% drop in the underlying after breakeven point [(underlying / 100) / premium ]
  # breakeven: an integer representing the market drop for the breakeven point of the put option.
  # data: a data set containing the returns of an asset
  # assetp_size: an integer representing the % of portfolio that the asset will be (ex: 97%)
  # hedgep_size: an integer representing the % of portfolio the hedge (put option) will be (ex: 3%)
  # r_rebalance: portfolio should be re-balanced every year. and
  
 # Create important variables
  hedge_cumulative_return <- 0
  asset_cumulative_return <- c() 
  portfolio_comparison <- data.frame()
  unhedged_portfolio <- 100
  portfolio <- 100
  period_count <- 1
  ASSET <- as.numeric(assetp_size) / 100  # in percentage
  HEDGE <- as.numeric(hedgep_size) / 100  # in percentage
  deltas_df <- data.frame()
  portfolios_df <- data.frame()
  asset_df <- data.frame()
  
  for (return in data) {
    # Convert return (in percentages) to arithmetic return
    areturn <- (return / 100) + 1
    
    # Compute hedge performance: 
    # where δ is the profit for every additional drop in the underlying after the breakeven point
    hedge_return <- (breakeven - return) * delta
    hedge_return <- sapply(hedge_return, function(x) { max(x, -1 ) })
    
    # Compute performances
    hedge_cumulative_return <- hedge_cumulative_return + (hedge_return * (hedgep_size/p_rebalance))  # Hedge return times actual hedge size (0.25%)
    asset_cumulative_return <- c(asset_cumulative_return, areturn)  # add monthly return to vector
    unhedged_portfolio <- unhedged_portfolio * areturn
    
    # Re-balance portfolio 
    if (period_count == p_rebalance) {
      # Compute asset annual performance and annual market return
      asset_annual_return <- prod(asset_cumulative_return) * assetp_size
      
      # Compute annual portfolio value
      portfolio <- asset_annual_return + hedge_cumulative_return  # portfolio
      
      deltas_df <- rbind(deltas_df, hedge_cumulative_return) 
      portfolios_df <- rbind(portfolios_df, portfolio)
      asset_df <- rbind(asset_df, portfolio - hedge_cumulative_return)
      portfolio_comparison <- rbind(portfolio_comparison, unhedged_portfolio)
      
      # Re-balance portfolio 
      assetp_size <- portfolio * ASSET  # asset as 97% of portfolio 
      hedgep_size <- portfolio * HEDGE  # Hedge as 3% of portfolio annually, or 0.25% each month 
      hedge_cumulative_return <- 0
      asset_cumulative_return <- c()
      
      # Re-set month count
      period_count <- 1
      
    } else {
      period_count <- period_count + 1
    }
  } 
  cat("Hedged Portfolio Return:", portfolio / 100 * 100, "%", "\n")
  cat("Unhedegd Portfolio Return:", prod((data / 100) + 1) * 100,  "%", "\n")
  
  portfolio_comparison <- cbind(portfolio_comparison, deltas_df)
  portfolio_comparison <- cbind(portfolio_comparison, asset_df)
  portfolio_comparison <- cbind(portfolio_comparison, portfolios_df)
  
  # Define columns names 
  names <- c("Unhedged_Portfolio", paste0("AssetD", delta), paste0("HedgeD", delta), paste0("PortfolioD", delta))
  colnames(portfolio_comparison) <- names 
  
  #colnames(portfolio_comparison) <- c("asset_return", "hedge_return", "portfolio_value", "unhedged_portfolio_value")
  return(portfolio_comparison)
}


portfolio_comparison <- delta_hedge_protection(
  delta = c(13:18), breakeven =-7, data = sp500_monthly$Return, assetp_size = 97,
  hedgep_size = 3, p_rebalance = 12)

portfolio_comparison$Date <- sp500_yearly$Date

# Reshape the data from wide to long format
delta_comparison <- portfolio_comparison[, c(1,14:ncol(portfolio_comparison))] %>%
  pivot_longer(cols = -Date, names_to = "Portfolio", values_to = "Return")

# Create the plot using the 'Portfolio' column for the legend
ggplot() +
  geom_line(data = delta_comparison, aes(x = Date, y = Return, color = Portfolio), linewidth = 1) +
  labs(title = "Portfolio Comparison", x = "Date", y = "Return") +
  scale_y_continuous(breaks = seq(100, 14000, by = 500), labels = function(x) paste0(x, "%")) +
  scale_x_date(breaks = "3 years") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_color_manual(values = c("green", "red", "blue", "pink", "purple", "orange", "black"))
  
    

