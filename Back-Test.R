library(dplyr)
library(tidyr)
library(lubridate)
library(quantmod)
library(ggplot2)



#source("Market Return Distribution and Probability.R")

# ===== DEFINE IMPORTANT FUNCTION TO BACKTEST =====
# Back test with options
delta_hedge_protection <- function(delta, breakeven, data, assetp_size, hedgep_size, p_rebalance, full = TRUE) {
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
  
  
 
  if (full == TRUE) { 
    portfolio_comparison <- cbind(portfolio_comparison, deltas_df)
    portfolio_comparison <- cbind(portfolio_comparison, asset_df)
    portfolio_comparison <- cbind(portfolio_comparison, portfolios_df)
    # Define columns names 
    names <- c("Unhedged_Portfolio", paste0("AssetD", delta), paste0("HedgeD", delta), paste0("PortfolioD", delta))
    colnames(portfolio_comparison) <- names 
 
    return(portfolio_comparison)
  } else {
      colnames(portfolios_df) <- "Portfolio"
      return(portfolios_df)
    }
}


# Back test for traditional 60/40 portfolio
balanced_portfolio <- function(data, HEDGE, ASSET, yield, full = TRUE) {
  # Takes 4 parameters
  # data: a (column) data frame or vector containing the returns for the asset
  # hedge: an integer representing the percentage size of hedge (bonds)
  # asset: an integer representing the percentage size of the asset
  # yield: the yield of hedge (bond)
  # full: defines how the output. When TRUE, returns a full data frame with each component
  # When FALSE, returns just the portfolio performance
  
  # Define important vectors and variables
  month_count <- 1
  asset_vec <- c()
  hedge_vec <- c()
  portfolio_vec <- c()
  asset <- ASSET
  hedge <- HEDGE
  yield <- (yield / 100) + 1  # convert to arithmetic return format
  
  # Iterate trough each return
  for (return in data) {
    # Convert return to arithmetic return
    areturn <- (return / 100) + 1
      
    # Compute return for asset
    asset <- asset * areturn
    
    # Annual portfolio re-balance
    if (month_count == 12) {
      
      # Compute bonds and portfolio performance
      hedge_annual_performance <- hedge * yield
      portfolio <- asset + hedge_annual_performance
      
      # Add performance to each vector and data frame
      asset_vec <- c(asset_vec, asset)
      hedge_vec <- c(hedge_vec, hedge_annual_performance)
      portfolio_vec <- c(portfolio_vec, portfolio)
      
      # Re-balance portfolio positions
      asset <- portfolio * (ASSET / 100)
      hedge <- portfolio * (HEDGE / 100)
      
      # Re set month count
      month_count <- 1
    } else {
      month_count <- month_count + 1
    }
    
  }
  
  print(portfolio / 100)
  print(prod((data/ 100) + 1))
  
  if (full == TRUE) {
    portfolio_df <- data.frame(hedge = hedge_vec, asset = asset_vec, portfolio = portfolio_vec)
    return(portfolio_df)
  } else {
    portfolio_df <- data.frame(portfolio_vec)
    colnames(portfolio_df) <- c("Portfolio")
    return(portfolio_df)
  }
}


bp <- balanced_portfolio(data = sp500_monthly$Return, HEDGE = 40, ASSET = 60,
                         yield = 8, full = FALSE)


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



# ===== BACKTEST WITH BOOTSRAP =====
### 100% S&P500
# Do a bootstrap and get possible future paths 
set.seed(1234)
n_paths <- 1000  # number of future paths
months_period <- 600  # period in months
paths <- replicate(n = n_paths, {
  
  path <- sample(x = monthly_return_distribution$Return, size = months_period, replace = TRUE, prob = monthly_return_distribution$Probability)
  path <- cumprod((path / 100 + 1)) * 100
  
  path
})

colnames(paths) <- paste0("path", 1:n_paths)

paths <- as.data.frame(paths) %>%
  mutate(Index = row_number())

paths_long_df <- data.frame(paths) %>%
  pivot_longer(cols = starts_with("path"), names_to = "path", values_to = "Value")


paths_long_df$Returns <- ((paths_long_df$Value / 100) - 1) * 100
paths_long_df$Log_value <- log(paths_long_df$Value)
paths_long_df$cagr <- log(paths_long_df$Value) - log(1) 
paths_long_df$Time <- rownames(paths_long_df)

# Identify the path that has the closest median value to the 95th and 5th percentile
# 95th percentile
percentile_95 <- quantile(paths_long_df[paths_long_df$Index == months_period, ]$Value, 0.95)
percentile_95 <- paths_long_df %>%
  filter(Index == months_period) %>%
  mutate(diff_from_target = abs(Value - percentile_95)) %>%
  arrange(diff_from_target) %>%
  slice(1) %>%
  pull(path)

# 5th percentile
percentile_5 <- quantile(paths_long_df[paths_long_df$Index == months_period, ]$Value, 0.05)
percentile_5 <- paths_long_df %>%
  filter(Index == months_period) %>%
  mutate(diff_from_target = abs(Value - percentile_5)) %>%
  arrange(diff_from_target) %>%
  slice(1) %>%
  pull(path)

ggplot(data = paths_long_df, aes(x = Index)) +
  geom_line(data = paths_long_df, aes(y = Log_value, group = path, color = "Possible Paths")) +
  geom_line(data = paths_long_df %>% filter(path == percentile_95),
            aes(y = Log_value, group = path, colour = "5% and 95% Percentile"), linewidth = 1) +
  geom_line(data = paths_long_df %>% filter(path == percentile_5),
            aes(y = Log_value, group = path, color = "5% and 95% Percentile"), linewidth = 1) + 
  geom_abline(slope = 0, intercept = log(100), linewidth = 1, group = "100", color = "100") +
  scale_color_manual(values = c("Possible Paths"= "black", "5% and 95% Percentile" = "green", "100" = "blue")) +
  scale_y_continuous(breaks = seq(0, round(max(paths_long_df$Log_value)), by = 0.5),
                     sec.axis = sec_axis( transform = ~ exp(.), name = "Value",
                                          breaks = round(exp(seq(0, max(paths_long_df$Log_value),  by = 0.5 )))) ) +
  scale_x_continuous(breaks = (seq(0, 600, by = 12))) +
  labs(title = "Simulated Return Paths Over 50 Years", x = "Months", y = "Value of Investment")


### Options and S&P 500
# Calculate multiple possible return paths (as return)
return_paths <- apply(paths, 2, function(x) c(NA, diff(x) / head(x, -1)))
return_paths <- return_paths * 100
return_paths <- data.frame(return_paths)
return_paths[is.na(return_paths)] <- 0

# Compute hedge performance for all possible paths
hedge_paths <- apply(return_paths, 2, function(x) {
  delta_hedge_protection(
    delta = 13, breakeven = -7, data = x, assetp_size = 97,
    hedgep_size = 3, p_rebalance = 12, full = FALSE)
  })


# Add an Index column to track from original data fram  e
hedge_paths <- as.data.frame(hedge_paths) %>%
  mutate(Index = row_number())

# Create a pivot data frame and create important columns
hedge_paths_long_df <- data.frame(hedge_paths) %>%
  pivot_longer(cols = starts_with("Portfolio"), names_to = "portfolio", values_to = "Value")

hedge_paths_long_df$Returns <- ((hedge_paths_long_df$Value / 100) - 1) * 100
hedge_paths_long_df$Log_value <- log(hedge_paths_long_df$Value)
hedge_paths_long_df$cagr <- log(hedge_paths_long_df$Value) - log(1) 
hedge_paths_long_df$Time <- rownames(hedge_paths_long_df)

# Identify the path that has the closest median value to the 95th and 5th percentile
# 95th percentile
percentile_95 <- quantile(hedge_paths_long_df[hedge_paths_long_df$Index == max(hedge_paths_long_df$Index), ]$Value, 0.95)
percentile_95 <- hedge_paths_long_df %>%
  filter(Index == max(hedge_paths_long_df$Index)) %>%
  mutate(diff_from_target = abs(Value - percentile_95)) %>%
  arrange(diff_from_target) %>%
  slice(1) %>%
  pull(portfolio)

# 5th percentile
percentile_5 <- quantile(hedge_paths_long_df[hedge_paths_long_df$Index == max(hedge_paths_long_df$Index), ]$Value, 0.05)
percentile_5 <- hedge_paths_long_df %>%
  filter(Index == max(hedge_paths_long_df$Index)) %>%
  mutate(diff_from_target = abs(Value - percentile_5)) %>%
  arrange(diff_from_target) %>%
  slice(1) %>%
  pull(portfolio)

# Plot everything
ggplot(data = hedge_paths_long_df, aes(x = Index)) +
  geom_line(data = hedge_paths_long_df, aes(y = Log_value, group = portfolio, color = "Possible Paths")) +
  geom_line(data = hedge_paths_long_df %>% filter(portfolio == percentile_95),
            aes(y = Log_value, group = portfolio, colour = "5% and 95% Percentile"), linewidth = 1) +
  geom_line(data = hedge_paths_long_df %>% filter(portfolio == percentile_5),
            aes(y = Log_value, group = portfolio, color = "5% and 95% Percentile"), linewidth = 1) + 
  geom_abline(slope = 0, intercept = log(100), linewidth = 1, group = "100", color = "100") +
  scale_color_manual(values = c("Possible Paths"= "black", "5% and 95% Percentile" = "green", "100" = "blue")) +
  scale_y_continuous(breaks = seq(0, round(max(hedge_paths_long_df$Log_value)), by = 0.5),
                     sec.axis = sec_axis( transform = ~ exp(.), name = "Value",
                                          breaks = round(exp(seq(0, max(hedge_paths_long_df$Log_value),  by = 0.5 )))) ) +
  scale_x_continuous(breaks = (seq(0, 50, by = 5))) +
  labs(title = "Simulated Return Paths Over 50 Years", x = "Years", y = "Value of Investment")
  

### Balanced portfolio - 60% S&P500 and 40% bonds
# Bootstrap for balanced portfolio
balanced_portfolio_paths <- apply(return_paths, 2, function(x) {
  balanced_portfolio(data = x, HEDGE = 40, ASSET = 60, yield = 5, full = FALSE)
  })

# Add an Index column to track from original data frame
balanced_portfolio_paths <- as.data.frame(balanced_portfolio_paths) %>%
  mutate(Index = row_number())

# Create a pivot data frame and create important columns
balanced_portfolio_paths_long_df <- data.frame(balanced_portfolio_paths) %>%
  pivot_longer(cols = starts_with("Portfolio"), names_to = "portfolio", values_to = "Value")

balanced_portfolio_paths_long_df$Returns <- ((balanced_portfolio_paths_long_df$Value / 100) - 1) * 100
balanced_portfolio_paths_long_df$Log_value <- log(balanced_portfolio_paths_long_df$Value)
balanced_portfolio_paths_long_df$cagr <- log(balanced_portfolio_paths_long_df$Value) - log(1) 
balanced_portfolio_paths_long_df$Time <- rownames(balanced_portfolio_paths_long_df)

# Identify the path that has the closest median value to the 95th and 5th percentile
# 95th percentile
percentile_95 <- quantile(balanced_portfolio_paths_long_df[balanced_portfolio_paths_long_df$Index == max(balanced_portfolio_paths_long_df$Index), ]$Value, 0.95)
percentile_95 <- balanced_portfolio_paths_long_df %>%
  filter(Index == max(balanced_portfolio_paths_long_df$Index)) %>%
  mutate(diff_from_target = abs(Value - percentile_95)) %>%
  arrange(diff_from_target) %>%
  slice(1) %>%
  pull(portfolio)

# 5th percentile
percentile_5 <- quantile(balanced_portfolio_paths_long_df[balanced_portfolio_paths_long_df$Index == max(balanced_portfolio_paths_long_df$Index), ]$Value, 0.05)
percentile_5 <- balanced_portfolio_paths_long_df %>%
  filter(Index == max(balanced_portfolio_paths_long_df$Index)) %>%
  mutate(diff_from_target = abs(Value - percentile_5)) %>%
  arrange(diff_from_target) %>%
  slice(1) %>%
  pull(portfolio)

# Plot everything
ggplot(data = balanced_portfolio_paths_long_df, aes(x = Index)) +
  geom_line(data = balanced_portfolio_paths_long_df, aes(y = Log_value, group = portfolio, color = "Possible Paths")) +
  geom_line(data = balanced_portfolio_paths_long_df %>% filter(portfolio == percentile_95),
            aes(y = Log_value, group = portfolio, colour = "5% and 95% Percentile"), linewidth = 1) +
  geom_line(data = balanced_portfolio_paths_long_df %>% filter(portfolio == percentile_5),
            aes(y = Log_value, group = portfolio, color = "5% and 95% Percentile"), linewidth = 1) + 
  geom_abline(slope = 0, intercept = log(100), linewidth = 1, group = "100", color = "100") +
  scale_color_manual(values = c("Possible Paths"= "black", "5% and 95% Percentile" = "green", "100" = "blue")) +
  scale_y_continuous(breaks = seq(0, round(max(balanced_portfolio_paths_long_df$Log_value)), by = 0.5),
                     sec.axis = sec_axis( transform = ~ exp(.), name = "Value",
                                          breaks = round(exp(seq(0, max(balanced_portfolio_paths_long_df$Log_value),  by = 0.5 )))) ) +
  scale_x_continuous(breaks = (seq(0, 50, by = 5))) +
  labs(title = "Simulated Return Paths Over 50 Years", x = "Years", y = "Value of Investment")
  

# Compare 
print("Pure Portfolio")
summary(paths_long_df[paths_long_df$Index == months_period, ]$Value)
summary( log(paths_long_df[paths_long_df$Index == months_period, ]$Value / 100) )

print("Hedge:")
summary(balanced_portfolio_paths_long_df[balanced_portfolio_paths_long_df$Index == 50, ]$Value)
summary( log(balanced_portfolio_paths_long_df[balanced_portfolio_paths_long_df$Index == 50, ]$Value / 100) )





# Compare 
print("===== 100% S&P500 Portfolio =====")
summary(paths_long_df[paths_long_df$Index == months_period, ]$Value)
summary( log(paths_long_df[paths_long_df$Index == months_period, ]$Value / 100) )

print("===== Options & S&P500 =====")
summary(hedge_paths_long_df[hedge_paths_long_df$Index == 50, ]$Value)
summary( log(hedge_paths_long_df[hedge_paths_long_df$Index == 50, ]$Value / 100) )

print("===== Balanced Portfolio =====")
summary(balanced_portfolio_paths_long_df[balanced_portfolio_paths_long_df$Index == 50, ]$Value)
summary( log(balanced_portfolio_paths_long_df[balanced_portfolio_paths_long_df$Index == 50, ]$Value / 100) )
