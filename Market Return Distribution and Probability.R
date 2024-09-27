# Load required packages and set working directory
library(quantmod)
library(lubridate)
library(ggplot2)
library(dplyr)
setwd("/home/rafaelconde/Research/Stock Market Return Distribution and Hedging/")


# ===== RETURN AND BOOSTRAP FUNCTION =====
# Gets an xts object as a dataset and an integer to serve as money invested
get_return <- function(data, amt) {
  # clean data set
  colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  data <- data.frame(data)
  data <- na.omit(data)
  amt_vec <- c()
  
  # get open and close price to calculate return
  data$Return <- (data$Close / data$Open) -1  # calculate return in decimals
  #data$Return <- (data$Return - 1)   # format return to percentage
  
  for (n in data$Return) {
    amt <- (amt * n) + amt
    amt_vec <- c(amt_vec, amt)
  }
  data$Total_Profit <- round(amt_vec, 2)
  data$Return <- round(data$Return, 3)
  data$Date <- rownames(data)
    
  return(data) 
}

# Gets a data frame with frequencies (dplyr) and returns it trimmed
trim_distribution <- function(data, upper_limit, lower_limit) {
  frequency_vec <-c()
  # trim lower returns
  sum_frequencies <- sum(data$Frequency[data$Return < lower_limit])
  data$Frequency[data$Return == lower_limit] <- data$Frequency[data$Return == lower_limit] + sum_frequencies
  data <- data[!data$Return < lower_limit, ]
  # trim upper returns
  sum_frequencies <- sum(data$Frequency[data$Return > upper_limit])
  data$Frequency[data$Return == upper_limit] <- data$Frequency[data$Return == upper_limit] + sum_frequencies
  data <- data[!data$Return > upper_limit, ]
  # calculate probability
  data$Probability <- round((data$Frequency / sum(data$Frequency)) * 100, 1)
  
  return(data)
}

# Models different scenarios of returns 
bootstrap <- function(n_bootstrap, days, returns, probabilities) {
  # takes 4 parameters
  # n_bootstrap (integers): number of times to replicate process, i.e possible paths
  # days (integers): number of days/ scenarios, i.e 1 to "days"
  # returns (vector): possible (registered) returns
  # probabilities (vector): probability of each given (registered) return
  
  # create data frame to hold path information
  paths <- data.frame(nrow=20, ncol=10000)
  returns <- (returns / 100) + 1

  
  # simulate possible paths for 1 to 180 days
  for (num in 1:days) {
    
    # Get a return (randomly) for the defined (n_bootstrap) times
    result <- replicate(n_bootstrap, {
      # Get a sample of returns
      sampled_returns <- sample(x = returns, size = num, replace = TRUE, prob = probability_vec)
      as.numeric(prod(sampled_returns))  # multiply all items in vector
      })
    
    paths <- cbind(paths, result)  # add result to paths
  }
  
  # Prettify data
  paths <- paths[, -1:-2]  # remove first two columns
  names <- paste(seq(1, days), "days")  # create vector with column names
  colnames(paths) <- names
  
  return(paths)
}


# ===== APPLY BOOTSTRAP =====
#write.csv(bootstrap(10000, 180, return_vec, probability_vec), "Possible_Returns_Paths.csv")

# ===== GET THE RETURN DISTRIBUTIONS =====
# Get SP500 information
sp500_daily <- get_return(getSymbols("^SPX", from = "1990-01-01", to = Sys.Date(), auto.assign = FALSE, periodicity = "daily", src="yahoo"), 100)
sp500_weekly <- get_return(getSymbols("^SPX", from = "1990-01-01", to = Sys.Date(), auto.assign = FALSE, periodicity = "weekly", src="yahoo"), 100)
sp500_monthly <- get_return(getSymbols("^SPX", from = "1990-01-01", to = Sys.Date(), auto.assign = FALSE, periodicity = "monthly", src="yahoo"), 100)
sp500_daily$Return <- round(sp500_daily$Return * 100, 1)
sp500_weekly$Return <- round(sp500_weekly$Return * 100, 1)
sp500_monthly$Return <- round(sp500_monthly$Return * 100, 1)

# Monthly returns
monthly_return_distribution <- sp500_monthly %>% 
  group_by(Return) %>%
  dplyr::summarise(Frequency = n())
monthly_return_distribution$Probability <- 100 * (monthly_return_distribution$Frequency / sum(monthly_return_distribution$Frequency))
monthly_hist <- trim_distribution(monthly_return_distribution, 4.4, -4.2)
# Weekly returns
weekly_return_distribution <- sp500_weekly %>%
  group_by(Return) %>%
  dplyr::summarize(Frequency = n())
weekly_return_distribution$Probability <- 100 * (weekly_return_distribution$Frequency / sum(weekly_return_distribution$Frequency))
weekly_hist <- weekly_return_distribution
weekly_hist <- trim_distribution(weekly_hist, 4.4, -4.2)
# Daily returns
daily_return_distribution <- sp500_daily %>%
  group_by(Return) %>%
  dplyr::summarize(Frequency = n())
daily_return_distribution$Probability <- round((daily_return_distribution$Frequency / sum(daily_return_distribution$Frequency)) * 100, 3)
daily_hist <- trim_distribution(daily_return_distribution, 4.4, -4.2)


# Save daily_return_distribution to a csv file
write.csv(daily_return_distribution, file = "Distribution_of_Daily_Returns.csv")
write.csv(monthly_return_distribution, file = "Distribution_of_Monthly_Returns.csv")


# ===== GRAPH RETURN DISTRIBUTIONS ===== 
# SPX monthly return distribution expanded
ggplot(data = monthly_hist, aes(x = Return, y = Probability)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  scale_y_continuous(breaks = seq(min(monthly_hist$Probability), max(monthly_hist$Probability), by = 1)) +
  scale_x_continuous(breaks = seq(-4.2, 4.5, by = 0.2)) +
  ggtitle("SPX Monhly Return Distribution") +
  labs(x = "Return", y = "Probability Frequency")
# monthly S&P50 distribution
ggplot(data = sp500_monthly, aes(x = Return)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 1, fill = "blue",
                 color = "black", alpha = 0.7) +
  geom_text(stat = "bin", aes(y = ..count.. / sum(..count..),
                              label = scales::percent(..count.. / sum(..count..))), vjust = -0.5, binwidth = 1) +
  labs(x = "Return", y = "Probability Density") +
  ggtitle("S&P500 Monthly Return Distribution") +
  scale_x_continuous(limits = c(-18, 12), breaks = seq(-18, 12, by = 1))


# SPX weekly return distribution expanded
ggplot(data = weekly_hist, aes(x = Return, y = Probability)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  scale_y_continuous(breaks = seq(min(weekly_hist$Return), max(weekly_hist$Return), by = 0.2)) +
  scale_x_continuous(breaks = seq(-4.2, 4.5, by = 0.2)) +
  ggtitle("SPX Weekly Return Distribution") + 
  labs(x = "Return", y = "Probability Frequency")
# weekly S&P50 distribution
ggplot(data = sp500_weekly, aes(x = Return)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 1, fill = "blue",
                 color = "black", alpha = 0.7) +
  geom_text(stat = "bin", aes(y = ..count.. / sum(..count..),
                              label = scales::percent(..count.. / sum(..count..))), vjust = -0.5, binwidth = 1) +
  labs(x = "Return", y = "Probability Density") +
  ggtitle("S&P500 Weekly Return Distribution") +
  scale_x_continuous(limits = c(-18, 12), breaks = seq(-18, 12, by = 1))


# daily SPX histogram expanded
ggplot(data = daily_hist, aes(x = Return, y = Probability)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black") +
  scale_x_continuous(breaks = seq(min(daily_hist$Return), max(daily_hist$Return), by = 0.2)) +
  scale_y_continuous(breaks = seq(min(daily_hist$Probability), max(daily_hist$Probability), by = 0.3)) +
  ggtitle("Frequency Distribution of SPX Daily Return") +
  labs(x = "Return (%)", y = "Probability Frequency")
# Daily S&P50 distribution - simplified
ggplot(data = sp500_daily, aes(x = Return)) +
  geom_histogram(aes(y = ..count.. / sum(..count..)), binwidth = 1, fill = "blue",
                 color = "black", alpha = 0.7) +
  geom_text(stat = "bin", aes(y = ..count.. / sum(..count..),
                              label = scales::percent(..count.. / sum(..count..))), vjust = -0.5, binwidth = 1) +
  labs(x = "Return", y = "Probability Density") +
  ggtitle("S&P500 Daily Return Distribution") +
  scale_x_continuous(limits = c(-10, 12), breaks = seq(-10, 12, by = 1))



