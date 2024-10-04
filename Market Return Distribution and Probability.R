# Load required packages and set working directory
library(quantmod)
library(lubridate)
library(ggplot2)
library(dplyr)
library(pracma)
library(evir)
setwd("/home/rafaelconde/Research/Stock Market Return Distribution and Hedging/")


start_date <- "1975-01-01"
end_date <- Sys.Date()



# ===== GET THE RETURN DISTRIBUTIONS =====
SPX <- getSymbols("^SPX", from = start_date, to = end_date, auto.assign = FALSE, periodicity = "daily", src="yahoo")

# Get SP500 information
# Calculate returns
arithmetic_returns <- data.frame(allReturns(SPX, type = "arithmetic"))
arithmetic_returns$Date <- as.Date(rownames(arithmetic_returns))
log_returns <- data.frame(allReturns(SPX, type = "log"))
log_returns$Date <- as.Date(rownames(log_returns))


# Daily returns
sp500_daily <- data.frame(cbind(SPX, Return = 100 * (arithmetic_returns$daily - 1) + 100))
sp500_daily$Date <- as.Date(rownames(sp500_daily)) 
sp500_daily <- na.omit(sp500_daily)

# Weekly returns
sp500_weekly <- data.frame(cbind(SPX, Return = 100 * (arithmetic_returns$weekly - 1) + 100))
sp500_weekly <- na.omit(sp500_weekly)
sp500_weekly$Date <- as.Date(rownames(sp500_weekly))
sp500_weekly <- sp500_weekly[-1, ]

# Monthly returns
sp500_monthly <- data.frame(cbind(SPX, Return = 100 * (arithmetic_returns$monthly - 1) + 100))
sp500_monthly <- na.omit(sp500_monthly)
sp500_monthly$Date <- as.Date(rownames(sp500_monthly))
sp500_monthly <- data.frame(sp500_monthly[-1, ])

# Quarterly returns
sp500_quarterly <- data.frame(cbind(SPX, Return = 100 * (arithmetic_returns$quarterly - 1) + 100))
sp500_quarterly <- na.omit(sp500_quarterly)
sp500_quarterly$Date <- as.Date(rownames(sp500_quarterly))
sp500_quarterly <- sp500_quarterly[-1, ]

# Yearly returns
sp500_yearly <- data.frame(cbind(SPX, Return = 100 * (arithmetic_returns$yearly - 1) + 100))
sp500_yearly <- na.omit(sp500_yearly)
sp500_yearly$Date <- as.Date(rownames(sp500_yearly))
sp500_yearly <- sp500_yearly[-1, ]




# ===== GRAPH RETURN DISTRIBUTIONS ===== 
# SPX daily return distribution 
ggplot(sp500_daily, aes(x = Return)) + 
  geom_histogram(aes(y = ..count..), bins = 20, fill = "blue", color = "black", alpha = 0.7) + 
  geom_text(stat='bin', aes(y = ..count.., label = ..count..), vjust = -2.5, bins = 20) + 
  geom_text(stat='bin', aes(y = ..count.., label = scales::percent(..count../sum(..count..))), 
            vjust = -0.5, bins = 20) +
  labs(title = "Stock Daily Returns Histogram", x = "Returns", y = "Frequency") +
  scale_x_continuous(breaks = seq(round(min(sp500_daily$Return)), round(max(sp500_daily$Return)), by = 2))


# SPX weekly return distribution 
ggplot(sp500_weekly, aes(x = Return)) + 
  geom_histogram(aes(y = ..count..), bins = 20, fill = "blue", color = "black", alpha = 0.7) + 
  geom_text(stat='bin', aes(y = ..count.., label = ..count..), vjust = -2.5, bins = 20) + 
  geom_text(stat='bin', aes(y = ..count.., label = scales::percent(..count../sum(..count..))), 
            vjust = -0.5, bins = 20) +
  labs(title = "Stock Weekly Returns Histogram", x = "Returns", y = "Frequency") +
  scale_x_continuous(breaks = seq(round(min(sp500_weekly$Return)), round(max(sp500_weekly$Return)), by = 2))


# SPX monthly return distribution 
ggplot(sp500_monthly, aes(x = Return)) + 
  geom_histogram(aes(y = ..count..), bins = 20, fill = "blue", color = "black", alpha = 0.7) + 
  geom_text(stat='bin', aes(y = ..count.., label = ..count..), vjust = -2.5, bins = 20) + 
  geom_text(stat='bin', aes(y = ..count.., label = scales::percent(..count../sum(..count..))), 
            vjust = -0.5, bins = 20) +
  labs(title = "Stock Monthly Returns Histogram", x = "Returns", y = "Frequency") +
  scale_x_continuous(breaks = seq(round(min(sp500_monthly$Return)), round(max(sp500_monthly$Return)), by = 2))


# Quarterly returns
ggplot(sp500_quarterly, aes(x = Return)) + 
  geom_histogram(aes(y = ..count..), bins = 20, fill = "blue", color = "black", alpha = 0.7) + 
  geom_text(stat='bin', aes(y = ..count.., label = ..count..), vjust = -2.5, bins = 20) + 
  geom_text(stat='bin', aes(y = ..count.., label = scales::percent(..count../sum(..count..))), 
            vjust = -0.5, bins = 20) +
  labs(title = "Stock Quarterly Returns Histogram", x = "Returns", y = "Frequency") +
  scale_x_continuous(breaks = seq(round(min(sp500_quarterly$Return)), round(max(sp500_quarterly$Return)), by = 2))


# SPX annual return distribution
ggplot(sp500_yearly, aes(x = Return)) + 
  geom_histogram(aes(y = ..count..), bins = 20, fill = "blue", color = "black", alpha = 0.7) + 
  geom_text(stat='bin', aes(y = ..count.., label = ..count..), vjust = -2.5, bins = 20) + 
  geom_text(stat='bin', aes(y = ..count.., label = scales::percent(..count../sum(..count..))), 
            vjust = -0.5, bins = 20) +
  labs(title = "Stock Annual Returns Histogram", x = "Returns", y = "Frequency") +
  scale_x_continuous(breaks = seq(round(min(sp500_yearly$Return)), round(max(sp500_yearly$Return)), by = 2))
  

# ====== GRAPH VARIANCE OF RETURNS =====
# Daily
ggplot(data = arithmetic_returns, aes(x = Date)) +
  geom_line(aes(y = daily)) +
  labs(title = "Daily Variance of Returns", x = "Date", y =  "Variance")

# Weekly
ggplot(data = sp500_weekly, aes(x = Date, y = 1 + ( Return / 100 ))) +
  geom_line() +
  labs(title = "Weekly Variance of Returns", x = "Date", y =  "Variance")

# Monthly
ggplot(data = sp500_monthly, aes(x = Date, y = 1 + ( Return / 100 ))) +
  geom_line() +
  labs(title = "Monthly Variance of Returns", x = "Date", y =  "Variance")

# Quarterly
ggplot(data = sp500_quarterly, aes(x = Date, y = 1 + ( Return / 100 ))) +
  geom_line() +
  labs(title = "Quarterly Variance of Returns", x = "Date", y = "Variance")

# Annual
ggplot(data = sp500_yearly, aes(x = Date, y = 1 + ( Return / 100 ))) +
  geom_line() +
  labs(title = "Annual Variance of Returns", x = "Date", y = "Variance")




# ===== BOOSTRAP =====
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
      sampled_returns <- sample(x = returns, size = num, replace = TRUE, prob = probabilities)
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

# Daily returns
daily_return_distribution <- sp500_daily %>%
  group_by(Return) %>%
  dplyr::summarize(Frequency = n())
daily_return_distribution$Probability <- round((daily_return_distribution$Frequency / sum(daily_return_distribution$Frequency)) * 100, 3)

# Weekly returns
weekly_return_distribution <- sp500_weekly %>%
  group_by(Return) %>%
  dplyr::summarize(Frequency = n())
weekly_return_distribution$Probability <- 100 * (weekly_return_distribution$Frequency / sum(weekly_return_distribution$Frequency))

# Monthly returns
monthly_return_distribution <- sp500_monthly %>% 
  group_by(Return) %>%
  dplyr::summarise(Frequency = n())
monthly_return_distribution$Probability <- 100 * (monthly_return_distribution$Frequency / sum(monthly_return_distribution$Frequency))

# Quarterly returns
quarterly_return_distribution <- sp500_quarterly %>% 
  group_by(Return) %>%
  dplyr::summarise(Frequency = n())
quarterly_return_distribution$Probability <- 100 * (quarterly_return_distribution$Frequency / sum(quarterly_return_distribution$Frequency))

# Yearly returns
yearly_return_distribution <- sp500_yearly %>% 
  group_by(Return) %>%
  dplyr::summarise(Frequency = n())
yearly_return_distribution$Probability <- 100 * (yearly_return_distribution$Frequency / sum(yearly_return_distribution$Frequency))


# Apply bootstrap
write.csv(bootstrap(10000, 180, daily_return_distribution$Return, daily_return_distribution$Probability), "Possible_Returns_PathsTEST.csv")



