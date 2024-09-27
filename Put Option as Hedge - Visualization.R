library(ggplot2)
library(tidyr)
library(quantmod)

source("Put Options - Data Analysis.R")


# "SPXW240822P04825000" 
OPTION <- "SPXW241022P05200000"  # normal (33 day)


# Compute how efficient a given put options is to protect portfolio
hedge_efficiency <- function(contractid, underlying, data) {
  # Takes three arguments
  # contractid: option's contract ID
  # underlying: option's underlying asset ticker
  # data: the data set containing put option information
  
  # Get asset's current price -- ERROR MAY be HERE
  uprice <- unlist(getQuote(underlying)$Open)
  
  # Get option data
  option <- data[data$ContractID == contractid, ]
  strike <- as.numeric(unlist(option["Strike"]))  # strike price
  premium <- as.numeric(unlist(option["Ask"]))  # premium
  hedge_size <- as.numeric(option["H_pct"])  # hedge as a percentage of portfolio
  
  # Define and initialize important vectors and variables
  downfall <- 1:50  # market downfalls from 1 to 30%
  portfolio <- 100  # portfolio size
  not_hedge <- portfolio - 3
  payoff_vec <- c()
  df <- c()
  p_protection <- c()
  
  
  for (fall in downfall) {
    percentage_fall <- 1 - (fall / 100)  # market fall as percentage
    # Impact on portfolio of market downfall 
    p_loss <- not_hedge - (not_hedge * percentage_fall)  # on portfolio
    loss <- uprice * percentage_fall  # on underlying asset
    print(strike)
    
    
    # Compute necessary payoff for break even in percentage - the rejection line
    pct_payoff <- round(p_loss / hedge_size, 2) * 100  
    payoff_vec <- c(payoff_vec, pct_payoff)  # add to a data frame
    
    # Compute hedge actual payoff
    hedge_payoff <- round((strike - premium  - loss) / premium, 3)  # hedge return
    cat("strike:", strike, "|", "premium:", premium, "loss:", loss, "\n")
    hedge_payoff <- hedge_payoff * 100  # payoff as percentage
    
    
    # Add payoff to data frame
    if (hedge_payoff > 0) {
      df <- c(df, hedge_payoff)  # add payoff a data frame
    } else {
      hedge_payoff <- 0
      df <- c(df, hedge_payoff)
    }
    
    
    # Impact of hedge on portfolio
    h_impact <- hedge_size * (hedge_payoff / 100)
    p_impact <- round(100 - p_loss + h_impact, 2)
    p_protection <- c(p_protection, p_impact)
  }
  
  # Combine all information into a single data frame
  result <- data.frame(Downfall = -downfall,
                       data.frame(Breakeven = unlist(payoff_vec)),
                       data.frame(Payoff = df),
                       data.frame(Portfolio_impact = p_protection))
  
  result <- cbind(Index = as.numeric(rownames(result)), result)
  
  return(result)
}

data <- hedge_efficiency(OPTION, "^SPX", hedge3_floor0)


# Define a scaling factor to help graph and graph it
scaling_factor <- max(data$Breakeven) / max(data$Portfolio_impact)
ggplot(data = data, aes(x=-Downfall)) +
  geom_line(aes(y = Breakeven, color = "Put Payoff for 0% loss")) +
  geom_point(aes(y = (100 + Downfall) * scaling_factor, color = "Market Downfall")) +
  geom_line(aes(y = Payoff, color = "Put Option Payoff")) +
  geom_point(aes(y = Portfolio_impact * scaling_factor, color = "Portfolio Impact")) +
  geom_text(aes(y = Portfolio_impact * scaling_factor,
                label = ifelse(Index %% 2 == 0, Portfolio_impact, "")), hjust = 0.5, vjust = -1.5) +
  geom_text(aes(y = (100 + Downfall) * scaling_factor, label = ifelse(Index %% 2 == 0, Downfall, "")), hjust = -0.5, vjust = 2.5) + 
  scale_y_continuous(breaks = seq(0, max(data), by = round(max(data) / length(data$Index))), name = "Return (%)",
    sec.axis = sec_axis(~ . / scaling_factor, name = "Portfolio Impact (Scaled)",
                        breaks = seq(0, max(data$Payoff), by = 10))) + # Scale secondary axis 
  scale_x_continuous(breaks = seq(0, 50, by = 2)) +
  scale_color_manual(values = c("black", "blue", "green", "red")) +
  labs(title = "Put Option as Hedge For Portfolio", y = "Return (%)", x = "Market Downfall (%)")


# Define important vectors and variables
lower_hedge_sizes <- c(seq(0.5, 1, by = (0.05)))  # possible hedge sizes as a percentage of portfolio
upper_hedge_sizes <- c(seq(1.5, 3, by = 0.5))
hedge_sizes <- c(lower_hedge_sizes, upper_hedge_sizes)
downfall <- c(seq(1, 30, by = (0.5)))  # range of possible market downfalls
payoff_df <- data.frame()
portfolio <- 100  # portfolio size


for (fall in downfall) {
  percentage_fall <- 1 - (fall / 100)
  # impact on portfolio
  p_loss <- portfolio - (portfolio * percentage_fall)
  payoff <- round(p_loss / hedge_sizes, 2) * 100  # necessary payoff for break even
  payoff_df <- rbind(payoff_df, payoff)  # add to a data frame
}

# Column names
col_names <- paste("Hedge", hedge_sizes, sep = "")
col_names <- c("Downfall", col_names)
payoff_df <- cbind(downfall, payoff_df)
colnames(payoff_df) <- col_names

payoff_df_long <- pivot_longer(payoff_df,
                               cols = -Downfall,
                               names_to = "Hedge",
                               values_to = "Value") 

ggplot() +
  geom_line(data = payoff_df_long, aes(x=Downfall, y = Value, color = Hedge, group = Hedge)) +
  scale_y_continuous(breaks = seq(0, max(payoff_df), by = (500))) +
  scale_x_continuous(breaks = seq(0, 30, by = (2))) +
  ggtitle("Hedge Return For 0% loss") +
  labs(x = "Market Downfall (%)", y = "Hedge Return (%)")
  

