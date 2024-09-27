library(quantmod)
library(dplyr)
library(lubridate)



# Global variables important data sets
portfolio_size <- 8750  # portfolio size
setwd("/home/rafaelconde/Research/Stock Market Return Distribution and Hedging/")


# Compute profit of a put option
compute_profit <- function(data, price) {
  # Takes on two arguments:
  # data: data frame with put option information
  # price: the current price of the underlying asset
  downfall_vec <- c(0.75, 0.80, 0.85, 0.90, 0.95, 0.98)
  
  # Calculate the profit matrix and return matrix using outer
  pf_matrix <- outer(data$Strike, price * downfall_vec, "-") - data$Ask
  r_matrix <- round((pf_matrix / data$Ask) * 100, 2)
  
  # Replace negative values
  pf_matrix[pf_matrix < 0] <- -data$Ask[row(pf_matrix)[pf_matrix < 0]]

  r_matrix[r_matrix < 0] <- -100
  
  # Combine profits and returns into a single data frame
  df <- cbind(pf_matrix, r_matrix)
  
  # Set column names
  colnames(df) <- c("Profit25", "Profit20", "Profit15", "Profit10", "Profit5",
                    "Profit2", "Return25", "Return20", "Return15", "Return10",
                    "Return5", "Return2" )
  
  return(df)
}

# Compute which function are profitable for tail hedging
cost_effective <- function(data, hedge_pct, portfolio, floor) {
  # Function to compute which functions are profitable for a tail hedging strategy
  # Takes 4 arguments:
  # data: a data frame with put options
  # hedge_pct: an integer representing the desired percentage of portfolio to be hedges
  # portfolio: an integer representing the total money amount of your portfolio
  # floor: an integer representing the downside you want hedge (your loss capacity)
  not_hedge_size <- 100 - hedge_pct
  
  # Remove unnecessary columns
  data <- data[ , -(4:5)]
  data <- data[ , -(5:9)]
  
  # Define the returns and downfall vectors
  return_cols <- c("Return2", "Return5", "Return10", "Return15", "Return20", "Return25")
  downfall_vec <- c(0.02, 0.05, 0.10, 0.15, 0.20, 0.25) * not_hedge_size
  
  # Apply the cost-effective logic to each row
  profitable_data <- apply(data, 1, function(row) {
    
    # Get number of times option needs to be rolled annually
    roll <- as.numeric(row["Roll_Number1Y"])  
    
    # Compute hedge as percentage of portfolio based on roll number
    portfolio_pct <- round(hedge_pct / roll, 3) 
    
    # Get returns
    return_vec <- as.numeric(row[return_cols])  # get all returns 
    return_vec <- (return_vec / 100) * portfolio_pct  # option return in portfolio
    
    # Check if hedge is effective on portfolio
    profitable_vec <- round(return_vec - downfall_vec, 2) 
    
    # Check if option (considering its size on portfolio) is profitable for any downfall
    if (any(profitable_vec >= floor)) {  # payoff explosive enough
      c(row["ContractID"], portfolio_pct, profitable_vec)
    } else {
      NULL
    }
  })
  print(class(profitable_data))  # check type for mistakes - ERROR PRONE!
  
  # Convert the list of profitable data to a data frame
  profitable_data <- do.call(rbind, profitable_data)
  colnames(profitable_data) <-c("ContractID", "H_pct", "PGain2", "PGain5", "PGain10", "PGain15", "PGain20", "PGain25") 
  
  # Merge with the original data
  final <- merge(data, profitable_data, by = "ContractID")
  
  # Convert necessary columns to numeric
  final[ , 8:26] <- lapply(final[ , 8:26], as.numeric)
  
  # Format the Month column and calculate Total_PGain
  final$Month <- format(as.Date(final$Expiration), "%Y-%m")
  final$Total_PGain <- round((final$PGain2 + final$PGain5 +  final$PGain10 + final$PGain15 + final$PGain20 + final$PGain25) / final$Ask, 2)
  
  # Calculate actual hedge size of portfolio
  final$HedgeSize <- final$H_pct * (portfolio / 100)
  
  # Compute protection per premium unit PpP
  final$PpP <- final$Breakeven / -final$Ask
  
  return(final)
}

# Compute probability of a given future return
compute_probability <- function(option_data) {
  # Function to compute probability of a given future return
  # Takes two data frames: one with the options; the other with the return
  # Distribution with the given probabilities. And takes an integer, how many times
  # The bootstrap is executed
  
  # Read data from file and create important vectors
  paths <- read.csv("Possible_Returns_Paths.csv")  # get possible future returns
  distribution_data <- read.csv("Distribution_of_Daily_Returns.csv")  # return distribution
  
  # Compute the probability of profitability in every row of options data frame
  probability_values <- apply(option_data, 1, function(row) {
    # Manipulate, load and convert necessary data
    breakeven <- as.numeric(row["Breakeven"])  # break even point
    breakeven <- (breakeven / 100) + 1  # break even as a decimal
    period <- as.numeric(time_length((row["Roll_Period"]), "days")) # roll period
    
    # If period is longer than the the calculated (number of columns), add a NA 
    if (period > dim(paths)[2]) {
      NA
      
    } else { 
      # Get column of returns for the given period
      selected_paths <- as.vector(paths[, period])
      
      # Check how many times break even point is achieved and compute odds
      num <- sum(breakeven >= selected_paths)  # number of times option is profitable
      result <- round((num / length(selected_paths)) * 100, 4)  # compute odds 
      result # add it to vector
    }})
  
  return(probability_values)
}

# Get the all put options inside a data frame
get_option_data <- function(ticker) {
  # Function to get data on put of a specified underlying asset
  # Takes 1 argument:
  # ticker: a string representing the ticker of the underlying stock/ asset
  
  # Get asset's information
  data <- getOptionChain(ticker, src = "yahoo", Exp = NULL)  # option data from database
  CURRENT_PRICE <- getQuote(ticker)$Last  # Get asset's current price
  
  # Get all put options in a data frame
  puts_df <- data.frame()
  for (item in data) {
    puts_df <- bind_rows(puts_df, item$puts)
  }
  
  # Treat data in put options data frame
  puts_df <- na.omit(puts_df)  # remove NA values
  
  # Replace Ask prices of 0 by Last prices
  puts_df$Ask <- ifelse(puts_df$Ask == 0, puts_df$Last, puts_df$A)
  
  # Compute the % SPX decline each put needs to be profitable - break even point
  puts_df$Breakeven <- round(( (((puts_df$Strike - puts_df$Ask) / CURRENT_PRICE) - 1) * 100), 3)  # break even point
  puts_df$Expiration <- as.Date(puts_df$Expiration)  # convert to date
  
  # Compute number of times option needs to be rolled in a year and rolling period
  puts_df$Roll_Period <- as.period(puts_df$Expiration - Sys.Date())
  puts_df$Roll_Number1Y <- years(1) / puts_df$Roll_Period  
  
  # Remove unnecessary columns
  puts_df <- puts_df[ , -2:-3]  
  puts_df <- puts_df[ , -5:-6]
  puts_df <- puts_df[!puts_df$Ask == 0, ]  # remove options prices with 0 ask price
  
  
  # Get profitability of options
  puts_df <- cbind(puts_df, compute_profit(puts_df, CURRENT_PRICE))
   return(puts_df)
}

# Get the data (may be commented for faster running time)
puts_df <- get_option_data("^SPX")

# Selects an option for a given selected criteria
select_hedge <- function(expl, expu, pbreakeven, data) {
  # Takes 4 arguments
  # expl: target lower limit of expiration period
  # expu: target upper limit of expiration period
  # pbreakeven: target break even point for hedge
  # data: data set containing put option information
  
  # Define important variables
  maxbp <- (pbreakeven + 1) * - 1
  minbp <- (pbreakeven - 1) * - 1
  
  
  # Filter by date - remove options that are not in the expiration date range
  data <- data[data$Roll_Period <= days(expu) & data$Roll_Period >= days(expl), ]
  
  # Filter by breakeven point - remove non-target break even points
  data <- data[data$Breakeven >= maxbp & data$Breakeven <= minbp, ]
  
  
  # Compute protection per premium unit - PpP
  data$PpP <- data$Breakeven / -data$Ask
  
  
  print(dim(data))
  return(data)
}



# Filter options by explosive payoff and how often it is profitable
hedge3_floor0 <- cost_effective(puts_df[!puts_df$Breakeven > 0, ], 3, portfolio_size, 0) 
hedge3_floor0$Probability <- compute_probability(hedge3_floor0)
hedge3_floor5 <- cost_effective(puts_df[!puts_df$Breakeven > 0, ], 3, portfolio_size, -5) 
hedge3_floor5$Probability <- compute_probability(hedge3_floor5)
selected <- select_hedge(25, 36, 9, hedge3_floor0) 
