
# Define functions
# Function to do a Value at Risk analyzes of portfolios
compute_VaR <- function(data, names) {
  # Takes 2 parameters:
  # data: a vector with returns in +-10%
  # names: a vector with strings for the title of the VaR graphs
  # Define un-hedged portfolio and insured portfolio vectors
  p <- c()
  p_insured <- c()
  
  for (return in data) {
    
    # Compute arithmetic return
    areturn <- (return / 100) + 1
    
    # Compute p_insured performance
    # Assume breakeven of -8%, and a delta of 15
    hedge <- (-8 - return) * 15
    hedge <- max(hedge, -3)
    
    insured_return <- hedge + (97 * areturn)
    
    
    p <- c(p, 100 * areturn)
    p_insured <- c(p_insured, insured_return)
    #cat("Return: ", return, "|====| Hedge: ", hedge, "| Test:", areturn * 97, "| p insured:", insured_return, "\n")
  }
  
  VaR <- data.frame(Downfall = data, Portfolio = p, InsuredPortfolio = p_insured)
    
  # Plot
  plot1 <- ggplot(data = simulatedVaR, aes(x = Portfolio))+
    geom_histogram(aes(y = ..count..), bins = 20, fill = "darkgreen", color = "black", alpha = 0.7 )+
    scale_x_continuous(breaks = seq(70, 150, by = 10))+
    geom_text(stat='bin', aes(y = ..count.., label = ..count..), vjust = -2.5, bins = 20) + 
    labs(x = "Final Portfolio Value", y = "Count", title = names[1])
  
  plot2 <- ggplot(data = simulatedVaR, aes(x = InsuredPortfolio))+
    geom_histogram(aes(y = ..count..), bins = 20, fill = "darkgreen", color = "black", alpha = 0.7 )+
    scale_x_continuous(breaks = seq(80, max(simulatedVaR$InsuredPortfolio), by = 10),
                       limits = c(min(simulatedVaR$InsuredPortfolio), max(simulatedVaR$InsuredPortfolio)))+
    geom_text(stat='bin', aes(y = ..count.., label = ..count..), vjust = -2.5, bins = 20) + 
    labs(x = "Final Portfolio Value", y = "Count", title = names[2])

  print(plot1)
  print(plot2)
  return(VaR)
 
}
  
  #geom_text(stat='bin', aes(y = ..count.., label = ..count..), vjust = -2.5, bins = 20) + 
  #geom_text(stat='bin', aes(y = ..count.., label = scales::percent(..count../sum(..count..))), 


# Simulated returns
simulatedVaR <- compute_VaR( data = c(-30:30), names = c("VaR: Simulated Returns for $100 Dollars", "Insured Portfolio VaR: Simluated Returns  for $100 Dollars"))

# Annual returns
annual_returnsVaR <- compute_VaR(yearly_return$yearly.returns*100,
                                 names = c("VaR: Annual Returns for $100 Dollars", "Insured Portfolio VaR: Annual Returns for $100 Dollars"))
