
# Define functions
compute_VaR <- function(data) {
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
    geom_histogram(aes(y = ..count..), bins = 30, fill = "darkgreen", color = "black", alpha = 0.7 )+
    scale_x_continuous(breaks = seq(70, 150, by = 10))+
    labs(x = "Final Portfolio Value", y = "Count", title = "VaR")
  
  plot2 <- ggplot(data = simulatedVaR, aes(x = InsuredPortfolio))+
    geom_histogram(aes(y = ..count..), bins = 20, fill = "darkgreen", color = "black", alpha = 0.7 )+
    scale_x_continuous(breaks = seq(80, max(simulatedVaR$InsuredPortfolio), by = 10),
                       limits = c(min(simulatedVaR$InsuredPortfolio), max(simulatedVaR$InsuredPortfolio)))+
    labs(x = "Final Portfolio Value", y = "Count", title = "VaR")

  print(plot1)
  print(plot2)
  return(VaR)
 
}
  


# Simulated returns
simulatedVaR <- compute_VaR( data = c(-30:30))

# Annual returns
annual_returnsVaR <- compute_VaR(yearly_return$yearly.returns*100)
