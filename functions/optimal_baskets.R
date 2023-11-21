optimal_baskets <- function(species, tech, cost, baskets, mort_guess, years){
  # Load the two functions
  source(here("functions", "qb_stock_m.R"))
  source(here("functions", "qb_stock_for_optim.R"))
  # Optimize over baskets:
  mortality <- optim(par = mort_guess, qb_stock_for_optim, method = c("L-BFGS-B"), lower = 0.1, upper = 0.9 ,species = species, tech = tech, cost = cost, baskets = baskets, years = years)
  # Run qb_stock for the optimal mortalities
  result <- c(qb_stock(species = species, tech = tech, cost = cost, baskets = baskets, mortality = mortality$par, years = years), mortality = mortality$par)
  
  return(result)
}