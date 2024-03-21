optimal_baskets <- function(species, tech, cost, baskets, mort_guess, years){
  # Load the two functions
  source(here("functions_cc_fixed", "qb_stock_m_cc_op1.R"))
  source(here("functions_cc_fixed", "qb_stock_for_optim_cc1.R"))
  # Optimize over baskets:
  mortality <- mort_guess
  # Run qb_stock for the optimal mortalities
  result <- c(qb_stock(species = species, tech = tech, cost = cost, baskets = baskets, years = years, mortality=mort_guess))
  
  return(result)
}