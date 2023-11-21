optimal_QB_cap <- function(species, tech, cost, baskets, cap_guess, years){
  # Load the two functions
  source(here("working_model", "functions", "qb_stock_m.R"))
  source(here("working_model", "functions", "qb_stock_for_optim.R"))
  # find the optimal mortaities:
  QBcap <- optim(par = cap_guess, qb_stock_for_optim, method = c("L-BFGS-B"), lower = 0.1, upper = 0.9 ,species = species, tech = tech, cost = cost, baskets = baskets, years = years)
  
  return(QBcap)
}