# test

every_basket <- function(species, tech, cost, n_baskets, mort_guess, years, cores = 2){
  require(future)
  require(future.apply)
  require(quadprog)
  require(here)
  require(plyr)
  
  # Calculate every basket combo: matrix of length 
  bmtx <- matrix_nodup(n_baskets, length(species))
  
  # Make empty vectors:
  profit_end <- NULL
  stock_end <- NULL
  
  basket_fx <- function(basket_list){
    
    # Make the input a list of lists
    baskets <- split(basket_list, 1:nrow(basket_list))
    
    # Optimize over baskets:
    mortality <- optim(par = mort_guess, 
                       qb_stock_for_optim, method = c("L-BFGS-B"), lower = 0.1, 
                       upper = 0.9 ,species = species, tech = tech, cost = cost, baskets = baskets, years = years)
    # Run qb_stock for the optimal mortalities
    result <- c(qb_stock(species = species, tech = tech, 
                         cost = cost, baskets = baskets, mortality = mortality$par, 
                         years = years), mortality = mortality$par)
    # Calculate total profit, bind to vector)
    profit = sum(colSums(result$profit_per_t, na.rm = TRUE))
    end_biomass = as_vector(tail(result$stock, 1))
    return(list(result))
  }
  plan(multiprocess, workers = cores)
  result <- future.apply::future_lapply(bmtx, possibly(basket_fx, otherwise = NA))
  
  
  profit <- as_tibble(do.call(rbind, map(result, 1)))
  stock <- as_tibble(do.call(rbind, map(result, 2)))
  return(list(result, baskets = bmtx))
}