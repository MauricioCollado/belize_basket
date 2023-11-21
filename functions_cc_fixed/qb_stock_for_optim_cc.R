qb_stock_for_optim <- function(mortality, species, tech, cost, baskets, years){

  
  # Make useful data frames of each of our lists: this allows any number to be added.
  sp_df <- as.data.frame(do.call(rbind, species))
  tech_df <- as.data.frame(do.call(rbind, tech))
  cost_df <- as.data.frame(do.call(rbind, cost))
  baskets_df <- as.data.frame(do.call(rbind, baskets))
  
  # Calculate number of tech and species
  ntech <- nrow(tech_df)
  nspecies <- nrow(sp_df)
  
  # Add starting stock values.
  stock_df <- data.frame(t(sp_df$X))
  
  # Make a few full data frames we can fill later
  h_df <- data.frame(matrix(ncol = nspecies))
  e_df <- data.frame(matrix(ncol = ntech))
  
  ## Profits:
  pft_df <- data.frame(matrix(ncol = ntech))
  rev_sp_df <- data.frame(matrix(ncol = nspecies))
  
  #WWWW
  # We still probably want to calculate harvest per species AND technology...
  #WWWW
  
  
  ### --------
  ### Unchanging Parameters
  ### --------
  
  # D is the cost matrix, where the diagonal values are the cost of each tech:
  D <- matrix(0, nrow = ntech, ncol = ntech)
  diag(D) <- 2*cost_df$cost
  
  # Z is the the tech matrix
  Z <- t(as.matrix(tech_df[,-1]))
  
  # P is the price matrix
  P <- as.matrix(sp_df$p)
  
  # M is whether a species is contained in a basket:
  M <- as.matrix(baskets_df)
  
  # We build a diagonal matrix (nxn) for the second constraint
  N <- matrix(0, nrow=ntech,ncol=ntech)
  diag(N) <- 1
  
  ### --------
  ### Changing Parameters
  ### --------
  
  for (i in 1:years){
    ### ------
    ### Calculate effort per year
    ### ------
    E <- NULL
    # B, where the diagonal corresponds to stock sizes (X) for each species
    B <- matrix(0, nrow = nspecies, ncol = nspecies)
    diag(B) <- as.numeric(tail(stock_df,1)) ### Stock goes here...
    # d combines the P,B,and Z matrices yielding the per tech revenue (1xtech)
    # (1xspecies)x(speciesxspecies)x(speciesxtech)
    d <- t(t(P)%*%B%*%Z)
    # A is the transpose matrix that defines which basket each species is in:
    A <- -1*M%*%B%*%Z
    # Add the second constraint N:
    A <- rbind(A, N)
    # Take the transpose for solve.QP:
    t_A <- t(A)
    # We need to calculate the basket caps based on the initial BASKET size(not stock size!):
    viz_stock <- rowSums(tail(stock_df,1)) ### And here...
    # Make the first constraint based on fixed quota:
    b <- -t(mortality)             #### Mortality needs to be input as a vector, eh??
    # Make a second constraint so they're all greater than zero:
    b2 <- matrix(0, nrow=ntech, ncol=1)
    # Bind them together
    b <- rbind(b, b2)
    # Find optimal efforts
    
    
    E <- solve.QP(D,d,t_A,b, meq=0)$solution # Vector where the position is the effort per tech.
    
    ##E <- solve.QP(na.omit(D),na.omit(d), na.omit(t_A), na.omit(b), meq=0)$solution
    
    ### ------
    ### Calculate harvest for each species for the year
    ### ------
    # Repeat E for each species, so we have effort per tech per species:
    h <- c(B%*%Z%*%E) # Position corresponds to the species:
    # Calculate stock growth with X(t+1) = Xt + Xt*r*(1-Xt/K) - h
    stock_next <- tail(stock_df,1) + (sp_df$r)*exp(-sp_df$tcoef*temp_anom[i])*tail(stock_df,1)*(1-tail(stock_df,1)/sp_df$K) - h
    
    stock_next <- pmax(stock_next,0)
    ### Calculate profit for each technology for the year
    ### -----
    
    # Rev per tech = revenue per unit effort of that tech x effort applied
    # (1xtech)*(techx1)
    rev_t <- t(d)*E
    
    # Cost per technology = cost per unit of effort for that tech * effort applied to that tech
    # Costs are available in the cost_df
    cpue <- cost_df$cost
    cost_t <- cpue*E
    
    # Profit per tech is the revenue - cost
    pft <- as.data.frame(rev_t-cost_t)
    names(pft) <- names(pft_df)
    
    ### Profit per species
    ### -----
    
    # Rev per species:
    rev_sp <- as.data.frame(h*t(P))
    names(rev_sp) <- names(rev_sp_df)
    
    # Add them to the corresponding data frames:
    stock_df <- rbind(stock_df, stock_next)
    h_df <- rbind(h_df, h)
    e_df <- rbind(e_df, E)
    pft_df <- rbind(pft_df, pft)
    rev_sp_df <- rbind(rev_sp_df, rev_sp)
    
  }
  
  # Make the output column names nice
  sp_names <- paste("s", sp_df$s, sep = "_")
  tech_names <- paste("t", tech_df$t, sep = "_")
  # Make yr vector:
  year <- seq(0, years, by = 1)
  # Add stock/tech names & years
  colnames(stock_df) <- sp_names
  colnames(h_df) <- sp_names
  colnames(rev_sp_df) <- sp_names
  
  colnames(e_df) <- tech_names
  colnames(pft_df) <- tech_names
  
  profit = -sum(colSums(pft_df, na.rm = TRUE))
  
  # Return output
  #return(list(stock = stock_df, harvest = h_df, effort = e_df, profit_per_t = pft_df, rev_per_sp= rev_sp_df))
  
  return(profit)
}