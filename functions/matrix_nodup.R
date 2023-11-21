matrix_nodup <- function(nrow, ncol) {
  # All possible column combinations for a matrix of dimension dim
  # Here we assume two different values c(1, 0)
  
  require(gtools);
  columns <- t(permutations(n = 2, r = nrow, v = c(1, 0), repeats.allowed = TRUE));
  columns <- columns[, colSums(columns) == 1];
  
  # Construct all possible combinations of dim column vectors and
  # impose constraint that row and column sum >= 1
  ret <- lapply(as.data.frame(t(permutations(ncol(columns), ncol, repeats.allowed = TRUE))), function(x) {
    m.cand <- columns[, x];
    if (all(rowSums(m.cand) > 0) & all(colSums(m.cand) > 0)) m.cand else NULL;
  })
  ret <- Filter(Negate(is.null), ret);
  ret <- ret[1:(length(ret)/2)]
  # # Remove duplicated rows
  # ret_rows <- ret %>% 
  #   map_df(as_tibble)
  # ret_nodup <- ret_rows[(!duplicated(ret_rows)),]
  # # Return a nice vector:
  # odd <- ret_nodup[seq(2, nrow(ret_nodup), by = 2),]
  # even <- ret_nodup[seq(1, nrow(ret_nodup), by = 2),]
  # m_fx <- function(x, y){
  #   matrix(x, y)
  # }
  # m_fx(odd, even)
  
  
  return(ret);
}