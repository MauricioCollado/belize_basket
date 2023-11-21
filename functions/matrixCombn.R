# Calculate every basket combo:
matrixCombn <- function(nrow, ncol) {
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
  return(ret);
}