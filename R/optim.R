library(om)

mat <- function(unit, y){
  u <- unique(unit)
  empty <- matrix(NA, nrow = length(u), ncol = max(table(unit)))
  for(i in seq_along(u)){
    r <- y[unit == u[i]]
    empty[i, 1:length(r)] <- r
  }
  return(empty)
}
