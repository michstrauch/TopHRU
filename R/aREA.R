# Calculation of the average Relative Error of Aggregation as a sum of the
# absolute contributions of the individual deviations.
aREA <- function(x){
  y <- sum(abs(x))
  return(y)
}
