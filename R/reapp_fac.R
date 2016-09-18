# Generating factors by applying a threshold to a distribution. Different
# approaches are applied for the two different threshold types ("A" for
# Area and "P" for percentage).

reapp_fac <- function(x, thrs, thrs_type){
  fac <- x
  if(all(x == 0)){
    fac[] <- 0
    }
  else if (thrs_type == "A"){
    if(sum(x >= thrs) == 0){
      fac[which.max(x)] <- sum(x) / max(x)
      fac[-c(which.max(x))] <- 0
      }
    else{
      fac[x >= thrs] <- sum(x) / sum(x[x >= thrs])
      fac[x < thrs] <- 0
      }
    }
  else if(thrs_type == "P"){
    if(sum(x >= thrs * sum(x)) == 0){
      fac[which.max(x)] <- sum(x) / max(x)
      fac[-c(which.max(x))] <- 0
      }
    else{
      fac[x >= thrs * sum(x)] <- sum(x) / sum(x[x >= thrs * sum(x)])
      fac[x < thrs * sum(x)] <- 0
      }
    }

  return(fac)
  }
