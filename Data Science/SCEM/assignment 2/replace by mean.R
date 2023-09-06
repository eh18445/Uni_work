
replace_by_mean <- function(x){
  mu <- mean(x,na.rm=TRUE) #compute mean of x
  
  impute_f <- function(z){ #imputation on a single element z
    if (is.na(z)){
      return(mu)
    }else{
      return(z)
    }
  }
  return(map_dbl(x,impute_f)) #apply the function to impute over whole vector x
}

x <- c(1,2,NA,4)
replace_by_mean(x) %>% print()