sum_func <- function(n){
  #Takes in a single numerical argument n. 
  #Outputs the sum of all those numbers strictly below n which are divisible by either 2 or 7 or both.
  
  stopifnot(is.numeric(n),n%%1==0,n>0) #check if n is positive integer
  
  sum <- c() #create list of numbers to sum

  for (i in 1:n-1){ #check for each integer if it divides by 2 or 7
    
    if (i%%2==0){
     
      sum[[length(sum)+1]] <- i #if it divides by 2 then appends to sum
    
    }else if (i%%7==0){
      
      sum[[length(sum)+1]] <- i #if it divides by 7 then appends to sum
    }
  }
  return(Reduce('+',sum)) #Return function sums each element in vector
}

print(sum_func(1000))