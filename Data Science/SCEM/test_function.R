x <- c(seq(0,20,0.01)) #create vector 0 to 20 incremented 0.01
y <- c(numeric(length(x))) #blank vector of same length of x

for (i in 1:length(x)){
  y[i] <- sin(x[i])
}

sin_df <- data.frame(x,y)
print(head(sin_df))
