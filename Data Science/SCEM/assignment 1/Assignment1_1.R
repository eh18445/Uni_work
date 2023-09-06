animals = c("Snake","Alligator","Ostrich","cow","Koala")
num_legs = c(0,4,2,4,2)

animals_df <- data.frame(animals,num_legs)

x_vect <- c(seq(12,2,-2))

X <- matrix(x_vect,2,3)

y_vect <- c(seq(1,4))

Y <- matrix(y_vect,2,2)

z_vect <- c(seq(4,10,2))

Z <- matrix(z_vect,2,2)


print(solve(Y)%*%X)

print(solve(Y,X))