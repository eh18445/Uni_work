#X1,...,Xn ~ N(mu0,sigma0^2) are iid gaussian random variables
#unknown parameters mu0, sigma0

mu <- 1 #choose a mean
sigma <- 2 #choose a standard deviation


#1) generate some x indicies
x <- seq(mu-3*sigma,mu+3*sigma,sigma*0.01)

#2) Data frame with population density
df_gaussian <- data.frame(x,Density=dnorm(x,mean=mu,sd=sigma),Source="population") #%>% print()

#3) Plot the density function
plot1 <- df_gaussian %>% ggplot(aes(x=x,y=Density,color=Source)) + geom_line() + ylab("Density function") + theme_bw() 
#plot1 %>% print()

#4) Genereate a sample of gaussian random variables
set.seed(123)
sample_size <- 100
sample_data <- rnorm(sample_size,mu,sigma)

#5) MLE of mu and sigma
mu_mle <- mean(sample_data)
sigma_mle <- sd(sample_data)*sqrt((sample_size-1)/sample_size)

#6) Add estimate density function to df
df_gaussian <- df_gaussian %>% rbind(data.frame(x,Density=dnorm(x,mean=mu_mle,sd=sigma_mle),Source="MLE estimate"))

#7) Plot the true and estimated density functions
plot2 <- df_gaussian %>% ggplot(aes(x=x,y=Density,color=Source)) + geom_line() + ylab("Density function") + theme_bw()
#plot2 %>% print()



