set.seed(0)
n <- 1000
#sample_X <- data.frame(U=runif(n)) %>% mutate(X=case_when((0<=U)&(U<0.25)~3,(0.25<=U)&(U<0.5)~10,(0.5<=U)&(U<=1)~0)) %>% pull()

sample_X_0310 <- function(alpha,beta,n){
  #inputs alpha, beta and n values
  #outputs X1,X2,...,Xn copies of X.
  
  X_idd <- data.frame(U=runif(n)) %>% mutate(X=case_when((0<=U)&(U<alpha)~3,(alpha<=U)&(U<alpha+beta)~10,(alpha+beta<=U)&(U<=1)~0)) %>% pull()
  return(X_idd)
}

sample_X_0310(0.25,0.25,1000) #%>% print()

#
#find sample average 
#compare with E(X)
alpha <- 0.25
beta <- 0.1
n <- 10000

X_sample2 <- sample_X_0310(alpha,beta,n)

#print('Sample Average')
#mean(X_sample2) %>% print()

#print("Expextation")
#print(3*alpha+10*beta)

#sample var. compare with pop
#print('Sample variance')
#var(X_sample2) %>% print()

#print("calc variance")
#print(9*alpha+100*beta-9*alpha**2-100*beta**2-60*alpha*beta)

#Create data frame
#4 columns beta , sample_X, sample mean, expectation
alpha <- 0.1
n <- 100
beta <- seq(0,0.9,0.01)

beta_frame <- data.frame(Beta=beta) %>% mutate(Sample_X=map2(alpha,Beta,~sample_X_0310,n))

a <- beta_frame %>% filter(Beta==0.5) %>% pull(Sample_X)
mean(a) #%>% print()

#beta_frame %>% mutate(Sample_mean=map(beta_frame$Sample_X,~mean)) %>% print()







#1.2
#2

my_cdf_exp <- function(x,lambda){
  #outputs cumulative distribution function for exponential
  
  if(x<0){
    return(0)
  }else{
    return(1-exp(-lambda*x))
  }
}

lambda <- 1/2
map_dbl(.x=seq(-1,4), .f=~my_cdf_exp(x=.x,lambda=lambda) ) #%>% print()

##3

my_quantile_exp <- function(p,lambda){
  #outputs quantile function for exponential
  
  #return((ln(1-p))/(-lambda))
}

#my_quantile_exp(0.01,0.5) %>% print()



#1.3
#Q2
#create data frame with columns: x with [0,50] and binom pmf for corresponding x

n <- 50
p <- 0.7

binom_df <- data.frame(x=seq(0,50)) %>% mutate(pmf=dbinom(x,size=n,prob=p)) %>% head(3) #%>% print()


##Q3
#create gaussian data frame with columns: x with [0,50] in increments of 0.01 and gaussian pdf for corresponding x

mu <- 50*0.7
sigma <- sqrt(50*0.7*0.3)

norm_df <- data.frame(x=seq(0,50,0.01)) %>% mutate(pdf=dnorm(x,mean=mu,sd=sigma)) %>% head(3) #%>% print()


##Q4
#No idea why this won't create a graph

colors<-c("Gaussian pdf"="red", "Binomial pmf"="blue")
fill<-c("Gaussian pdf"="white", "Binomial pmf"="white")
plot1 <- ggplot() + labs(x="x",y="Probability") + theme_bw() +
  # create plot of Gaussian density
  geom_line(data=norm_df, aes(x,y=pdf,color="Gaussian pdf"),size=2) +
  # create a bar chart from PMF of Binomial distribution
  geom_col(data=binom_df, aes(x,y=pmf, color="Binomial pmf",fill="Binomial pmf")) +
  # set color
  scale_color_manual(name = "myLegend", values=colors) +
  scale_fill_manual(name = "myLegend", values=fill) +
  xlim(c(20,50))
#plot1 %>% print()


#1.4 Q1
x <- seq(-10,10,0.01)
gauss1 <- dnorm(x,mean=1,sd=1)
gauss2 <- dnorm(x,mean=1,sd=2)
gauss3 <- dnorm(x,mean=1,sd=3)
gauss_df <- data.frame(x,gauss1,gauss2,gauss3) #%>% print()

colors2 <- c("1"="red", "2"="blue", "3"="green")
plot2 <- ggplot() + labs(x="x",y="Density") + theme_bw() +
  geom_line(data=gauss_df,aes(x=x,y=gauss1,color="1")) +
  geom_line(data=gauss_df,aes(x=x,y=gauss2,color="2")) +
  geom_line(data=gauss_df,aes(x=x,y=gauss3,color="3")) +
  scale_color_manual(name = "Variance", values=colors2)

plot2 #%>% print()

#Q2
#same but do cumulative distribution
x <- seq(-10,10,0.01)
gauss_c1 <- pnorm(x,mean=1,sd=1)
gauss_c2 <- pnorm(x,mean=1,sd=2)
gauss_c3 <- pnorm(x,mean=1,sd=3)
cumulative_df <- data.frame(x,gauss_c1,gauss_c2,gauss_c3) #%>% print()

colors2 <- c("1"="red", "2"="blue", "3"="green")
plot3 <- ggplot() + labs(x="x",y="Density") + theme_bw() +
  geom_line(data=cumulative_df,aes(x=x,y=gauss_c1,color="1")) +
  geom_line(data=cumulative_df,aes(x=x,y=gauss_c2,color="2")) +
  geom_line(data=cumulative_df,aes(x=x,y=gauss_c3,color="3")) +
  scale_color_manual(name = "Variance", values=colors2)

plot3 #%>% print()


#Q3
#same but do quantile distribution
x <- seq(-10,10,0.01)
gauss_q1 <- qnorm(x,mean=1,sd=1)
gauss_q2 <- qnorm(x,mean=1,sd=2)
gauss_q3 <- qnorm(x,mean=1,sd=3)
quantile_df <- data.frame(x,gauss_q1,gauss_q2,gauss_q3) #%>% print()

colors2 <- c("1"="red", "2"="blue", "3"="green")
plot3 <- ggplot() + labs(x="x",y="Density") + theme_bw() +
  geom_line(data=quantile_df,aes(x=x,y=gauss_q1,color="1")) +
  geom_line(data=quantile_df,aes(x=x,y=gauss_q2,color="2")) +
  geom_line(data=quantile_df,aes(x=x,y=gauss_q3,color="3")) +
  scale_color_manual(name = "Variance", values=colors2) +
  xlim(c(0,1))

plot3 #%>% print()


#Q4
#Now do random gaussian distribution
set.seed(1)
n <- 100
#x <- seq(-10,10,0.01)

standardGaussianExample <- rnorm(n,mean=0,sd=1) #%>% print()
random_df <- data_frame(x,standardGaussianExample)


#Q5











