
#Every red-tailed hawk with only weight, tail and wing columns

RedTailedDf <- Hawks %>% filter(Species=="RT") %>% select(Weight,Tail,Wing) #%>% head(5) %>% print()

#1.1 Q2
#apply maximum likelihood method (MLE) to find mu_MLE and sigma_MLE which are estimates for mu_0 and sigma_0 for the tail values
wing_df <- RedTailedDf %>% pull(Wing)

n <- length(wing_df)
mu_mle <- mean(wing_df,na.rm=TRUE)
sigma_mle <- sd(wing_df,na.rm=TRUE)*sqrt(abs((1-n)/n)) #%>% na.omit()

#indicies
wings <- seq(mu_mle-3*sigma_mle,mu_mle+3*sigma_mle,sigma_mle*0.001) 

#MLE density function
colors <- c("MLE density"="red","Kernel density"="blue") #set color legend
estimated_density <- data.frame(Wing=wings,Density=dnorm(wings,mean=mu_mle,sd=sigma_mle))
plot_obj <- ggplot() + geom_line(data=estimated_density,aes(x=wings,y=Density,color="MLE density"))

#kernel density
plot_obj <- plot_obj + geom_density(data=tibble(wing_df),aes(x=wing_df,color="Kernel density")) + labs(y="Density function",color="Estimator") + theme_bw() + scale_color_manual(values=colors) + xlim(c(100,500))
plot_obj #%>% print()

#1.2 Q1
set.seed(0)
n <- 1000 #num of trials
sample_size <- seq(5,100,5)
mu0 <- 1 #true mean
sigma0 <- 3 #true sigma

df <- data.frame(sample_size)

#df_2 <- data.frame(trial=seq(1,n),sample_size) %>% mutate(sample_size=map(.x=trial,.f=seq(5,100,5)*)) %>% print()


#df_trials <- mutate(df_trials,simulation=map(.x=trials,.f=~rnorm(.x,mean=mu0,sd=sigma0)))


df <- mutate(df,simulation=map(.x=sample_size,.f=~rnorm(.x,mean=mu0,sd=sigma0))) #compute gaussian samples

df <- mutate(df,V_mle=map_dbl(.x=simulation,.f=var)) %>% mutate(V_U=map_dbl(.x=simulation,.f=~var(.x)*(length(.x)/(length(.x)-1)))) #%>% print()

plot_2 <- ggplot() + geom_line(data=df,aes(x=sample_size,y=V_mle)) + geom_line(data=df,aes(x=sample_size,y=V_U)) + labs(y="Density",x="Sample Size") + theme_bw() #+ scale_color_manual(values=colors)
plot_2 %>% print()


#1.3 Q3
set.seed(0)
#n <- 1000 #num of trials
n <- 1000
lambda0 <- 0.5 #true lambda

poisson_df <- data.frame(sample_size2=seq(1,n)) %>% mutate(sample=map(.x=sample_size2,.f=~rpois(.x,lambda=lambda0))) %>% mutate(lambda_mle=map(.x=sample,.f=~mean(.x))) %>% head(5) %>% print()

plot_poisson <- ggplot() + geom_point(data=poisson_df,aes(x=sample_size2,y=lambda_mle)) + theme_bw()
plot_poisson %>% print()



