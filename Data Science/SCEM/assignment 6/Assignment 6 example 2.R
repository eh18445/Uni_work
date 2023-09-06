#Statistical estimation using a gaussian distribution

#simulation1

set.seed(0)
num_trials <- 1000
sample_size <- 30
mu <- 1 #true mu
sigma_sqr <- 3 #true sigma^2

#Create data frame (trial=1,2,...,num_trials)
df <- data.frame(trial=seq(num_trials))

#Generate num_trials samples with rnorm()
df <- mutate(df,simulation=map(.x=trial,.f=~rnorm(sample_size,mean=mu,sd=sqrt(sigma_sqr))))

#Compute the sample variances (estimated sigma^2)
simulation_df <- mutate(df,sample_var=map_dbl(.x=simulation,.f=var)) %>% print()

#kernel density plot of sample variances
plot_obj <- ggplot() + labs(x='Variance',y='Density') + theme_bw() + geom_density(data=simulation_df,aes(x=sample_var,color='Sample',linetype='Sample'))

#Vertical line displaying population mean
plot_obj <- plot_obj +geom_vline(aes(xintercept=sigma_sqr,color='Popuation',linetype='Population'))

#add legend
plot_obj + scale_color_manual(name='Legend',values=c('Sample'='red','Population'='blue')) + scale_linetype_manual(name='Legend',values=c('Sample'='solid','Population'='dashed')) 
#plot_obj %>% print()


#simulation 2
set.seed(0)
num_trials_per_sample_size <- 10
max_sample_size <- 10000
mu <- 1 #True parameter mu
sigma_sqr <- 3 #sigma^2

#Create a data frame containing all pairs of sample_size and trial
df <- crossing(trial=seq(num_trials_per_sample_size),sample_size=seq(to=sqrt(max_sample_size),by=0.1)**2)

#for each pair, simulate a sequence of gaussian random variables
df <- mutate(df,simulation=pmap(.l=list(trial,sample_size),.f=~rnorm(.y,mean=mu,sd=sqrt(sigma_sqr))))

#Compute the sample variance of each sequence
sim_by_n_df <- mutate(df,sample_var=map_dbl(.x=simulation,.f=var))

#Scatter plot of sample varianace (for different sample sizes)
plot_obj <- ggplot() + labs(x='Sample size',y='Variance') + theme_bw() + geom_point(data=sim_by_n_df,aes(x=sample_size,y=sample_var,color='Sample',linetype='Sample'),size=0.1)

#Horizontal line displaying population variance
plot_obj <- plot_obj +geom_hline(aes(yintercept=sigma_sqr,color='Population',linetype='Population'),size=1)

#add legends
plot_obj + scale_color_manual(name='Legend',values=c('Sample'='blue','Population'='red')) + scale_linetype_manual(name='Legend',values=c('Sample'='dashed','Population'='solid')) + scale_x_sqrt()
plot_obj %>% print()






