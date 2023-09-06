#Statistical estimation using Cauchy Distribution

#simulation 1

set.seed(0)
num_trials_per_sample_size <- 10
max_sample_size <- 10000
theta <- 1 #true parameter theta

#Create a data frame containing all pairs of sample_size and trial
df <- crossing(trial=seq(num_trials_per_sample_size),sample_size=seq(to=sqrt(max_sample_size),by=0.1)**2)

#for each pair, simulate a sequence of gaussian random variables
df <- mutate(df,simulation=pmap(.l=list(trial,sample_size),.f=~rcauchy(.y,location=theta)))

#Compute the sample variance of each sequence
sim_by_n_df <- mutate(df,sample_mean=map_dbl(.x=simulation,.f=mean))

#Scatter plot of sample mean (for different sample sizes)
plot_obj <- ggplot() + labs(x='Sample size',y='Mean') + theme_bw() + geom_point(data=sim_by_n_df,aes(x=sample_size,y=sample_mean,color='Sample',linetype='Sample'),size=0.1) 

#Horizontal line displaying population variance
plot_obj <- plot_obj +geom_hline(aes(yintercept=theta,color='Population',linetype='Population'),size=1) + ylim(c(-10,10))

#add legends
plot_obj + scale_color_manual(name='Legend',values=c('Sample'='blue','Population'='red')) + scale_linetype_manual(name='Legend',values=c('Sample'='dashed','Population'='solid')) + scale_x_sqrt()
#plot_obj %>% print()


#simulation 2

set.seed(0)
num_trials_per_sample_size <- 10
max_sample_size <- 10000
theta <- 1 #true parameter theta

#Create a data frame containing all pairs of sample_size and trial
df <- crossing(trial=seq(num_trials_per_sample_size),sample_size=seq(to=sqrt(max_sample_size),by=0.1)**2)

#for each pair, simulate a sequence of gaussian random variables
df <- mutate(df,simulation=pmap(.l=list(trial,sample_size),.f=~rcauchy(.y,location=theta)))

#Compute the sample variance of each sequence
sim_by_n_df <- mutate(df,sample_median=map_dbl(.x=simulation,.f=median))

#Scatter plot of sample mean (for different sample sizes)
plot_obj <- ggplot() + labs(x='Sample size',y='Median') + theme_bw() + geom_point(data=sim_by_n_df,aes(x=sample_size,y=sample_median,color='Sample',linetype='Sample'),size=0.1) 

#Horizontal line displaying population variance
plot_obj <- plot_obj +geom_hline(aes(yintercept=theta,color='Population',linetype='Population'),size=1) + ylim(c(-10,10))

#add legends
plot_obj + scale_color_manual(name='Legend',values=c('Sample'='blue','Population'='red')) + scale_linetype_manual(name='Legend',values=c('Sample'='dashed','Population'='solid')) + scale_x_sqrt()
plot_obj %>% print()