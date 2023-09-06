#Statistical estimation using Bernoulli distribution

#simulation 1
set.seed(0)
num_trials <- 1000
sample_size <- 30
q <- 0.3 #True parameter q

#create data frame (trial=1,2,..,trials)
df <- data.frame(trial=seq(num_trials))

#generate samples for Bernoulli random variables
df <- mutate(df,simulation=map(.x=trial,.f=~rbinom(sample_size,1,q)))

#compute the sample means
simulation_df <- mutate(df,sample_mean=map_dbl(.x=simulation,.f=mean)) #%>% print()

#kernel density plot of sample means
plot_obj <- ggplot() + labs(x='Mean',y='Density') + theme_bw() + geom_density(data=simulation_df,aes(x=sample_mean,color='Sample',linetype='Sample'))

#vertical line displaying population mean
plot_obj <- plot_obj + geom_vline(aes(xintercept=q,color='Population',linetype='Population'))

#legends
plot_obj + scale_color_manual(name='Legend',values=c('Sample'='red','Population'='blue')) + scale_linetype_manual(name='Legend',values=c('Sample'='solid','Population'='dashed')) 
#plot_obj %>% print()

#simulation 2
set.seed(0)
num_trials_per_sample_size <- 10
max_sample_size <- 10000
q <- 0.3 #True parameter q

#Create a data frame containing all pairs of sample_size and trial
df <- crossing(trial=seq(num_trials_per_sample_size),sample_size=seq(to=sqrt(max_sample_size),by=0.1)**2)

#for each pair, simulate a sequence of Bernoulli random variables
df <- mutate(df,simulation=pmap(.l=list(trial,sample_size),.f=~rbinom(.y,1,q)))

#Compute the sample mean of each sequence
sim_by_n_df <- mutate(df,sample_mean=map_dbl(.x=simulation,.f=mean))

#Scatter plot of sample means (for different sample sizes)
plot_obj <- ggplot() + labs(x='Sample size',y='Mean') + theme_bw() + geom_point(data=sim_by_n_df,aes(x=sample_size,y=sample_mean,color='Sample',linetype='Sample'),size=0.1)

#Horizontal line displaying population mean
plot_obj <- plot_obj +geom_hline(aes(yintercept=q,color='Population',linetype='Population'),size=1)

#add legends
plot_obj + scale_color_manual(name='Legend',values=c('Sample'='blue','Population'='red')) + scale_linetype_manual(name='Legend',values=c('Sample'='dashed','Population'='solid')) + scale_x_sqrt()
plot_obj %>% print()





