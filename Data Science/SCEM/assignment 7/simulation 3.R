
set.seed(0)
sample_size <- 100
theta_0 <- 5

#1) Generate sample of a sequence of Cauchy random variables
cauchy_sample <- rcauchy(n=sample_size,location=theta_0)

#2) The log likelihood function
log_lik_cauchy <- function(theta,sample_X){
  return(-sum(log(1+(sample_X-theta)**2)))
}
log_lik_cauchy_X <- function(theta){
  return(log_lik_cauchy(theta,cauchy_sample))
}

#3) Optimise the log likelihood function
#optimise functon searches interval for a minimum of maximum of selected function
theta_ml_est <- optimise(f=log_lik_cauchy_X,interval=c(-1000,1000),maximum=TRUE)$maximum
#theta_ml_est %>% print()


#Next conduct simulation to study the distribution of MLE using 100000 trials
set.seed(0)
num_trials <- 100000
sample_size <- 100
theta_0 <- 5 #true parameter theta

#1) log likelihood function
#alteady done above

#2) Mapping a sample to MLE of theta
theta_ml <- function(theta){
  log_lik_cauchy_X <- function(theta){
    return(log_lik_cauchy(theta,sample_X))
  }
  theta_ml_est <- optimise(f=log_lik_cauchy_X,interval=c(-10,18),maximum=TRUE)$maximum
  return(theta_ml_est)
}

#3.1) Create num_trials samples; the size of each sample is sample_size
df <- data.frame(trial=seq(num_trials)) %>% mutate(sample=map(.x=trial,~rcauchy(sample_size,location=theta_0)))

#3.2) For each sample, compute MLE and median estimate
cauchy_simulation_df <- mutate(df,ml_est=map_dbl(.x=sample,.f=theta_ml)) %>% mutate(med_est=map_dbl(.x=sample,.f=median))

#4) Pivot
plot_df <- cauchy_simulation_df %>% pivot_longer(cols=c(ml_est,med_est)) %>% mutate(name=map_chr(.x=name,~case_when(.x=="med_est"~"Median",.x=="ml_est"~"Maximum likelihood")))

#5) Create plot
plot1 <- ggplot(plot_df,mapping=aes(x=value,color=name,linetype=name)) + geom_density() + theme_bw() + xlim(c(4,6)) + labs(color="",linetype="") + xlab("Estimate") + ylab("Density")
plot1 %>% print()















