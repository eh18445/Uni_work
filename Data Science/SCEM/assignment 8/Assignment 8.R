
#2 Q1 
# Do t-test on Barley data

alpha <- 0.01
pair_test <- t.test(x=Barley$Glabron,y=Barley$Velvet,paired=TRUE)

#Q2 use cohen's d to compute effect size

diff <- abs(Barley$Glabron-Barley$Velvet)
effect_size <- mean(diff)/sd(diff)
effect_size #%>% print()

#Q3 Assumptions are: That the diff can be modelled as a gaussian distribution

Barley_diff <- Barley %>% mutate(diff=abs(Barley$Glabron-Barley$Velvet))
plot_1 <- ggplot() + geom_density(data=Barley_diff,aes(x=diff)) + xlab("Difference") + ylab("Density") + theme_bw()
plot_1 #%>% print()
plot_2 <- ggplot() + geom_density(data=Barley_diff,aes(x=Glabron)) + xlab("Glabron") + ylab("Density") + theme_bw()
plot_2 #%>% print()
plot_3 <- ggplot() + geom_density(data=Barley_diff,aes(x=Velvet)) + xlab("Velvet") + ylab("Density") + theme_bw()
plot_3 #%>% print()

#Looking at these graphs each of Glabron and Velvet could be modelled as a gaussian but the diference doesn't appear very gaussain at all.
#So I don't think that the paired t-test is a good choice.

#Aim in part 3 is to create a function that performs an unpaired student t-test.
peng_AC <- penguins %>% drop_na(species,body_mass_g) %>% filter(species!="Gentoo")

#Q1 understand what the following code does
val_col <- "body_mass_g"
group_col <- "species"
data <- peng_AC
data_new <- data %>%
  # rename the columns; note that you can not drop the "!!" (why?)
  rename(group=(!!group_col),val=(!!val_col))%>%
  group_by(group) %>%
  drop_na(val) %>%
  summarise(mn=mean(val))
#data_new$mn [2] %>% print()

#Create function
t_test_function <- function(data,val_col,group_col){
  #Input data=dataframe, val_col=string name of column with continuous values, group_col=string name of binary column
  #aim: Do student t test on val_col
  
  #1) Partition into 2 groups
  data_new <- data %>%
    rename(group=(!!group_col),val=(!!val_col))%>%
    group_by(group) %>%
    drop_na(val)
  
  #2) compute sample mean, variance and size for each group
  data_new <- data_new %>% summarise(mn=mean(val),var=var(val),size=length(val)) #%>% print()
  
  #3) Compute test statistic, p-value and effect size
  mean_1 <- data_new$mn[1]
  mean_2 <- data_new$mn[2]
  
  var_1 <- data_new$var[1]
  var_2 <- data_new$var[2]
  
  n_1 <- data_new$size[1]
  n_2 <- data_new$size[2]
  
  sd_combined <- sqrt(((n_1-1)*var_1+(n_2-1)*var_2)/(n_1+n_2-2))
  t_statistic <- (mean_1-mean_2)/(sd_combined*sqrt(1/n_1+1/n_2))
  
  p_value <- 2*(1-pt(abs(t_statistic),df=n_1+n_2-2))
  
  effect_size <- (mean_1-mean_2)/sd_combined
  
  #4) Return a data frame containing the test statistic, p-value and effect size
  df_result <- data.frame(t_stat=t_statistic,p_val=p_value,effect_size=effect_size)
  
  return(df_result)
}

t_test_function(data=peng_AC,val_col="body_mass_g",group_col="species") #%>% print()



#5 Investigating size of student's unpaired t-test
num_trials <- 10000
sample_size <- 30
mu_0 <- 1
mu_1 <- 1
sigma_0 <- 3
sigma_1 <- 3
alpha <- 0.05
set.seed(0) # set random seed for reproducibility
single_alpha_test_size_simulation_df <- data.frame(trial=seq(num_trials)) %>%
  # generate random Gaussian samples
  mutate(sample_0=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_0,sd=sigma_0)),
         sample_1=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_1,sd=sigma_1))) %>%
  # generate p values
  mutate(p_value=pmap(.l=list(trial,sample_0,sample_1),
                      .f=~t.test(..2,..3,var.equal = TRUE)$p.value))%>%
  # type I error
  mutate(type_1_error=p_value<alpha) #%>% head(5) %>% print()
single_alpha_test_size_simulation_df %>%
  pull(type_1_error) %>%
  mean() # estimate of coverage probability

#Modify the above code to explore how the size of the test varies as a function of the significance level α. 
#You might want to use visualization
#vary alpha
#plot mean type_1_error for each alpha
num_trials <- 10000
sample_size <- 30
mu_0 <- 1
mu_1 <- 1
sigma_0 <- 3
sigma_1 <- 3
alpha <- seq(0.01,0.1,0.01)
set.seed(0) # set random seed for reproducibility

alpha_test_size_simulation_df <- crossing(alpha=alpha,trial=seq(num_trials)) %>% 
  mutate(sample_0=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_0,sd=sigma_0)),
         sample_1=map(.x=trial,.f=~rnorm(n=sample_size,mean=mu_1,sd=sigma_1))) %>%
  # generate p values
  mutate(p_value=pmap(.l=list(trial,sample_0,sample_1),
                      .f=~t.test(..2,..3,var.equal = TRUE)$p.value)) %>%
  # type I error
  mutate(type_1_error=p_value<alpha) %>% 
  group_by(alpha) %>% summarise(mean=mean(type_1_error)) #%>% print()

plot_4 <- ggplot() + geom_line(data=alpha_test_size_simulation_df,aes(x=alpha,y=mean)) + xlab("alpha") + ylab("mean") + theme_bw()
#plot_4 %>% print()

#Statistical power of unpaired t-test
num_trials<-10000
n_0<-30
n_1<-30
mu_0<-3
mu_1<-4
sigma_0<-2
sigma_1<-2
alpha<-0.05
set.seed(0) # set random seed for reproducibility
data.frame(trial=seq(num_trials)) %>%
  # generate random Gaussian samples
  mutate(sample_0 = map(.x=trial,.f =~ rnorm(n=n_0,mean=mu_0,sd=sigma_0)),
         sample_1 = map(.x=trial,.f =~ rnorm(n=n_1,mean=mu_1,sd=sigma_1))) %>%
  # for each sample, generate p value; check examples of pmap() with ?map
  mutate(p_value=pmap(.l = list(trial,sample_0,sample_1),
                      .f =~ t.test(..2, ..3, var.equal = TRUE)$p.value)) %>%
  # estimate of coverage probability
  mutate(reject_null = p_value<alpha ) %>%
  # extract a column
  pull(reject_null) %>%
  # compute probability
  mean() #%>% print()

#Q1) Conduct a simulation study to explore how the statistical power varies as a function of the significance level
num_trials <- 10000
n_0 <- 30
n_1 <- 30
mu_0 <- 3
mu_1 <- 4
sigma_0 <- 2
sigma_1 <- 2
alpha <- seq(0.01,0.1,0.01)
set.seed(0) # set random seed for reproducibility
power_alpha_df <- crossing(alpha=alpha,trial=seq(num_trials)) %>%
  # generate random Gaussian samples
  mutate(sample_0 = map(.x=trial,.f =~ rnorm(n=n_0,mean=mu_0,sd=sigma_0)),
         sample_1 = map(.x=trial,.f =~ rnorm(n=n_1,mean=mu_1,sd=sigma_1))) %>%
  # for each sample, generate p value; check examples of pmap() with ?map
  mutate(p_value=pmap(.l = list(trial,sample_0,sample_1),
                      .f =~ t.test(..2, ..3, var.equal = TRUE)$p.value)) %>%
  # estimate of coverage probability
  mutate(reject_null = p_value<alpha ) %>%
  
  group_by(alpha) %>% summarise(mean=mean(reject_null)) #%>% print()

plot_5 <- ggplot() + geom_line(data=power_alpha_df,aes(x=alpha,y=mean)) + xlab("alpha") + ylab("mean") + theme_bw()
#plot_5 %>% print()


#Q2) Conduct a simulation study to explore how the statistical power varies as a function of the difference in means µ1 − µ0.
num_trials <- 10000
n_0 <- 30
n_1 <- 30
mu_0 <- 3
mu_1 <- seq(1,10)
sigma_0 <- 2
sigma_1 <- 2
alpha <- 0.05
set.seed(0) # set random seed for reproducibility
power_mu_df <- crossing(mu_1=mu_1,trial=seq(num_trials)) %>%
  mutate(mu_diff=map(.x=mu_1,.f=~abs(.x-mu_0))) %>% as.data.frame() %>%
  # generate random Gaussian samples
  mutate(sample_0 = map(.x=trial,.f =~ rnorm(n=n_0,mean=mu_0,sd=sigma_0)),
         sample_1 = map(.x=trial,.y=mu_1,.f =~ rnorm(n=n_1,mean=.y,sd=sigma_1))) %>%
  # for each sample, generate p value; check examples of pmap() with ?map
  mutate(p_value=pmap(.l = list(trial,sample_0,sample_1),
                      .f =~ t.test(..2, ..3, var.equal = TRUE)$p.value)) %>%
  # estimate of coverage probability
  mutate(reject_null = p_value<alpha ) %>%
  
  group_by(mu_diff) %>% summarise(mean=mean(reject_null)) %>% print()

plot_6 <- ggplot() + geom_line(data=power_mu_df,aes(x=mu_diff,y=mean)) + xlab("mu differernce") + ylab("mean") + theme_bw()
plot_6 %>% print()

