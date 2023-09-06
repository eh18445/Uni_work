
num_trials_per_sample_size <- 100
min_sample_size <- 100
sample_size_inc <- 1000
max_sample_size <- 30000

mu_0 <- 10
mu_1 <- 10.05

sigma <- 1

set.seed(0)
crossing(trial=seq(num_trials_per_sample_size),
         sample_size=seq(min_sample_size,
                         max_sample_size,sample_size_inc)) %>%
  mutate(sample_0=pmap(.l=list(trial,sample_size),
                       .f=~rnorm(n=..2,mean=mu_0,sd=sigma))) %>%
  mutate(sample_1=pmap(.l=list(trial,sample_size),
                       .f=~rnorm(n=..2,mean=mu_1,sd=sigma))) %>%
  mutate(p_value=pmap_dbl(.l=list(sample_0,sample_1),
                          .f=~(t.test(..1,..2,paired=TRUE)$p.value))) %>%
  group_by(sample_size) %>%
  ggplot()+geom_smooth(aes(x=sample_size,y=p_value))+theme_bw()+
  xlab("Sample Size")+ylab("P value")+
  geom_hline(aes(yintercept=0.05),linetype="dashed",color="red") %>% plot()

