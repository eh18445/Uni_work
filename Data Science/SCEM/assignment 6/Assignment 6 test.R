num_trials <- 1000000 #number of trials
set.seed(0) #set the random seed
sample_size <- 2 #set the sample size

#simulate average of a dice roll
#add columns dice_sample (1..6) and sample_avg (sample average)
dice_sample_average_simulation <- data.frame(trial=1:num_trials) %>% mutate(dice_sample=map(.x=trial,~sample(6,sample_size,replace=TRUE))) %>% mutate(sample_avg=map_dbl(.x=dice_sample,~mean(.x))) %>% print()

#plot to histogram
dice_sample_average_simulation %>% ggplot(aes(x=sample_avg)) + geom_histogram(aes(y=..count../sum(..count..)),binwidth=1/sample_size,fill='blue',color='blue') + theme_bw() + xlim(c(1,6)) + xlab('Sample Average') + ylab('Proportion') %>% print()


