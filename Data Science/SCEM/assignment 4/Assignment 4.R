prob_red_spheres <- function(z){
  Prob <- choose(22,z)*(0.3**z)*(0.7**(22-z))
  return(Prob)
}
#print(prob_red_spheres(10))

num_reds <- c(seq(22))
prob <- prob_red_spheres(num_reds)
prob_by_nums_red <- data.frame(num_reds,prob)
prob_by_nums_red %>% head(5) #%>% print()


reds_plot <- ggplot(data=prob_by_nums_red,aes(x=num_reds,y=prob)) + geom_line() + xlab('Number of Reds')+ ylab('Probability')
#reds_plot %>% plot()

#sample with replacement
#produces a random number between 1 to 10. 22 times.
sample(10,22,replace=TRUE) #%>% print()

#use a set seed to get the same sample every time
for (i in 1:5){
  set.seed(0)
  #print(sample(100,5,replace=FALSE))
}

#First set a random seed. Then create a data frame called sampling_with_replacement_simulation consisting of 
#two columns. The first is called trial and contains numbers 1 through 1000. The second is called
#sample_balls and corresponds to random samples of size 22 from a bag of size 10, with replacement

num_trials <- 1000 # set the number of trials
set.seed(0) # set the random seed
sampling_with_replacement_simulation <- data.frame(trial=1:num_trials) %>% mutate(sample_balls=map(.x=trial,~sample(10,22, replace = TRUE)))

#Using mutate(), map_dbl and sum()
#add another column with the number of values less than/ equal to 3


sum_under_3 <- function(v){
  s <- 0
  for (i in 1:length(v)){
    if (v[i]<=3){
      s <- s + 1      
    }
  }
  return(s)
}
#c(1,5,2,4,7,8,5,21,5) %>% sum_under_3 %>% print()

sampling_with_replacement_simulation <- sampling_with_replacement_simulation %>% mutate(num_reds=map_dbl(.x=sample_balls,~sum_under_3(.x))) #%>% print()

#add new column predicted_prob 
num_reds_in_simulation <- sampling_with_replacement_simulation %>% select(num_reds)
#we extract a vector corresponding to the number of reds in each trial
prob_by_nums_reds <- prob_by_nums_red %>% mutate(predicted_prob=map_dbl(.x=num_reds,~sum(num_reds_in_simulation==.x))/num_trials) #%>% print()



#trying to make a graph of number of reds against probability for both theoretical and estimated
prob_by_num_reds %>% rename(TheoreticalProbability=prob,EstimatedProbability=predicted_prob) %>% pivot_longer(cols=c("EstimatedProbability","TheoreticalProbability"),names_to="Type",values_to="count") %>% ggplot(aes(num_reds,count)) + geom_line(aes(linetype=Type,color=Type)) + geom_point(aes(color=Type)) + scale_linetype_manual(values=c("solid","dashed")) + theme_bw() + xlab("Number of reds") + ylab("Probabilities") %>% print()








