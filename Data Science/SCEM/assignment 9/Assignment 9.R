#2
#Q1)
#create one_sample_chi_square_test

one_sample_chi_square_test <- function(sample,sigma_square_null){
  sample <- sample[!is.na(sample)]
  n <- length(sample)
  chi_squared_statistic <- (n-1)*var(sample)/sigma_square_null
  p_value <- 2*min(pchisq(chi_squared_statistic,df=n-1),1-pchisq(chi_squared_statistic,df=n-1))
  return(p_value)
}

#Q2)
#Conduct a simulation study to see how the size of the test varies as a function of the significance level. You
#can consider a sample size of 100, µ = 1, σ2 = 4.
#For example, you can create a plot similar to the one below

#test size is P(type I error|H0 is true)
set.seed(0)
mu <- 4
sigma_square <- 4
sample_size <- 1000
num_tests <- 10000
alpha_list <- seq(0,0.2,0.01)

single_alpha_df <- data.frame(seq(num_tests)) %>% 
  mutate(sample=map(.x=sample_size,.f=~rnorm(.x,mean=mu,sd=sqrt(sigma_square)))) %>%
  mutate(p_value=map(.x=sample,.f=~one_sample_chi_square_test(.x,sigma_square)))

compute_test_size <- function(alpha){
  type_1_error = single_alpha_df$p_value<alpha
  return (mean(type_1_error))
}

multiple_alpha_df <- data.frame(alpha=alpha_list) %>%
  mutate(test_size=map_dbl(alpha,compute_test_size)) #%>% print()

plot1 <- ggplot(data=multiple_alpha_df,aes(x=alpha,y=test_size)) +
  geom_point() + 
  xlab('alpha') + 
  ylab('test size') +
  theme_bw()

#plot1 %>% print()
  
  
#3
#Suppose you want to build a classifier to predict whether a hawk belongs to either the “Sharp-shinned” or
#the “Cooper’s” species of hawks. The feature vector will be a four-dimensional row vector containing the
#weight, and the lengths of the wing, the tail and the hallux. The labels will be binary: 1 if the hawk is
#“Sharp-shinned” and 0 if the hawk belongs to “Cooper’s” species.

#Q1)
#columns Weight,Wing,Hallux,Tail,Species(which is 0 for CH and 1 for SS).

Species_boolean <- data.frame(Species=c("SS","CH"),binary=c(1,0))

hawks_total <- Hawks %>% 
  filter(Species=='CH'|Species=='SS') %>%
  select(Weight,Wing,Hallux,Tail,Species) %>%
  drop_na() %>%
  left_join(Species_boolean) %>%
  select(-Species) %>%
  rename(Species=binary)

#Q2)
#Now implement a train test split for your “hawks_total” data frame. You should use 60% of your data
#within your training data and 40% in your test data. You should create a data frame consisting of training
#data called “hawks_train” and a data frame consisting of test data called “hawks_test”. Display the number
#of rows in each data frame.

num_total <- nrow(hawks_total)
num_train <- floor(num_total*0.6)
num_test <- num_total-num_train

set.seed(1)
test_ind <- sample(seq(num_total),num_test) #random sample of test indicies
train_ind <- setdiff(seq(num_total),num_train) #remaining indicies make up training data

hawks_test <- hawks_total %>% filter(row_number()%in%test_ind)
hawks_train <- hawks_total %>% filter(row_number()%in%train_ind)

#nrow(hawks_train) %>% print()
#nrow(hawks_test) %>% print()

#Q3)
#Next extract a data frame called “hawks_train_x” from your training data (from “hawks_train”) containing
#the feature vectors and no labels. In addition, extract a vector called “hawks_train_y” consisting of
#labels from your training data. Similarly, create data frames called “hawks_test_x” and “hawks_test_y”
#corresponding to the feature vectors and labels within the test set, respectively.

#The labels in this case is the species
hawks_train_x <- hawks_train %>% select(Species)
hawks_train_y <- hawks_train %>% select(-Species)
  
hawks_test_x <- hawks_test %>% select(Species)
hawks_test_y <- hawks_test %>% select(-Species)

#Q4)
#use classifier of the form phi(x)=y Choose a value of y e{0,1} that minimises training error

#training error sum of labels that don't match /n on training set

classifier1 <- rep(1,nrow(hawks_train_y))

training_error <- function(training_y,classifier){
  error <- 0
  for(i in 1:length(training_y)){
    if(classifier[i]!=training_y[i]){
      error <- error+1
    }
  }
  return(error/length(training_y))
}

training_error(hawks_train_y,classifier1) %>% print()










