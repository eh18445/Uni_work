#Make sure to use boot package

set.seed(123)
geyser = faithful #volcano data set

#1) Define a function that computes the median of a column of interest

compute_median <- function(df,indicies,col_name){
  sub_sample <- slice(df,indicies) %>% pull(all_of(col_name)) #extract a sub sample
  return(median(sub_sample,na.rm=TRUE))
}

#2) Use the boot function to generate the bootstrap statistics

results <- boot(data=geyser,statistic=compute_median,col_name="eruptions",R=10000)

#3) Compute the 99% confidence interval for the median

boot.ci(boot.out=results,type="basic",conf=0.99) %>% print()



