
max_cor_var <- function(df, col_name){
  #function that takes a column of a data frame and returns the variable with the highest correlation and the correlation value
  
  v_col <- select(df,all_of(col_name)) #extract column based on column name
  df_num <- select_if(df,is.numeric) %>% select(-all_of(col_name)) #Select all numeric columns except col_name
  
  cor_func <- function(x){cor(x,v_col,use='complete.obs')} #A function that computes cor between v_col and a vector
  correlations <- unlist(map(df_num,cor_func)) #Compute correlations with all other numeric columns
  
  max_abs_cor_var <- names(which(abs(correlations)==max(abs(correlations)))) #Extract column with max correlation
  cor_val <- as.double(correlations[max_abs_cor_var]) #Extracts correlation value of max correlation
  
  return(data.frame(max_abs_cor_var,cor_val)) #Return in the form of a data frame
}

print(max_cor_var(penguins,'body_mass_g'))
