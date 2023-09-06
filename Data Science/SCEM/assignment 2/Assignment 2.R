
hSF <- Hawks %>% filter(Species=="RT" & Weight>1000) %>% select(Wing,Weight,Tail)

#hSF %>% arrange(Wing) %>% head(5) %>% print()

species_code <- c("CH","RT","SS")
species_name_full <- c("Cooper's","Red-tailed","Sharp-shinned")

hawkSpeciesNameCodes <- data.frame(species_code,species_name_full)

#Hawks %>% print()


#select(Hawks,Species) %>% print() 
hawksFullName <- left_join(Hawks,hawkSpeciesNameCodes,by=c("Species"="species_code")) %>% select(-Species)

hawksFullName <- rename(hawksFullName, Species=species_name_full)

#hawksFullName %>% select(Species,Wing,Weight) %>% head(7) %>% print()

hawksWithBMI <- Hawks %>% mutate(bird_BMI=Weight*10^3/Wing^2) %>% select(Species,bird_BMI) %>% arrange(desc(bird_BMI)) %>% head(8)

#summarize number of rows, mean wing span (cm), median wing span (cm), trimmed average wing span (cm), biggest ratio between wingspan and length 

hawksFullName %>% group_by(Species) %>% summarize(num_rows=n(),mn_wing=mean(Wing),nd_wing=median(Wing),t_mn_wing=mean(Wing,trim=0.1),b_wt_ratio=max(Wing/Tail)) %>% print()

#Wing, Weight, Culmen, Hallux, Tail, StandardTail, Tarsus, and Crop
Hawks %>% group_by(Species) %>% summarize(across(everything(),~sum(is.na(.x)))) %>% select(Species,Wing,Weight,Culmen,Hallux,Tail,StandardTail,Tarsus,Crop)


#tidying date
#mean
impute_by_mean <- function(x){
  mu <- mean(x,na.rm=TRUE) #calculates mean of the vector
  impute_f <- function(z){ # function that checks for if each element is NA and if it is replaces it with mean
    if(is.na(z)){
      return(mu)
    }else{
      return(z)
    }
  }
  #map applies a function to each element of vector. In this case the vector is x and the function is impute_f
  #map_dbl returns a double vector
  return(map_dbl(x,impute_f))
}

#median
impute_by_median <- function(x){
  med <- median(x,na.rm=TRUE)
  impute_k <- function(y){
    if(is.na(y)){
      return(med)
    }else{
      return(y)
    }
  }
  return(map_dbl(x,impute_k))
}

impute_by_median(c(1,2,NA,4))

x <- c(seq(0,10,0.1))
y <- 5*x +1
df_xy <- data.frame(x,y)

multi <- function(x,y){
  z <- x*y
  return (z)
} 
#df_xy %>% mutate(z = map2_dbl(x,y,multi)) %>% print()

sometimes_missing <- function(index,value){
  #function returns NA if index divisible by 5 and returns value otherwise
  if (index%%5 == 0){
    return (NA)
  }else{
    return (value)
  }
}

#use row_number() to check if divisible by 5
#use map2_dbl() to apply function
#mutate() to create a new column?

df_xy_missing <- df_xy %>% mutate(y = map2_dbl(row_number(),y,sometimes_missing))

#replace missing values in df_xy_missing with median using impute_by_median()
df_xy_imputed <- df_xy_missing %>% mutate(y = impute_by_median(y))

#df_xy_imputed %>% head(6) %>% print()


folder_path<-"C:/Users/danie/OneDrive/Documents/R/SCEM/assignment 2/" # set this to the name of the
# directory containing "HockeyLeague.xlsx"
file_name<-"HockeyLeague(3).xlsx" # set the file name
file_path<-paste(folder_path,file_name,sep="") # create the file_path
wins_data_frame<-read_excel(file_path,sheet="Wins") # read of a sheet from an xl file

#create wins_tidy
#columns: teams,year,win,total
#rename(), pivot_longer(), mutate() and separate()

columns <- c(colnames(wins_data_frame)) 
columns <- columns[! columns %in% '...1']
wins_tidy <- wins_data_frame %>% pivot_longer(columns,names_to='year',values_to='wins_per_season') %>% separate(wins_per_season,into=c("wins","total"),sep=" of ",convert=TRUE) %>% type.convert(as.is=TRUE) %>% rename(teams='...1')


#Same thing but with losses instead of wins
folder_path<-"C:/Users/danie/OneDrive/Documents/R/SCEM/assignment 2/" # set this to the name of the
# directory containing "HockeyLeague.xlsx"
file_name<-"HockeyLeague(3).xlsx" # set the file name
file_path<-paste(folder_path,file_name,sep="") # create the file_path
losses_data_frame<-read_excel(file_path,sheet="Losses") # read of a sheet from an xl file

#create wins_tidy
#columns: teams,year,win,total
#rename(), pivot_longer(), mutate() and separate()

columns <- c(colnames(losses_data_frame)) 
columns <- columns[! columns %in% '...1']
losses_tidy <- losses_data_frame %>% pivot_longer(columns,names_to='year',values_to='losses_per_season') %>% separate(losses_per_season,into=c("losses","total"),sep=" of ",convert=TRUE) %>% type.convert(as.is=TRUE) %>% rename(teams='...1')

#columns: team, year, wins, losses, total, draws, wins_rt, losses_rt, draws_rt
hockey_df <- full_join(wins_tidy,losses_tidy) %>% mutate(draws=total-wins-losses,wins_rt=wins/total,losses_rt=losses/total,draws_rt=draws/total)
#hockey_df %>% head(5) %>% print()

#create summary data frame with mean and median for each of win_rt, losses_rt and draws_rt for each team.
#sort in descending order of median win rate
#select(), group_by(), across(), arrange()

hockey_mn <- hockey_df %>% group_by(teams) %>% select(wins_rt,losses_rt,draws_rt) %>% summarise(across(everything(),~mean(.x))) %>% rename(W_mn=wins_rt,L_mn=losses_rt,D_mn=draws_rt)
hockey_med <- hockey_df %>% group_by(teams) %>% select(wins_rt,losses_rt,draws_rt) %>% summarise(across(everything(),~median(.x))) %>% rename(W_med=wins_rt,L_med=losses_rt,D_med=draws_rt)
hockey_sum <- full_join(hockey_mn,hockey_med) %>% arrange(desc(W_med))
#print(hockey_sum)

#using wins_tidy. Create histogram opf the wins of Ducks. binwidth 3
#filter(), ggplot() and geom_histogram

wins_plot <- ggplot(data=filter(wins_tidy,teams == 'Ducks'),aes(x=wins)) + xlab("number of wins by ducks")
#print(wins_plot + geom_histogram(binwidth=3) + ylab("count"))

#now with geom_density()
print(wins_plot + geom_density(adjust=0.5) + ylab("Count"))
print(wins_plot + geom_density(adjust=2) + ylab("Count"))

#print(penguins)
#mass_flipper_scatter <- ggplot(data=penguins,aes(y=body_mass_g,x=flipper_length_mm)) + xlab("Flipper Length (mm)") + ylab("Body Mass (g)")
#print(mass_flipper_scatter+geom_point(size=3))


#using wins_tidy create data frame wins_teams with columns: Year, Ducks, Eagles, as well as the other teams.
#use select() and pivot_wider()

wins_teams <- wins_tidy %>% pivot_wider(names_from=teams,values_from=wins) %>% select(-total)
print(wins_teams)
teams_plot <- ggplot(data=wins_teams,aes(x=Ducks,y=Eagles)) + xlab("Wins by Ducks") + ylab("Wins by Eagles")
print(teams_plot + geom_point(size=3))




