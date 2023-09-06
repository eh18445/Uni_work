HawksTail <- c(Hawks$Tail)

HT_mean <- mean(HawksTail)
#print("Mean:")
#print(HT_mean)

HT_median <- median(HawksTail)
#print("Median:")
#print(HT_median)

Hawks %>% group_by(Species) %>% summarise(Wing_avg=mean(Wing,na.rm=TRUE),Wing_med=median(Wing,na.rm=TRUE),Wing_trim_avg=mean(Wing,na.rm=TRUE,trim=0.5),Weight_avg=mean(Weight,na.rm=TRUE),Weight_med=median(Weight,na.rm=TRUE),Weight_trim_avg=mean(Weight,na.rm=TRUE,trim=0.5))

A <- mean(HawksTail)
a <- 2
b <- 3

HawksTail2 <- a*HawksTail+b
#print(mean(HawksTail2))
#print(b+a*A)

p <- var(HawksTail)
q <- sd(HawksTail)

#calculate with functions
p2 <- var(HawksTail2)
q2 <- sd(HawksTail2) 
#calculate with formulae
#print(p*a**2)
#print(a*q)

hal <- Hawks$Hallux
hal <- hal[!is.na(hal)]

outlier_value <- 100
num_outliers <- 10
corrupted_hal <- c(hal,rep(outlier_value,times=num_outliers))

mean(hal) #%>% print()
mean(corrupted_hal) #%>% print()

num_outliers_vect <- seq(0,1000)
means_vect <- c()
median_vect <- c()
trim_means_vect <- c()

#how does no. of outliers affect mean
for (num_outliers in num_outliers_vect){
  corrupted_hal <- c(hal,rep(outlier_value,times=num_outliers))
  means_vect <- c(means_vect,mean(corrupted_hal))
}
#median
for (num_outliers in num_outliers_vect){
  corrupted_hal <- c(hal,rep(outlier_value,times=num_outliers))
  median_vect <- c(median_vect,median(corrupted_hal))
}
#trimmed mean q=0.1
for (num_outliers in num_outliers_vect){
  corrupted_hal <- c(hal,rep(outlier_value,times=num_outliers))
  trim_means_vect <- c(trim_means_vect,mean(corrupted_hal,trim=0.1))
}

df_means_medians <- data.frame(num_outliers=num_outliers_vect,mean=means_vect,median=median_vect,trim_mean=trim_means_vect)

means_median_plot <- df_means_medians %>% pivot_longer(!num_outliers,names_to="Estimator",values_to="Value") 
#print(ggplot(means_median_plot,aes(x=num_outliers,color=Estimator,linetype=Estimator,y=Value)) + geom_line() + xlab("Number of Outliers"))

#box plot with weights broken down as species
weight_plot <- ggplot(data=Hawks,aes(x=Species,y=Weight)) + geom_boxplot() + xlab("Species") + ylab("Weight (g)")
#print(weight_plot)

#compute weights grouped by species
Hawks %>% group_by(Species)%>% summarise(quantile025=quantile(Weight,probs=0.25,na.rm=TRUE),quantile050=quantile(Weight,probs=0.5,na.rm=TRUE),quantile075=quantile(Weight,probs=0.75,na.rm=TRUE)) #%>% print()


#test vector
test <- c(0,20,13,50,64,22,37,82,99,13,NA,51,200,-600)
test <- test[!is.na(test)]
#iq_range <- IQR(test,na.rm=TRUE)
#print(iq_range)
#outliers <- test[(test>quantile75+1.5*iq_range)|(test<quantile25-1.5*iq_range)]
#print(outliers)

num_outliers <- function (val){
  #val <- val[!is.na(data)]
  iq_range <- IQR(val,na.rm=TRUE)
  outliers <- val[(val>quantile75+1.5*iq_range)|(val<quantile25-1.5*iq_range)]
  return(length(outliers))
}
num_outliers(test) #%>% print()
num_outliers(c(0,40,60,185)) #%>% print()


Hawks %>% group_by(Species)%>% summarise(num_outliersweight=num_outliers(Weight)) #%>% print()

X = Hawks$Weight
Y= Hawks$Wing

a = 2.4
b = 7.1
c = -1
d = 3

X2 = a*X+b
Y2 = c*Y+d

cov(X2,Y2,use='complete.obs') #%>% print()
cor(X2,Y2,use='complete.obs') #%>% print()

#print(a*c*cov(X,Y,use='complete.obs'))
#print(cor(X,Y,use='complete.obs'))

Hawk_plot1 <- ggplot(data=Hawks,aes(x=Tail,color=Species)) + xlab("Tail Length (mm)") + geom_density(adjust=0.5) + theme_bw() + ylab("Density")
#Hawk_plot %>% print()

Hawk_plot2 <- ggplot(data=Hawks,aes(x=Tail,y=Species,fill=Species)) + xlab("Tail Length (mm)") + geom_violin() + ylab("Density")
#Hawk_plot2 %>% print()

Hawk_plot3 <- ggplot(data=Hawks,aes(x=Tail,y=Weight,color=Species,shape=Species)) + xlab('Tail Length (mm)') + ylab('Weight (g)') + geom_point()
#Hawk_plot3 %>% print()

#ggplot(), geom_point(), geom_smooth() and facet_wrap()
Hawk_plot4 <- ggplot(data=Hawks,aes(x=Tail,y=Weight,color=Species)) + xlab('Tail Length (mm)') + ylab('Weight (g)') + geom_point(na.rm=TRUE) + geom_smooth(na.rm=TRUE) + facet_wrap(~Species,scales='free_x')
#Hawk_plot4 %>% print()

heaviest <- Hawks %>% filter(Weight==max(select(Hawks,Weight),na.rm=TRUE)) 
Hawk_plot5 <- ggplot(data=Hawks,aes(x=Tail,y=Weight,color=Species,shape=Species)) + xlab('Tail Length (mm)') + ylab('Weight (g)') + geom_point(na.rm=TRUE) + geom_curve(x=175,xend=as.numeric(select(heaviest,Tail)),y=1750,yend=as.numeric(select(heaviest,Weight)),arrow=arrow(length=unit(0.5,'cm')),curvature=0.1,color='black') + geom_text(x=175,y=1730,label='Heaviest Hawk',color='black')
#Hawk_plot5 %>% print()




