
#1) Sample of Gentoo weights
gentoo_weights <- penguins %>% filter(species=='Gentoo') %>% pull(body_mass_g)

#2) MLE of mu and sigma
n <- length(gentoo_weights) #sample size
mu_mle <- mean(gentoo_weights,na.rm=TRUE)
sigma_mle <- sd(gentoo_weights,na.rm=TRUE)*sqrt(abs((1-n)/n))

#3) Generate indicies
weights <- seq(mu_mle-3*sigma_mle,mu_mle+3*sigma_mle,sigma_mle*0.001)

#4) Plot estimated  density functions (MLE density)
colors <- c("MLE density"="red","Kernel density"="blue") #set color legend
estimated_density <- data.frame(Weight=weights,Density=dnorm(weights,mean=mu_mle,sd=sigma_mle))
plot_obj <- ggplot() + geom_line(data=estimated_density,aes(x=Weight,y=Density,color="MLE density"))

#5) Kernel density plot of the example
plot_obj <- plot_obj + geom_density(data=tibble(gentoo_weights),aes(x=gentoo_weights,color="Kernel density")) + labs(y="Density function",color="Estimator") + theme_bw() + scale_color_manual(values=colors)
plot_obj #%>% print()

