require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)

load("fitted_model.Rdata")

#plotting initial occupancy parameters
df<-data.frame(out$sims.list$C) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3, Firegt4=X4, Grazing=X5, Grazing2=X6) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland", "Fire", "Firegt4", "Grazing", "Grazing2"))) 

histocol="green"

ggplot(df, aes(x=value)) +
	geom_density(col="green", fill="green")+
	geom_vline(xintercept=0, linetype=1) +
	facet_grid(key~., scales="fixed") +
	ylab("Posterior density")+
	xlab(expression(lambda))+
	theme_bw()

ggsave("Figures/extinction_params.pdf", width=5, height=7)