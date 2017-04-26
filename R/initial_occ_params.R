require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)

load("fitted_model.Rdata")

#plotting initial occupancy parameters
df<-data.frame(out$sims.list$B) %>%
	 rename(Intercept=X1, Grassland=X2, Clay=X3, Grazing1=X4, Grazing2=X5) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland", "Clay","Grazing1", "Grazing2"))) 

histocol="green"

ggplot(df, aes(x=value)) +
	geom_density(col="green", fill="green")+
	geom_vline(xintercept=0, linetype=1) +
	facet_grid(key~., scales="fixed") +
	ylab("Posterior density")+
	xlab(expression(beta))+
	theme_bw()

ggsave("Figures/initial_occ_params.pdf", width=5, height=7)



