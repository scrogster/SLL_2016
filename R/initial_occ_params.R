

require(jagsUI)
require(ggplot2)
require(ggmcmc)

load("fitted_model.Rdata")

#plotting initial occupancy parameters
df<-data.frame(out$sims.list$B) %>%
	 rename(Intercept=X1, Grassland=X2, Fire=X3, Grazing=X4) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland", "Fire", "Grazing"))) %>%
	filter(key != "Fire")

histocol="green"

ggplot(df, aes(x=value)) +
	geom_histogram(col=histocol, fill=histocol, binwidth=0.05) +
	geom_vline(xintercept=0) +
	facet_grid(key~., scales="fixed") +
	ggtitle("Initial occupancy") +
	theme_bw()



