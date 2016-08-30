require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)

load("fitted_model.Rdata")

#plotting initial occupancy parameters
df<-data.frame(out$sims.list$C) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3, Grazing=X4) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland", "Fire", "Grazing"))) %>%
	filter(key != "Fire") #we haven't put fire in the model yet, so don't estimate.

histocol="green"

ggplot(df, aes(x=value)) +
	geom_histogram(col=histocol, fill=histocol, binwidth=0.05) +
	geom_vline(xintercept=0) +
	facet_grid(key~., scales="fixed") +
	ggtitle("Probability of persistence") +
	theme_bw()

ggsave("Figures/extinction_params.pdf", width=5, height=7)