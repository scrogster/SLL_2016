require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)

load("fitted_model.Rdata")

#plotting initial occupancy parameters
df<-data.frame(out$sims.list$B) %>%
	 rename(Intercept=X1, Grassland=X2, Conservation=X3, Grazing=X4) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland", "Conservation", "Grazing"))) 

histocol="green"

ggplot(df, aes(x=value)) +
	geom_histogram(col=histocol, fill=histocol, binwidth=0.05) +
	geom_vline(xintercept=0) +
	facet_grid(key~., scales="fixed") +
	ggtitle("Initial occupancy") +
	theme_bw()

ggsave("Figures/initial_occ_params.pdf", width=5, height=7)



