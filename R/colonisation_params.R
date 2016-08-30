require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)

load("fitted_model.Rdata")

#plotting initial occupancy parameters
df<-data.frame(out$sims.list$D) %>%
	rename(Intercept=X1, Grassland=X2) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland")))  

histocol="green"

ggplot(df, aes(x=value)) +
	geom_histogram(col=histocol, fill=histocol, binwidth=0.05) +
	geom_vline(xintercept=0) +
	facet_grid(key~., scales="fixed") +
	ggtitle("Probability of colonisation") +
	theme_bw()

ggsave("Figures/colonisation_params.pdf", width=5, height=3.5)