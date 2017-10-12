require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)
require(gridExtra)

load("fitted_model.Rdata")

#plotting initial occupancy parameters
df<-data.frame(out$sims.list$B) %>%
	rename(Intercept=X1, Grassland=X2, Clay=X3, Roadside=X4) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland", "Clay","Roadside"))) 


INITPLOT<-ggplot(df, aes(y=value, x=key)) +
	geom_hline(yintercept=0, linetype=2) +
	geom_violin(col="black", fill="grey", alpha=0.5, scale="width")+
	ylab("Parameter value")+
	xlab("")+
	annotate("text", x=Inf, y=Inf, label="A", vjust=1.2, hjust=1.1, size=8)+
	theme_bw()

#plotting the persistence parameters
df<-data.frame(out$sims.list$C) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3,  Grazing=X4) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland", "Fire",  "Grazing" ))) 

PERSISTPLOT<-ggplot(df, aes(y=value, x=key)) +
	geom_hline(yintercept=0, linetype=2) +
	geom_violin(col="black", fill="grey", alpha=0.5, scale="width")+
	ylab("Parameter value")+
	xlab("")+
	scale_x_discrete(labels=c(
		expression(Intercept), expression(Grassland), expression(Fire),  expression(Graze)
	))+
	annotate("text", x=Inf, y=Inf, label="B", vjust=1.2, hjust=1.1, size=8)+
	theme_bw()

#plotting the colonisation parameters
df<-data.frame(out$sims.list$D) %>%
	rename(Intercept=X1, Grassland=X2) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland")))  

COLPLOT<-ggplot(df, aes(y=value, x=key)) +
	geom_hline(yintercept=0, linetype=2) +
	geom_violin(col="black", fill="grey", alpha=0.5, scale="width")+
	ylab("Parameter value")+
	xlab("")+
	annotate("text", x=Inf, y=Inf, label="C", vjust=1.2, hjust=1.1, size=8)+
	theme_bw()

pdf("Figures/parameter_plot.pdf", width=4, height=8)
grid.arrange(INITPLOT, PERSISTPLOT, COLPLOT, ncol=1, nrow=3)
dev.off()
