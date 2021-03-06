require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)
require(gridExtra)

load("fitted_model.Rdata")
dir.create("Figures", showWarnings = FALSE)
#plotting initial occupancy parameters
df<-data.frame(out$sims.list$B) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3, Grazing=X4, Fire.Grazing=X5, Clay=X6) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland", "Fire", "Grazing" , "Fire.Grazing", "Clay")))

df_error<-df %>%
	     group_by(key) %>%
	     summarise(lower=quantile(value, 0.025), mid=quantile(value, 0.5), upper=quantile(value, 0.975))


INITPLOT<-ggplot(df, aes(y=value, x=key)) +
	geom_hline(yintercept=0, linetype=2, colour=gray(0.5)) +
	geom_violin(col="black", fill="#21908CFF", alpha=0.5, scale="width")+
	geom_errorbar(data=df_error, aes(x=key, y=mid, ymin=lower, ymax=upper), width=0.3) +
	geom_point(data=df_error, aes(x=key, y=mid)) +
	ylab("Parameter value")+
	xlab("")+
	annotate("text", x=-Inf, y=Inf, label="a", vjust=1.2, hjust=-0.8, size=8)+
	theme_bw()+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#plotting the persistence parameters
df<-data.frame(out$sims.list$C) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3, Grazing=X4, Fire.Grazing=X5, Clay=X6) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland", "Fire", "Grazing" , "Fire.Grazing", "Clay")))

df_error<-df %>%
	group_by(key) %>%
	summarise(lower=quantile(value, 0.025), mid=quantile(value, 0.5), upper=quantile(value, 0.975))

PERSISTPLOT<-ggplot(df, aes(y=value, x=key)) +
	geom_hline(yintercept=0, linetype=2, colour=gray(0.5)) +
	geom_violin(col="black", fill="#21908CFF", alpha=0.5, scale="width")+
	geom_errorbar(data=df_error, aes(x=key, y=mid, ymin=lower, ymax=upper), width=0.3) +
	geom_point(data=df_error, aes(x=key, y=mid)) +
	ylab("Parameter value")+
	xlab("")+
	annotate("text", x=-Inf, y=Inf, label="b", vjust=1.2, hjust=-0.8, size=8)+
	theme_bw()+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#plotting the colonisation parameters
df<-data.frame(out$sims.list$D) %>%
	rename(Intercept=X1, Grassland=X2) %>%
	gather() %>%
	mutate(key=factor(key, levels=c("Intercept", "Grassland")))  

df_error<-df %>%
	group_by(key) %>%
	summarise(lower=quantile(value, 0.025), mid=quantile(value, 0.5), upper=quantile(value, 0.975))


COLPLOT<-ggplot(df, aes(y=value, x=key)) +
	geom_hline(yintercept=0, linetype=2, colour=gray(0.5)) +
	geom_violin(col="black", fill="#21908CFF", alpha=0.5, scale="width")+
	geom_errorbar(data=df_error, aes(x=key, y=mid, ymin=lower, ymax=upper), width=0.15) +
	geom_point(data=df_error, aes(x=key, y=mid)) +
	ylab("Parameter value")+
	xlab("")+
	annotate("text", x=-Inf, y=Inf, label="c", vjust=1.2, hjust=-0.8, size=8)+
	theme_bw()+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf("Figures/parameter_plot.pdf", width=4.5, height=8)
grid.arrange(INITPLOT, PERSISTPLOT, COLPLOT, ncol=1, nrow=3)
dev.off()

png("Figures/parameter_plot.png", width=4.5, height=8, units="in", res=450)
grid.arrange(INITPLOT, PERSISTPLOT, COLPLOT, ncol=1, nrow=3)
dev.off()
