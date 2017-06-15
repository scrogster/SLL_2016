require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)
require(gridExtra)
require(stringr)

load("fitted_model.Rdata")

#### Plot of number of surveyed, detected, and predicted occupied sites --------------------- ################
df<-data.frame(out$sims.list$Numocc) %>%
	gather() %>%
	group_by(key) %>%
	summarise(post.mean=mean(value), 
						post.median=mean(value),
						post.lwr=quantile(value, 0.025),
						post.upp=quantile(value, 0.975),
						post.25=quantile(value, 0.25),
						post.75=quantile(value, 0.75)) 
df$year.num<-as.numeric(gsub("[A-Z]","",df$key))+2004

#sites known to be occupied in each year
num_known_occ<-colSums(z.init, na.rm=TRUE)

#number of sites actually surveyed in each year
sites_per_year<-tapply(jags_dat$site, jags_dat$t, function(x) length(unique(x)))

tot_sites<-nrow(z.init)

df<-data.frame(df, num_known_occ, sites_per_year)

NOCC<-ggplot(df, aes(x=year.num, y=post.mean)) +
	geom_ribbon(aes(x=year.num, ymin=post.lwr, ymax=post.upp), col=NA, fill="green", alpha=0.3)+
	geom_ribbon(aes(x=year.num, ymin=post.25, ymax=post.75), col=NA, fill="green", alpha=0.5)+
	geom_line(col="green")+
	geom_point(col="green")+
	geom_line(aes(x=year.num, y=num_known_occ), col="red")+
	geom_point(aes(x=year.num, y=num_known_occ), col="red", pch=3)+
	geom_line(aes(x=year.num, y=sites_per_year), col="blue")+
	geom_point(aes(x=year.num, y=sites_per_year), col="blue", pch=2)+
	geom_hline(yintercept=tot_sites, col="gray", lty=2) +
	ylab("Number of occupied sites")+
	xlab("Year")+
	ylim(0, NA)+
	scale_x_continuous(breaks=seq(2005, 2013))+
	annotate("text", x=Inf, y=Inf, label="A", vjust=1.2, hjust=1.1, size=8)+
	theme_bw()


#### Plot of extinction and colonisation events  --------------------------------------- ################

df_ext<-data.frame(out$sims.list$Ext_t) %>%
	gather() %>%
	group_by(key) %>%
	summarise(post.mean=mean(value), 
						post.median=mean(value),
						post.lwr=quantile(value, 0.025),
						post.upp=quantile(value, 0.975),
						post.25=quantile(value, 0.25),
						post.75=quantile(value, 0.75)) 
df_ext$year.num<-as.numeric(gsub("[A-Z]","",df_ext$key))+2005

df_col<-data.frame(out$sims.list$Col_t) %>%
	gather() %>%
	group_by(key) %>%
	summarise(post.mean=mean(value), 
						post.median=mean(value),
						post.lwr=quantile(value, 0.025),
						post.upp=quantile(value, 0.975),
						post.25=quantile(value, 0.25),
						post.75=quantile(value, 0.75)) 
df_col$year.num<-as.numeric(gsub("[A-Z]","",df_col$key))+2005

EXT<-ggplot(df_ext, aes(x=year.num, y=post.mean)) +
	geom_ribbon(aes(x=year.num, ymin=post.lwr, ymax=post.upp), col=NA, fill="red", alpha=0.2)+
	geom_ribbon(aes(x=year.num, ymin=post.25, ymax=post.75), col=NA, fill="red", alpha=0.5)+
	geom_line(col="red")+
	geom_point(col="red")+
	geom_ribbon(data=df_col, aes( x=year.num, ymin=post.lwr, ymax=post.upp), col=NA, fill="green", alpha=0.2)+
	geom_ribbon(data=df_col, aes( x=year.num, ymin=post.25, ymax=post.75), col=NA, fill="green", alpha=0.5)+
	geom_line(data=df_col, aes(x=year.num, y=post.mean), col="green")+
	geom_point(data=df_col, aes(x=year.num, y=post.mean), col="green")+
	ylab("Number of extinctions/colonisations")+
	xlab("Year")+
	ylim(0, NA)+
	annotate("text", x=Inf, y=Inf, label="B", vjust=1.2, hjust=1.1, size=8)+
	theme_bw()

#### Plot of site occupancy flux  --------------------------------------- ################

df<-data.frame(out$sims.list$Deficit) %>%
	gather() %>%
	group_by(key) %>%
	summarise(post.mean=mean(value), 
						post.median=mean(value),
						post.lwr=quantile(value, 0.025),
						post.upp=quantile(value, 0.975),
						post.25=quantile(value, 0.25),
						post.75=quantile(value, 0.75)) 
df$year.num<-as.numeric(gsub("[A-Z]","",df$key))+2005

FLUX<-ggplot(df, aes(x=year.num, y=post.mean)) +
	geom_ribbon(aes(x=year.num, ymin=post.lwr, ymax=post.upp), col=NA, fill="blue", alpha=0.3)+
	geom_ribbon(aes(x=year.num, ymin=post.25, ymax=post.75), col=NA, fill="blue", alpha=0.5)+
	geom_line(col="blue")+
	geom_point(col="blue")+
	ylab("Net change in occupancy")+
	xlab("Year")+
	geom_hline(yintercept = 0, lty=2)+
	annotate("text", x=Inf, y=-Inf, label="C", vjust=-0.2, hjust=1.1, size=8)+
	theme_bw()

#### Arrange plots and dump plot to file  --------------------------------------- ################

pdf("Figures/site_occ_plot.pdf", width=5, height=7.5)
grid.arrange(NOCC, EXT, FLUX, ncol=1, nrow=3)
dev.off()