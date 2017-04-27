
require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)
require(stringr)

load("fitted_model.Rdata")

df<-data.frame(out$sims.list$Numocc) %>%
	gather() %>%
	group_by(key) %>%
	summarise(post.mean=mean(value), 
						post.median=mean(value),
						post.lwr=quantile(value, 0.025),
						post.upp=quantile(value, 0.975)) 
df$year.num<-as.numeric(gsub("[A-Z]","",df$key))+2004

#sites known to be occupied in each year
num_known_occ<-colSums(z.init, na.rm=TRUE)

#number of sites actually surveyed in each year
sites_per_year<-tapply(jags_dat$site, jags_dat$t, function(x) length(unique(x)))

tot_sites<-nrow(z.init)

df<-data.frame(df, num_known_occ, sites_per_year)

ggplot(df, aes(x=year.num, y=post.mean)) +
	geom_ribbon(aes(x=year.num, ymin=post.lwr, ymax=post.upp), col=NA, fill="green", alpha=0.3)+
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
	theme_bw()
ggsave("Figures/num_occ_plot.pdf", width=6, height=6)
