
require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)
require(stringr)

load("fitted_model.Rdata")

df<-data.frame(out$sims.list$Deficit) %>%
	gather() %>%
	group_by(key) %>%
	summarise(post.mean=mean(value), 
						post.median=mean(value),
						post.lwr=quantile(value, 0.025),
						post.upp=quantile(value, 0.975)) 
df$year.num<-as.numeric(gsub("[A-Z]","",df$key))+2005

ggplot(df, aes(x=year.num, y=post.mean)) +
	geom_ribbon(aes(x=year.num, ymin=post.lwr, ymax=post.upp), col=NA, fill="blue", alpha=0.3)+
	geom_line(col="blue")+
	geom_point(col="blue")+
	ylab("Net change in occupancy")+
	xlab("Year")+
	geom_hline(yintercept = 0)+
	theme_bw()
ggsave("flux_plot.pdf", width=6, height=6)