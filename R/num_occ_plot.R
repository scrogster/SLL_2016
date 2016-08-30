
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

ggplot(df, aes(x=year.num, y=post.mean)) +
	geom_ribbon(aes(x=year.num, ymin=post.lwr, ymax=post.upp), col="green", fill="green", alpha=0.7)+
	geom_line()+
	geom_point()+
	ylab("Number of occupied sites")+
	xlab("Year")+
	ylim(0, 75)+
	theme_bw()
ggsave("num_occ_plot.pdf", width=6, height=6)
