require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)
require(stringr)

load("fitted_model.Rdata")

df_ext<-data.frame(out$sims.list$Ext_t) %>%
	gather() %>%
	group_by(key) %>%
	summarise(post.mean=mean(value), 
						post.median=mean(value),
						post.lwr=quantile(value, 0.025),
						post.upp=quantile(value, 0.975)) 
df_ext$year.num<-as.numeric(gsub("[A-Z]","",df_ext$key))+2005

df_col<-data.frame(out$sims.list$Col_t) %>%
	gather() %>%
	group_by(key) %>%
	summarise(post.mean=mean(value), 
						post.median=mean(value),
						post.lwr=quantile(value, 0.025),
						post.upp=quantile(value, 0.975)) 
df_col$year.num<-as.numeric(gsub("[A-Z]","",df_col$key))+2005

ggplot(df_ext, aes(x=year.num, y=post.mean)) +
	geom_ribbon(aes(x=year.num, ymin=post.lwr, ymax=post.upp), col="red", fill="red", alpha=0.2)+
	geom_line(col="red")+
	geom_point(col="red")+
	geom_ribbon(data=df_col, aes( x=year.num, ymin=post.lwr, ymax=post.upp), col="green", fill="green", alpha=0.2)+
	geom_line(data=df_col, aes(x=year.num, y=post.mean), col="green")+
	geom_point(data=df_col, aes(x=year.num, y=post.mean), col="green")+
	ylab("Number of extinctions/colonisations")+
	xlab("Year")+
	ylim(0, 20)+
	theme_bw()
ggsave("num_ext_plot.pdf", width=6, height=6)