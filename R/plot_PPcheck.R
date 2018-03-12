
load("PP_check.Rdata")

library(ggplot2)
library(patchwork)


func<-function(x, t){
	out<-tapply(x, t, "mean")
	return(out)
}

real_liz<-func(jags_dat$detect.liz, jags_dat$t)
real_skin<-func(jags_dat$detect.skin, jags_dat$t)

fake_skin<-t(apply(out$sims.list$fake.skin, 1, func, t=jags_dat$t))
lwr_skin<-apply(fake_skin, 2, quantile, prob=0.025)
med_skin<-apply(fake_skin, 2, quantile, prob=0.5)
upp_skin<-apply(fake_skin, 2, quantile, prob=0.975)

skin_stats<-data.frame("real"=real_skin, med=med_skin, lwr=lwr_skin, upp=upp_skin, t=1:9)


fake_liz<-t(apply(out$sims.list$fake.liz, 1, func, t=jags_dat$t))
lwr_liz<-apply(fake_liz, 2, quantile, prob=0.025)
med_liz<-apply(fake_liz, 2, quantile, prob=0.5)
upp_liz<-apply(fake_liz, 2, quantile, prob=0.975)

liz_stats<-data.frame("real"=real_liz, med=med_liz, lwr=lwr_liz, upp=upp_liz, t=1:9)


skin_plot<-ggplot(skin_stats, aes(x=t, y=real)) +
	geom_line(aes(y=med), col="green") +
	geom_ribbon(aes(x=t, ymin=lwr, ymax=upp),  col=NA, fill="green", alpha=0.3) +
	geom_line(aes(x=t, y=real)) +
	geom_point( aes(x=t, y=real)) +
	scale_x_continuous(breaks=seq(1, 9))+
	ylim(0, NA)+
	xlab("Year") +
	ylab("Proportion positive")+
	ggtitle("Skins")+
	theme_bw()

liz_plot<-ggplot(liz_stats, aes(x=t, y=real)) +
	geom_line(aes(y=med), col="green") +
	geom_ribbon(aes(x=t, ymin=lwr, ymax=upp),  col=NA, fill="green", alpha=0.3) +
	geom_line(aes(x=t, y=real)) +
	geom_point( aes(x=t, y=real)) +
	scale_x_continuous(breaks=seq(1, 9))+
	ylim(0, NA)+
	xlab("Year") +
	ylab("Proportion positive")+
	ggtitle("Lizards")+
	theme_bw()


skin_plot+liz_plot + plot_layout(ncol=1, heights = c(1, 1))
ggsave("Figures/PP_check_plot.pdf", width=6, height=7)
