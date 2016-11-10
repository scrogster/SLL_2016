require(jagsUI)
require(ggplot2)
require(ggmcmc)
require(ggplot2)

load("fitted_model.Rdata")

#plotting the detection curves

#trial plotting of seasonal response curves. no uncertainty yet.
BETA<-out$sims.list$BETA
#BETA<-colMeans(out$sims.list$BETA)

x<-seq(0, 1, by=0.01)
x<-t(x)

#curve for lizards
aa<-BETA[,1]+BETA[,2]%*%(cos(2*pi*x))+BETA[,3]%*%(sin(2*pi*x)) + 
	BETA[,4]%*%(cos(4*pi*x))+BETA[,5]%*%(sin(4*pi*x))

#curve for skins, eggs etc
bb<-BETA[,10]+BETA[,11]%*%(cos(2*pi*x))+BETA[,12]%*%(sin(2*pi*x))+ 
	BETA[,13]%*%(cos(4*pi*x))+BETA[,14]%*%(sin(4*pi*x))

cc<-  1-(1-plogis(aa)) * (1-plogis(bb))

summary_func<-function(x){
	post.mean=apply(x, 2, mean)
	lwr.ci=apply(x, 2, quantile, 0.025)
	upp.ci=apply(x, 2, quantile, 0.975)
	out=data.frame("mean"=post.mean, "lwr"=lwr.ci, "upp"=upp.ci)
	return(out)
}

lizcurve<-data.frame("x"=t(x), summary_func(plogis(aa)))
skincurve<-data.frame("x"=t(x), summary_func(plogis(bb)))
totcurve<-data.frame("x"=t(x), summary_func(cc))

combined<-rbind(lizcurve, skincurve, totcurve)
combined<-data.frame("Type"=c(rep("lizards", 101), rep("skins", 101), rep("combined", 101)), combined)

combined$x<-combined$x*365

ggplot(combined, aes(y=mean, x=x, group=Type, col=Type))+
	geom_ribbon(aes(ymin=lwr, ymax=upp, fill=Type), col=NA, alpha=0.2)+
	geom_line()+
	ylab("Probability of detection")+
	xlab("Julian date")+
	ylim(0, 1)+
	scale_x_continuous(limits=c(0, 365), breaks=cumsum(c(0, 30, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)),
										 labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", ""))+
	theme_bw()+
	theme(axis.text.x=element_text(hjust=-2))+
	theme(legend.position=c(0.15, 0.85))+
	theme(legend.background=element_rect(colour="black", fill="white", size=0))+
	theme(legend.key=element_rect(colour=NA, size=0))
ggsave("Figures/detection_plot.pdf", width=6, height=6)

