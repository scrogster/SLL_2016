require(jagsUI)
require(ggplot2)
require(ggmcmc)
require(ggplot2)
require(gridExtra)

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
bb<-BETA[,11]+BETA[,12]%*%(cos(2*pi*x))+BETA[,13]%*%(sin(2*pi*x))+ 
	BETA[,14]%*%(cos(4*pi*x))+BETA[,15]%*%(sin(4*pi*x))

cc<-  1-(1-plogis(aa)) * (1-plogis(bb))

summary_func<-function(x){
	post.mean=apply(x, 2, mean)
	lwr.ci=apply(x, 2, quantile, 0.025)
	upp.ci=apply(x, 2, quantile, 0.975)
	lwr.qt=apply(x, 2, quantile, 0.25)
	upp.qt=apply(x, 2, quantile, 0.75)
	out=data.frame("mean"=post.mean, "lwr"=lwr.ci, "upp"=upp.ci, "lqt"=lwr.qt, "uqt"=upp.qt)
	return(out)
}

lizcurve<-data.frame("x"=t(x), summary_func(plogis(aa)))
skincurve<-data.frame("x"=t(x), summary_func(plogis(bb)))
totcurve<-data.frame("x"=t(x), summary_func(cc))

combined<-rbind(lizcurve, skincurve, totcurve)
combined<-data.frame("Type"=c(rep("lizards", 101), rep("skins", 101), rep("combined", 101)), combined)

combined$x<-combined$x*365

SP<-ggplot(combined, aes(y=mean, x=x, group=Type, col=Type))+
	geom_ribbon(aes(ymin=lwr, ymax=upp, fill=Type), col=NA, alpha=0.2)+
	geom_ribbon(aes(ymin=lqt, ymax=uqt, fill=Type), col=NA, alpha=0.4)+
	geom_line()+
	ylab("Probability of detection")+
	xlab("Julian date")+
	ylim(0, 1)+
	scale_x_continuous(limits=c(0, 365), breaks=cumsum(c(0, 30, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)),
										 labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D", ""))+
	theme_bw()+
	theme(axis.text.x=element_text(hjust=-2))+
	theme(legend.position=c(0.135, 0.82))+
	theme(legend.background=element_rect(colour="black", fill="white", size=0))+
	theme(legend.key=element_rect(colour=NA, size=0))
#ggsave("Figures/detection_plot.pdf", width=6, height=6)

#Response to under-tile temperature ----############################################
#Temperature response plot
Temp<-seq(0, 50)  #Undertile temps between 0 and 50
Temp<-t(Temp) 
dd<-t(rep(10/12, length(Temp)))  #early November
Tdiff=t(rep(0, length(Temp)))   #No air-soil Temperature difference

#curve for lizards
Tpred<-BETA[,1]+BETA[,2]%*%(cos(2*pi*dd))+BETA[,3]%*%(sin(2*pi*dd)) + 
	BETA[,4]%*%(cos(4*pi*dd))+BETA[,5]%*%(sin(4*pi*dd)) +
	BETA[,8]%*%((Temp-22)/5) +
	BETA[,9]%*%(((Temp-22)/5)^2) +
	BETA[,10]%*%Tdiff 

#Summary stats in a curve....
Tcurve<-data.frame("x"=t(Temp), summary_func(plogis(Tpred)))

Tplot<-ggplot(Tcurve, aes(y=mean, x=x))+
	geom_ribbon(aes(ymin=lwr, ymax=upp), col=NA, fill="green", alpha=0.2)+
	geom_ribbon(aes(ymin=lqt, ymax=uqt), col=NA, fill="green", alpha=0.4)+
	geom_line(col="green")+
	ylab("Probability of detection")+
	xlab("T (tile)")+
	ylim(0, 1)+
	theme_bw()

#Response to temperature differential ----############################################
Temp<-rep(25, 50)  #Undertile temp of 25
Temp<-t(Temp) 
dd<-t(rep(10/12, length(Temp)))  #early November
Tdiff=t(seq(-20, 20, length.out=length(Temp)))   #No air-soil Temperature difference

#curve for lizards
Tdiffpred<-BETA[,1]+BETA[,2]%*%(cos(2*pi*dd))+BETA[,3]%*%(sin(2*pi*dd)) + 
	BETA[,4]%*%(cos(4*pi*dd))+BETA[,5]%*%(sin(4*pi*dd)) +
	BETA[,8]%*%((Temp-22)/5) +
	BETA[,9]%*%(((Temp-22)/5)^2) +
	BETA[,10]%*%Tdiff 

#Summary stats in a curve....
Tdiffcurve<-data.frame("x"=t(Tdiff), summary_func(plogis(Tdiffpred)))

Tdiffplot<-ggplot(Tdiffcurve, aes(y=mean, x=x))+
	geom_ribbon(aes(ymin=lwr, ymax=upp), col=NA, fill="green", alpha=0.2)+
	geom_ribbon(aes(ymin=lqt, ymax=uqt), col=NA, fill="green", alpha=0.4)+
	geom_line(col="green")+
	ylab("Probability of detection")+
	xlab(expression(paste(Delta,T)))+
	ylim(0, 1)+
	theme_bw()


pdf("Figures/detection_plot.pdf", width=5, height=11)
grid.arrange(SP, Tplot, Tdiffplot, ncol=1, nrow=3)
dev.off()


