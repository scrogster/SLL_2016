require(jagsUI)
require(ggplot2)
require(ggmcmc)
require(ggplot2)
require(gridExtra)
require(viridis)

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
bb<-BETA[,12]+BETA[,13]%*%(cos(2*pi*x))+BETA[,14]%*%(sin(2*pi*x))+ 
	BETA[,15]%*%(cos(4*pi*x))+BETA[,16]%*%(sin(4*pi*x))

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
combined<-data.frame("Type"=c(rep("lizards", 101), rep("indirect", 101), rep("combined", 101)), combined)

combined$x<-combined$x*365
SP<-ggplot(combined, aes(y=mean, x=x, group=Type, col=Type, linetype=Type))+
	geom_ribbon(aes(ymin=lwr, ymax=upp, fill=Type), col=NA, alpha=0.3)+
	geom_line()+
	scale_linetype_manual(values=c("solid", "dashed", 'dotted'))+
	ylab("Probability of detection")+
	xlab(NULL)+
	ylim(0, 1)+
	scale_x_continuous(breaks = cumsum(c(1, 31, 30, 28, 30, 31, 30, 31, 31, 30, 31, 30,31)), 
										 label = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 
										 					'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec', ''), expand = c(0, 0))+
	annotate("text", x=Inf, y=Inf, label="a", vjust=1.2, hjust=1.1, size=10)+
	theme_bw()+
#	theme(axis.text.x=element_text(hjust=-1.8))+
	theme(legend.position=c(0.15, 0.87))+
	theme(legend.title=element_blank())+
	theme(legend.key.size = unit(0.5, "cm")) +
	theme(legend.background=element_rect(colour="black", fill="white", size=0))+
	theme(legend.key=element_rect(colour=NA, size=0))


#Response to under-tile temperature ----############################################
#Temperature response plot
TempA<-seq(0, 50)  #Air temps between 0 and 50
TempA<-t(TempA) 
dd<-t(rep(10/12, length(TempA)))  #early November
Tdiff=t(rep(0, length(TempA)))   #No air-soil Temperature difference

#curve for lizards
Tpred<-BETA[,1]+BETA[,2]%*%(cos(2*pi*dd))+BETA[,3]%*%(sin(2*pi*dd)) + 
	BETA[,4]%*%(cos(4*pi*dd))+BETA[,5]%*%(sin(4*pi*dd)) +
	BETA[,8]%*%((TempA-22)/5) +
	BETA[,9]%*%(((TempA-22)/5)^2) +
	BETA[,10]%*%Tdiff +
	BETA[,11]%*% (((TempA-22)/5)*Tdiff ) #interaction term.
#Summary stats in a curve....
Tcurve<-data.frame("x"=t(TempA), summary_func(plogis(Tpred)))

Tplot<-ggplot(Tcurve, aes(y=mean, x=x))+
	geom_ribbon(aes(ymin=lwr, ymax=upp), col=NA, fill="green", alpha=0.2)+
	geom_ribbon(aes(ymin=lqt, ymax=uqt), col=NA, fill="green", alpha=0.4)+
	geom_line(col="green")+
	ylab("Probability of detection")+
	xlab("T (air)")+
	ylim(0, 1)+
	annotate("text", x=Inf, y=Inf, label="B", vjust=1.2, hjust=1.1, size=10)+
	theme_bw()

#Response to temperature differential ----############################################
TempA<-rep(25, 50)  #Air temp of 25
TempA<-t(TempA) 
dd<-t(rep(10/12, length(TempA)))  #early November
Tdiff=t(seq(-20, 20, length.out=length(TempA)))   #range of temp differences

#curve for lizards
Tdiffpred<-BETA[,1]+BETA[,2]%*%(cos(2*pi*dd))+BETA[,3]%*%(sin(2*pi*dd)) + 
	BETA[,4]%*%(cos(4*pi*dd))+BETA[,5]%*%(sin(4*pi*dd)) +
	BETA[,8]%*%((TempA-22)/5) +
	BETA[,9]%*%(((TempA-22)/5)^2) +
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
	annotate("text", x=Inf, y=Inf, label="C", vjust=1.2, hjust=1.1, size=10)+
	theme_bw()

#Really need a raster plot to properly visualise p ~ (T, Tdiff) relationship 


##raster plot of response

pred_data<-expand.grid(TempS=0:61, TempA=0:43, TOD=0) %>%
	        mutate(Tdiff=TempS-TempA)

beta_means<-colMeans(BETA)

yfrac<-10/12 #fraction of year
fourier.liz<-beta_means[2]*(cos(2*pi*yfrac))+beta_means[3]*(sin(2*pi*yfrac)) + 
	beta_means[4]*(cos(4*pi*yfrac))+beta_means[5]*(sin(4*pi*yfrac))

pred_detect<-function(predrow){
p.liz<-beta_means[1] +fourier.liz  + 
	beta_means[6]*predrow["TOD"]+
	beta_means[7]*(predrow["TOD"]^2)+  
	beta_means[8]*((predrow["TempA"]-22)/5) +
	beta_means[9]*((predrow["TempA"]-22)/5)^2 +
	beta_means[10]*predrow["Tdiff"] +   #effect of air-soil temp diff. 
	beta_means[11]*((predrow["TempA"]-22)/5)*predrow["Tdiff"]
return(plogis(p.liz))
}

out<-apply(pred_data, 1, pred_detect)
out<-cbind(pred_data, p=out)

dd<-data.frame(TempA=jags_dat$TempA, TempS=jags_dat$TempS, Tdiff=jags_dat$TempS-jags_dat$TempA, 
							 yfrac=jags_dat$year.frac, detect=factor(jags_dat$detect.liz)) 
#	     filter(yfrac>9/12)

ddunfilt<-data.frame(TempA=jags_dat$TempA, TempS=jags_dat$TempS, Tdiff=jags_dat$TempS-jags_dat$TempA, 
							 yfrac=jags_dat$year.frac, detect=factor(jags_dat$detect.liz), site=jags_dat$site) 

everdet_sites<- unique(ddunfilt$site[which(ddunfilt$detect==1)])

dd <- ddunfilt %>%
	     filter(site %in% everdet_sites) %>%
	     filter(yfrac > 9/12 & yfrac<12/12 ) 

RASTPLOT<-ggplot(out, aes(y=Tdiff, x=TempA)) +
	geom_raster(aes(fill=p), interpolate=FALSE) +
	scale_fill_viridis(name=~p, breaks=c(0, 0.25, 0.5, 0.75, 1), direction=-1, limits=c(NA, 1))+
	geom_contour(aes(z=p), breaks=c(0.6), col=gray(0.8), show.legend = TRUE, linetype="dashed")+
	labs(y=expression(Delta*T( degree~C)),
			 x=expression("Air temperature " ( degree~C))) +
	scale_x_continuous(limits=c(9, 41), expand = c(0, -0.5))+
	scale_y_continuous(limits=c(-6, 11), expand = c(0, -0.5))+
	scale_color_manual(values=c("white", "black"))+
	geom_abline(intercept=0, slope=0) +
	annotate("text", x=40, y=50, label="B", vjust=1.2, hjust=1.1, size=10)+
	theme_bw()+
	#theme(legend.position=c(0.11, 0.17))+
	theme(legend.position=c(0.11, 0.84))+
	theme(legend.title.align=0.5)  +
	theme(legend.key.size = unit(0.25, "cm")) +
	annotate("text", x=Inf, y=Inf, label="b", vjust=1.2, hjust=1.1, size=10)+
theme(legend.background=element_rect(colour="black", fill="white", size=0))

pdf("Figures/detection_plot.pdf", width=4, height=7)
grid.arrange(SP, RASTPLOT, ncol=1, nrow=2)
dev.off()



