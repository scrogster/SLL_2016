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
combined<-data.frame("Type"=c(rep("lizards", 101), rep("skins", 101), rep("combined", 101)), combined)

combined$x<-combined$x*365
SP<-ggplot(combined, aes(y=mean, x=x, group=Type, col=Type))+
	geom_ribbon(aes(ymin=lwr, ymax=upp, fill=Type), col=NA, alpha=0.2)+
	geom_ribbon(aes(ymin=lqt, ymax=uqt, fill=Type), col=NA, alpha=0.4)+
	geom_line()+
	ylab("Probability of detection")+
	xlab(NULL)+
	ylim(0, 1)+
	scale_x_continuous(breaks = cumsum(c(1, 31, 30, 28, 30, 31, 30, 31, 31, 30, 31, 30)), 
										 label = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 
										 					'Jul', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec'), expand = c(0, 0))+
	annotate("text", x=Inf, y=Inf, label="A", vjust=1.2, hjust=1.1, size=10)+
	theme_bw()+
#	theme(axis.text.x=element_text(hjust=-1.8))+
	theme(legend.position=c(0.17, 0.83))+
	theme(legend.title=element_blank())+
	theme(legend.background=element_rect(colour="black", fill="white", size=0))+
	theme(legend.key=element_rect(colour=NA, size=0))


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
	BETA[,10]%*%Tdiff +
	BETA[,11]%*% (((Temp-22)/5)*Tdiff ) #interaction term.
#Summary stats in a curve....
Tcurve<-data.frame("x"=t(Temp), summary_func(plogis(Tpred)))

Tplot<-ggplot(Tcurve, aes(y=mean, x=x))+
	geom_ribbon(aes(ymin=lwr, ymax=upp), col=NA, fill="green", alpha=0.2)+
	geom_ribbon(aes(ymin=lqt, ymax=uqt), col=NA, fill="green", alpha=0.4)+
	geom_line(col="green")+
	ylab("Probability of detection")+
	xlab("T (tile)")+
	ylim(0, 1)+
	annotate("text", x=Inf, y=Inf, label="B", vjust=1.2, hjust=1.1, size=10)+
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
	annotate("text", x=Inf, y=Inf, label="C", vjust=1.2, hjust=1.1, size=10)+
	theme_bw()

#Really need a raster plot to properly visualise p ~ (T, Tdiff) relationship 


##raster plot of response

pred_data<-expand.grid(Temp=5:60, TempA=0:50, TOD=0) %>%
	        mutate(Tdiff=Temp-TempA)

beta_means<-colMeans(BETA)

yfrac<-10/12 #fraction of year
fourier.liz<-beta_means[2]*(cos(2*pi*yfrac))+beta_means[3]*(sin(2*pi*yfrac)) + 
	beta_means[4]*(cos(4*pi*yfrac))+beta_means[5]*(sin(4*pi*yfrac))

pred_detect<-function(predrow){
p.liz<-beta_means[1] +fourier.liz  + 
	beta_means[6]*predrow["TOD"]+
	beta_means[7]*(predrow["TOD"]^2)+  
	beta_means[8]*((predrow["Temp"]-22)/5) +
	beta_means[9]*((predrow["Temp"]-22)/5)^2 +
	beta_means[10]*predrow["Tdiff"] +   #effect of air-soil temp diff. 
	beta_means[11]*((predrow["Temp"]-22)/5)*predrow["Tdiff"]
return(plogis(p.liz))
}

out<-apply(pred_data, 1, pred_detect)
out<-cbind(pred_data, p=out)

dd<-data.frame(TempA=jags_dat$TempA, TempS=jags_dat$TempS, Tdiff=TempS-TempA, 
							 yfrac=jags_dat$year.frac, detect=factor(jags_dat$detect.liz)) %>%
	     filter(yfrac>9/12)



RASTPLOT<-ggplot(out, aes(y=Temp, x=TempA)) +
	geom_raster(aes(fill=p)) +
#	stat_contour(breaks=c(0.2, 0.6, 0.76), col="grey")+
	scale_fill_gradientn(colours=rev(terrain.colors(7)))+
	labs(y=expression("Tile temperature " ( degree~C)),
			 x=expression("Air temperature " ( degree~C))) +
#	labs('Air Temperature') +
	scale_x_continuous(limits=c(0, 40), expand = c(0, 0))+
	scale_y_continuous(limits=c(5, 50), expand = c(0, 0))+
#	geom_point(data=dd, aes(y=TempS, x=TempA, col=detect), alpha=0.8, size=0.4) +
	scale_color_manual(values=c("lightblue", "black"))+
	geom_abline(intercept=0, slope=1) +
	annotate("text", x=40, y=50, label="B", vjust=1.2, hjust=1.1, size=10)+
	theme_bw()+
	theme(legend.position=c(0.12, 0.74))+
	theme(legend.title=element_blank())+
theme(legend.background=element_rect(colour="black", fill="white", size=0))

#another version with temperature difference on y-axis
# out2<-out %>%
# 	filter(Tdiff>-20, Tdiff<30, TempA>5, TempA<40)
# 
# ggplot(out2, aes(x=TempA, y=Tdiff)) +
# 	geom_raster(aes(fill=pdet)) +
# 	#	stat_contour(breaks=c(0.2, 0.6, 0.76), col="grey")+
# 	scale_fill_gradientn(colours=rev(terrain.colors(7)))+
# 	xlab('Air temperature') +
# 	ylab('Temperature difference') +
# 	geom_point(data=dd, aes(x=TempA, y=Tdiff, col=detect), alpha=0.8, size=0.8) +
# 	scale_color_manual(values=c("darkgrey", "black"))+
# 	ylim(-20, 30)+
# 	geom_abline(intercept=0, slope=0) +
# 	theme_bw()

pdf("Figures/detection_plot.pdf", width=4, height=7)
grid.arrange(SP, RASTPLOT, ncol=1, nrow=2)
dev.off()

