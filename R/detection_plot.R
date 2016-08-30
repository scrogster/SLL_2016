require(jagsUI)
require(ggplot2)
require(ggmcmc)

load("fitted_model.Rdata")

#plotting the detection curves

 #trial plotting of seasonal response curves. no uncertainty yet.
 BETA<-colMeans(out$sims.list$BETA)
 
 x<-seq(0, 1, by=0.01)

#curve for lizards
aa<-BETA[1]+BETA[2]*(cos(2*pi*x))+BETA[3]*(sin(2*pi*x)) + 
 	          BETA[4]*(cos(4*pi*x))+BETA[5]*(sin(4*pi*x))

#curve for skins, eggs etc
bb<-BETA[10]+BETA[11]*(cos(2*pi*x))+BETA[12]*(sin(2*pi*x))+ 
 	           BETA[13]*(cos(4*pi*x))+BETA[14]*(sin(4*pi*x))

#combined curve (assuming conditional independence between two types of sightings)
cc<-  1-(1-plogis(aa))*(1-plogis(bb))

pdf("Figures/detection_plot.pdf", width=8, height=8)
plot(plogis(aa)~x, col="red", type="l", ylim=c(0, 1), lwd=2, ylab="Pr(detect)",
 		 xlab="Fraction of Calendar Year", las=1, axes=FALSE)
 lines(y=plogis(bb), x=x, col="blue", lwd=2)
 lines(y =cc, x=x, col="green", lwd=2)
 axis(2, las=1)
 axis(1, at=seq(0, 12)/12, labels=FALSE)
 box()
 title(main="Seasonal variation in probability of detection")
 legend(x=0, y=0.9, legend=c("Lizards", "Sloughs", "Combined"), col=c("red", "blue", "green"), lty=1)
dev.off()

