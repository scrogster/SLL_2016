require(jagsUI)
require(lubridate)
require(dplyr)

load("formatted_for_JAGS.Rdata")

modfile <- 'R/prototype_occmod.txt'
params <- c('B', 'C', 'D', 'BETA')

z.init<-
	tapply(DelmaFiltered$DelmaLizards+DelmaFiltered$DelmaOther, 
				 list(DelmaFiltered$GridCMA, DelmaFiltered$yearnum), 
				 function(x){min(sum(x), 1)  }  ) 
z.init[z.init==0]<-NA #just assign one to years with known occurences. otherwise NA for init.

inits <- function(){  
	list(B=rnorm(4,0,1),
			 C=rnorm(4,0,1),
			 D=rnorm(4,0,1), 
			 BETA=rnorm(14,0,0.2),
			 Z=z.init)
}


out <- jags(data = jags_dat,
						parameters.to.save = params,
						inits=inits,
						model.file = modfile,
						parallel=FALSE, 
						n.chains = 3,
						n.adapt = 200,
						n.iter = 2000,
						n.burnin = 500,
						n.thin = 5)

save.image("fitted_model.Rdata")

# plot(out)
# 
# #trial plotting of seasonal response curves 
# BETA<-colMeans(out$sims.list$BETA)
# 
# x<-seq(0, 1, by=0.01)
# aa<-BETA[1]+BETA[2]*(cos(2*pi*x))+BETA[3]*(sin(2*pi*x)) + 
# 	BETA[4]*(cos(4*pi*x))+BETA[5]*(sin(4*pi*x))
# 
# bb<-BETA[10]+BETA[11]*(cos(2*pi*x))+BETA[12]*(sin(2*pi*x))+ 
# 	BETA[13]*(cos(4*pi*x))+BETA[14]*(sin(4*pi*x))
# 
# pdf("Figures/detection_curve.pdf", width=8, height=8)
# plot(plogis(aa)~x, col="red", type="l", ylim=c(0, 1), lwd=2, ylab="Pr(detect)",
# 		 xlab="Fraction of Calendar Year", las=1)
# lines(y=plogis(bb), x=x, col="blue", lwd=2)
# lines(y = 1- (1-plogis(aa))*(1-plogis(bb)), x=x, col="green", lwd=2)
# title(main=c("Red=lizards, Blue=sloughs, Green=combined"))
# dev.off()

