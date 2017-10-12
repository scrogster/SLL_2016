require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)
require(gridExtra)

load("fitted_model.Rdata")

#plotting initial occupancy parameters
B<-data.frame(out$sims.list$B) %>%
	rename(Intercept=X1, Grassland=X2, Clay=X3, Roadside=X4)

pred_dat<-expand.grid(Intercept=1, Grassland=seq(0, 1.0, by=0.01), Clay=seq(0, 70, by=0.02), Roadside=c(0, 1))

coeff<-apply(B, 2, mean)

prob_occ<-plogis(coeff[1] + 
	    coeff[2]*((pred_dat$Grassland-0.076)/0.146)+
	    coeff[3]*((pred_dat$Clay-32.68)/16.77)+ 
	    coeff[4]*pred_dat$Roadside)

psi_curve<-data.frame(prob_occ, pred_dat)
psi_curve$Type<-ifelse(psi_curve$Roadside==0, "Non-roadside", "Roadside")

OCCGRAPH<-ggplot(psi_curve, aes(x=Grassland, y=Clay)) +
	geom_raster(aes(fill=prob_occ))+
	facet_wrap(~Type)+
	xlab("Proportion of grassland within 1000 m") +
	ylab("Soil clay (%)") +
	scale_fill_distiller(type="seq", palette="Greys",  direction=1, name=~psi[1])+
	ggtitle("A. Probability of initial occupancy")+
	theme_bw()


#plotting the persistence parameters
C<-data.frame(out$sims.list$C) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3,  Graze=X4)

pred_dat<-expand.grid(Intercept=1, Grassland=seq(0, 1.0, by=0.05), Fire=seq(0, 6, by=0.02), Graze=seq(0, 4, by=0.02))

coeff<-apply(C, 2, mean)

prob_persist<- plogis( coeff[1]+ 
	coeff[2]*((pred_dat$Grassland-0.076)/0.146)+  #roughly centered/scaled grassland area
	coeff[3]*pred_dat$Fire +                 #fire score
	coeff[4]*pred_dat$Graze  )               #grazing score
phi_curve<-data.frame(prob_persist, pred_dat)

phi_curve<-phi_curve %>% 
	filter(Grassland %in% c(0.25, 0.75)) %>%
	mutate(Grasslevel=ifelse(Grassland==0.25, "Grassland=0.25", "Grassland=0.75"))
PERSISTGRAPH<-ggplot(phi_curve, aes(x=Graze, y=Fire)) +
	geom_raster(aes(fill=prob_persist))+
	facet_wrap(~Grasslevel)+
	xlab("Grazing intensity score")+
	ylab("Number of times burnt") +
	scale_fill_distiller(type="seq", palette="Greys",  direction=1, name=~phi)+
	ggtitle("B. Annual probability of persistence")+
	theme_bw()

#plotting the colonisation parameters

D<-data.frame(out$sims.list$D) %>%
	rename(Intercept=X1, Grassland=X2)

pred_dat<-expand.grid(Intercept=1, Grassland=seq(0, 1, by=0.05))

Colpred<-plogis(D[,1]+D[,2]%*%t((pred_dat$Grassland-0.076)/0.146))
summary_func<-function(x){
	post.mean=apply(x, 2, mean)
	lwr.ci=apply(x, 2, quantile, 0.025)
	upp.ci=apply(x, 2, quantile, 0.975)
	lwr.qt=apply(x, 2, quantile, 0.25)
	upp.qt=apply(x, 2, quantile, 0.75)
	out=data.frame("mean"=post.mean, "lwr"=lwr.ci, "upp"=upp.ci, "lqt"=lwr.qt, "uqt"=upp.qt)
	return(out)
}

col_curve<-data.frame(Grassland=pred_dat$Grassland, summary_func(Colpred))
COLGRAPH<-ggplot(col_curve, aes(x=Grassland, y=mean))+
	geom_ribbon(aes(ymin=lwr, ymax=upp), col=NA, fill="black", alpha=0.2)+
	geom_ribbon(aes(ymin=lqt, ymax=uqt), col=NA, fill="black", alpha=0.4)+
	geom_line()+
	ylab(expression(gamma))+
	xlab("Proportion of grassland")+
	ggtitle("C. Annual probability of colonisation")+
	theme_bw()

pdf("Figures/response_plot.pdf", width=6, height=8)
grid.arrange(OCCGRAPH, PERSISTGRAPH, COLGRAPH, ncol=1, nrow=3)
dev.off()