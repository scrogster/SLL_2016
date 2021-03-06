require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)
require(viridis)
require(gridExtra)

load("fitted_model.Rdata")
dir.create("Figures", showWarnings = FALSE)
#plotting initial occupancy parameters
B<-data.frame(out$sims.list$B) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3, Grazing=X4, Fire.Grazing=X5, Clay=X6)

pred_dat<-expand.grid(Intercept=1, Grassland=c(0.2, 0.8), Fire=seq(0, 5, by=0.1), Grazing=seq(0, 3, by=0.1), Clay=c(10, 60))

coeff<-apply(B, 2, mean)

prob_occ<-plogis(coeff[1] + 
	               coeff[2]*((pred_dat$Grassland-0.076)/0.146)+
								 coeff[3]*pred_dat$Fire +
								 coeff[4]*pred_dat$Grazing +
								 coeff[5]*pred_dat$Grazing*pred_dat$Fire+
	               coeff[6]*((pred_dat$Clay-32.68)/16.77)
	              )

psi_curve<-data.frame(prob_occ, pred_dat)
psi_curve$GrasslandType<-ifelse(psi_curve$Grassland==0.2, "Grassland=0.2", "Grassland=0.8")
psi_curve$ClayType<-ifelse(psi_curve$Clay==10, "Clay=10", "Clay=60")


OCCGRAPH<-ggplot(psi_curve, aes(x=Fire, y=Grazing)) +
	geom_raster(aes(fill=prob_occ), interpolate=FALSE)+
	facet_grid(GrasslandType~ClayType)+
#	geom_contour(aes(x=Fire, y=Grazing, z=prob_occ), breaks=0.25, col="black", linetype="dashed")+
#	geom_contour(aes(x=Fire, y=Grazing, z=prob_occ), breaks=0.75, col="black", linetype="solid")+
	xlab("Fire") +
	ylab("Grazing") +
	scale_x_continuous(expand=c(0, 0))+
	scale_y_continuous(expand=c(0, 0))+
	scale_fill_viridis(name=~psi[1], breaks=c(0, 0.25, 0.5, 0.75, 1), direction=1, limits=c(0,1))+
	ggtitle("a. Probability of initial occupancy")+
	theme_bw()+
	theme(strip.background =element_rect(fill="white"))+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#plotting the persistence parameters
C<-data.frame(out$sims.list$C) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3, Grazing=X4, Fire.Grazing=X5, Clay=X6)
pred_dat<-expand.grid(Intercept=1, Grassland=c(0.2, 0.8), Fire=seq(0, 5, by=0.1), Grazing=seq(0, 3, by=0.1), Clay=c(10, 60))

coeff<-apply(C, 2, mean)

prob_persist<-plogis(coeff[1] + 
										 	coeff[2]*((pred_dat$Grassland-0.076)/0.146)+
										 	coeff[3]*pred_dat$Fire +
										 	coeff[4]*pred_dat$Grazing +
										 	coeff[5]*pred_dat$Grazing*pred_dat$Fire+
										 	coeff[6]*((pred_dat$Clay-32.68)/16.77)
)

phi_curve<-data.frame(prob_persist, pred_dat)
phi_curve$GrasslandType<-ifelse(phi_curve$Grassland==0.2, "Grassland=0.2", "Grassland=0.8")
phi_curve$ClayType<-ifelse(phi_curve$Clay==10, "Clay=10", "Clay=60")

PERSISTGRAPH<-ggplot(phi_curve, aes(x=Fire, y=Grazing)) +
	geom_raster(aes(fill=prob_persist), interpolate=FALSE)+
	facet_grid(GrasslandType~ClayType)+
	geom_contour(aes(x=Fire, y=Grazing, z=prob_persist), breaks=0.25, col="black", linetype="dashed")+
	geom_contour(aes(x=Fire, y=Grazing, z=prob_persist), breaks=0.5, col="black", linetype="solid")+
	xlab("Fire") +
	ylab("Grazing") +
	scale_fill_viridis(name=~phi, breaks=c(0, 0.25, 0.5, 0.75, 1), direction=1, limits=c(0, 1))+
	scale_x_continuous(expand=c(0, 0))+
	scale_y_continuous(expand=c(0, 0))+
	ggtitle("b. Annual probability of persistence")+
	theme_bw()+
	theme(strip.background =element_rect(fill="white"))+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

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
	geom_ribbon(aes(ymin=lwr, ymax=upp), col=NA, fill="#21908CFF", alpha=0.4)+
	geom_ribbon(aes(ymin=lqt, ymax=uqt), col=NA, fill="#21908CFF", alpha=0.6)+
	geom_line()+
	ylab(expression(gamma))+
	xlab("Proportion of grassland")+
	ggtitle("c. Annual probability of colonisation")+
	theme_bw()+
	theme(strip.background =element_rect(fill="white"))+
	theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

pdf("Figures/response_plot.pdf", width=5, height=10)
grid.arrange(OCCGRAPH, PERSISTGRAPH, COLGRAPH, ncol=1, nrow=3)
dev.off()

png("Figures/response_plot.png", width=5, height=10, units="in", res=450)
grid.arrange(OCCGRAPH, PERSISTGRAPH, COLGRAPH, ncol=1, nrow=3)
dev.off()