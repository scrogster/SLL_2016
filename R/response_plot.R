require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)
require(gridExtra)

load("fitted_model.Rdata")

#plotting initial occupancy parameters
B<-data.frame(out$sims.list$B) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3, Grazing=X4, Clay=X5)

pred_dat<-expand.grid(Intercept=1, Grassland=seq(0, 1.0, by=0.05), Fire=c(0, 5), Grazing=c(0, 3), Clay=seq(0, 70, by=1))

coeff<-apply(B, 2, mean)

prob_occ<-plogis(coeff[1] + 
	               coeff[2]*((pred_dat$Grassland-0.076)/0.146)+
								 coeff[3]*pred_dat$Fire +
								 coeff[4]*pred_dat$Grazing +
	               coeff[5]*((pred_dat$Clay-32.68)/16.77)
	              )

psi_curve<-data.frame(prob_occ, pred_dat)
psi_curve$FireType<-ifelse(psi_curve$Fire==0, "Fire=0", "Fire=5")
psi_curve$GrazeType<-ifelse(psi_curve$Grazing==0, "Grazing=0", "Grazing=3")

OCCGRAPH<-ggplot(psi_curve, aes(x=Grassland, y=Clay)) +
	geom_raster(aes(fill=prob_occ), interpolate=TRUE)+
	facet_grid(FireType~GrazeType)+
	geom_contour(aes(x=Grassland, y=Clay, z=prob_occ), breaks=0.5, col="black")+
	geom_contour(aes(x=Grassland, y=Clay, z=prob_occ), breaks=0.75, col="black", linetype="dashed")+
	xlab("Proportion of grassland within 1000 m") +
	ylab("Soil clay (%)") +
	scale_fill_distiller(type="seq", palette='OrRd',  direction=1, name=~psi[1])+
	ggtitle("A. Probability of initial occupancy")+
	theme_bw()+
	theme(strip.background =element_rect(fill="white"))


#plotting the persistence parameters
C<-data.frame(out$sims.list$C) %>%
	rename(Intercept=X1, Grassland=X2, Fire=X3, Grazing=X4, Clay=X5)

pred_dat<-expand.grid(Intercept=1, Grassland=c(0.2, 0.8), Fire=seq(0, 6, by=0.02), Grazing=seq(0, 6, by=0.02), Clay=50)

coeff<-apply(C, 2, mean)

prob_persist<-plogis(coeff[1] + 
								 	coeff[2]*((pred_dat$Grassland-0.076)/0.146)+
								 	coeff[3]*pred_dat$Fire +
								 	coeff[4]*pred_dat$Grazing +
								 	coeff[5]*((pred_dat$Clay-32.68)/16.77)
)


phi_curve<-data.frame(prob_persist, pred_dat)
phi_curve$GrassType<-ifelse(phi_curve$Grassland==0.2, "Grassland=0.2", "Grassland=0.8")



PERSISTGRAPH<-ggplot(phi_curve, aes(x=Fire, y=Grazing)) +
	geom_raster(aes(fill=prob_persist), interpolate=TRUE)+
	facet_grid(.~GrassType)+
	geom_contour(aes(x=Fire, y=Grazing, z=prob_persist), breaks=0.5, col="black")+
	geom_contour(aes(x=Fire, y=Grazing, z=prob_persist), breaks=0.75, col="black", linetype="dashed")+
	xlab("Fire score") +
	ylab("Grazing score") +
	scale_fill_distiller(type="seq", palette='OrRd',  direction=1, name=~phi[1])+
	ggtitle("B. Annual probability of persistence")+
	theme_bw()+
	theme(strip.background =element_rect(fill="white"))

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
	geom_ribbon(aes(ymin=lwr, ymax=upp), col=NA, fill="red", alpha=0.4)+
	geom_ribbon(aes(ymin=lqt, ymax=uqt), col=NA, fill="red", alpha=0.6)+
	geom_line()+
	ylab(expression(gamma))+
	xlab("Proportion of grassland")+
	ggtitle("C. Annual probability of colonisation")+
	theme_bw()+
	theme(strip.background =element_rect(fill="white"))

pdf("Figures/response_plot.pdf", width=6, height=8)
grid.arrange(OCCGRAPH, PERSISTGRAPH, COLGRAPH, ncol=1, nrow=3)
dev.off()