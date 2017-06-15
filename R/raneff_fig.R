library(jagsUI)
library(coda)
library(ggplot2)
library(dplyr)
load("fitted_model.Rdata")

samples<-out$samples
paramnames<-names(as.data.frame(samples[[1]]))

extract_param<-function(paramname){
	out<-data.frame(as.matrix(samples[,paramname], iters=TRUE, chains=TRUE))
	out<-data.frame(out, "Parameter"=paramname)
	out$CHAIN<-factor(out$CHAIN)
	return(out)
}


clusvar<-rbind(
	extract_param("clus_var_occ"),
	extract_param("clus_var_persist"),
	extract_param("clus_var_colon"))


clusvar$Parameter<-	recode(clusvar$Parameter, "clus_var_occ"="occupancy", "clus_var_persist"="persistence", "clus_var_colon"="colonisation" )


ggplot(clusvar, aes(x=var1, colour=Parameter, fill=Parameter))+
	geom_density(alpha=0.4)+
	ylab("Density")+
	xlab(quote(sigma))+
	theme_bw()+
	theme(legend.background=element_rect(colour="black", fill="white", size=0))+
	theme(legend.key=element_rect(colour=NA, size=0))+
  theme(legend.position=c(0.85, 0.87))+
	theme(legend.title=element_blank())
ggsave("Figures/raneff_fig.pdf", width=4.5, height=4.5)

