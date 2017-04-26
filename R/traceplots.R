

library(jagsUI)
library(coda)
library(ggplot2)
load("fitted_model.Rdata")

samples<-out$samples
paramnames<-names(as.data.frame(samples[[1]]))

extract_param<-function(paramname){
out<-data.frame(as.matrix(samples[,paramname], iters=TRUE, chains=TRUE))
out<-data.frame(out, "Parameter"=paramname)
out$CHAIN<-factor(out$CHAIN)
return(out)
}



BB<-rbind(
extract_param("B[1]"),
extract_param("B[2]"),
extract_param("B[3]"),
extract_param("B[4]"),
extract_param("B[5]"))

CC<-rbind(
	extract_param("C[1]"),
	extract_param("C[2]"),
	extract_param("C[3]"),
	extract_param("C[4]"),
	extract_param("C[5]"),
	extract_param("C[6]"))

DD<-rbind(
	extract_param("D[1]"),
	extract_param("D[2]"))

BETABETA<-rbind(
	extract_param("BETA[1]"),
	extract_param("BETA[2]"),
	extract_param("BETA[3]"),
	extract_param("BETA[4]"),
	extract_param("BETA[5]"),
	extract_param("BETA[6]"),
	extract_param("BETA[7]"),
	extract_param("BETA[8]"),
	extract_param("BETA[9]"),
	extract_param("BETA[10]"),
	extract_param("BETA[11]"),
	extract_param("BETA[12]"),
	extract_param("BETA[13]"),
	extract_param("BETA[14]"),
	extract_param("BETA[15]"))


clusvar<-rbind(
	         extract_param("clus_var_occ"),
	         extract_param("clus_var_persist"),
	         extract_param("clus_var_colon"))

dir.create("Traceplots", showWarnings = FALSE)

ggplot(clusvar, aes(x=ITER, y=var1, colour=CHAIN))+
	geom_line(size=0.1, alpha=0.4)+
	facet_wrap(~Parameter)+
	ylab("Parameter value")+
	xlab("Iteration")+
	theme_bw()
ggsave("Traceplots/clusvar.pdf", width=10, height=4)

ggplot(clusvar, aes(x=var1, colour=CHAIN, fill=CHAIN))+
	geom_density(alpha=0.4)+
	facet_wrap(~Parameter, scales="free_y")+
	ylab("Density")+
	xlab("Value")+
	theme_bw()
ggsave("Traceplots/clusvardensity.pdf", width=10, height=4)


ggplot(BB, aes(x=ITER, y=var1, colour=CHAIN))+
	geom_line(size=0.1, alpha=0.4)+
	geom_hline(yintercept=0, lty=2)+
	facet_wrap(~Parameter)+
	ylab("Parameter value")+
	xlab("Iteration")+
	theme_bw()
ggsave("Traceplots/initocc_trace.pdf", width=10, height=4)

ggplot(BB, aes(x=var1, colour=CHAIN, fill=CHAIN))+
	geom_density(alpha=0.4)+
	geom_vline(xintercept=0, lty=2)+
	facet_wrap(~Parameter, scales="free_y")+
	ylab("Density")+
	xlab("Parameter value")+
	theme_bw()
ggsave("Traceplots/initocc_density.pdf", width=10, height=4)


ggplot(CC, aes(x=ITER, y=var1, colour=CHAIN))+
	geom_line(size=0.1, alpha=0.4)+
	geom_hline(yintercept=0, lty=2)+
	facet_wrap(~Parameter)+
	ylab("Parameter value")+
	xlab("Iteration")+
	theme_bw()
ggsave("Traceplots/persist_trace.pdf", width=10, height=6)

ggplot(CC, aes(x=var1, colour=CHAIN, fill=CHAIN))+
	geom_density(alpha=0.4)+
	geom_vline(xintercept=0, lty=2)+
	facet_wrap(~Parameter, scales="free_y")+
	ylab("Parameter value")+
	xlab("Value")+
	theme_bw()
ggsave("Traceplots/persist_density.pdf", width=10, height=6)

ggplot(DD, aes(x=ITER, y=var1, colour=CHAIN))+
	geom_line(size=0.1, alpha=0.4)+
	geom_hline(yintercept=0, lty=2)+
	facet_wrap(~Parameter)+
	ylab("Parameter value")+
	xlab("Iteration")+
	theme_bw()
ggsave("Traceplots/colonise_trace.pdf", width=10, height=2)

ggplot(DD, aes(x=var1, colour=CHAIN, fill=CHAIN))+
	geom_density(alpha=0.4)+
	geom_vline(xintercept=0, lty=2)+
	facet_wrap(~Parameter, scales="free_y")+
	ylab("Parameter value")+
	xlab("Value")+
	theme_bw()
ggsave("Traceplots/colonise_density.pdf", width=10, height=2)

ggplot(BETABETA, aes(x=ITER, y=var1, colour=CHAIN))+
	geom_line(size=0.1, alpha=0.4)+
	geom_hline(yintercept=0, lty=2)+
	facet_wrap(~Parameter)+
	ylab("Parameter value")+
	xlab("Iteration")+
	theme_bw()
ggsave("Traceplots/beta_trace.pdf", width=10, height=8)

ggplot(BETABETA, aes(x=var1, colour=CHAIN, fill=CHAIN))+
	geom_density(alpha=0.4)+
	geom_vline(xintercept=0, lty=2)+
	facet_wrap(~Parameter, scales="free_y")+
	ylab("Parameter value")+
	xlab("Value")+
	theme_bw()
ggsave("Traceplots/beta_density.pdf", width=10, height=8)



