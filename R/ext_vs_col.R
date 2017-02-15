require(jagsUI)
require(ggplot2)
require(dplyr)
require(tidyr)

load("fitted_model.Rdata")

ever.detect<-as.numeric(rowSums(z.init, na.rm=TRUE)>0)

#posterior mean number of extinctions per site
df.numexi<-data.frame(out$sims.list$numexi)
post.mean.exts<-apply(df.numexi, 2, function(x){mean(x)})

#posterior mean number of colonisations per site
df.numcoli<-data.frame(out$sims.list$numcoli)
post.mean.cols<-apply(df.numcoli, 2, function(x){mean(x)})

extcol<-data.frame(post.mean.exts, post.mean.cols, ever.detect, grassjoin)

#initial attempt at plotting extinction vs colonisation events

ggplot(extcol, aes(x=post.mean.exts, y=post.mean.cols, size=grasstot)) +
	geom_point(col='green') +
	facet_grid(cut(extcol$grasstot, 3)~ever.detect) +
	xlab("Posterior mean number of extinctions") +
	ylab("Posterior mean number of colonisations") +
	theme_bw()
ggsave("Figures/ext_vs_col.pdf", width=7, height=5)

#mapping in space will also be worthwhile - are the extinction and colonisation clustered in space?

