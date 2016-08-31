require(jagsUI)
require(lubridate)
require(dplyr)

load("formatted_for_JAGS_Pseudpag.Rdata")

modfile <- 'R/single_occmod.txt'
params <- c('B', 'C', 'D', 'BETA', 
						   'Numocc', 'Ext_t', 'Col_t', 'Deficit')

z.init<-
	tapply(DelmaFiltered$PseudPag, 
				 list(DelmaFiltered$GridCMA, DelmaFiltered$yearnum), 
				 function(x){min(sum(x), 1)  }  ) 
z.init[z.init==0]<-NA #just assign one to years with known occurences. otherwise NA for init of Z.

inits <- function(){  
	list(B=rnorm(4,0,1),
			 C=rnorm(5,0,1),
			 D=rnorm(2,0,1), 
			 BETA=rnorm(9,0,0.2),
			 Z=z.init)
}

out <- jags(data = jags_dat,
						parameters.to.save = params,
						inits=inits,
						model.file = modfile,
						parallel=TRUE, 
						n.chains = 3,
						n.adapt = 250,
						n.iter = 2000,
						n.burnin = 500,
						n.thin = 1)

out

save.image("fitted_model_PseudPag.Rdata")


