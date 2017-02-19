require(jagsUI)
require(lubridate)
require(dplyr)

load("formatted_for_JAGS.Rdata")

#MCMC settings
n.chains=4
n.adapt=500
n.iter=5000
n.burnin=2500
n.thin=5
para=TRUE



modfile <- 'R/prototype_occmod.txt'
params <- c('B', 'C', 'D', 'BETA', 
						   'Numocc', 'Ext_t', 'Col_t', 'Deficit', 'numexi', 'numcoli',
						'clus_var_occ', 'clus_var_persist', 'clus_var_colon')

z.init<-
	tapply(DelmaFiltered$DelmaLizards+DelmaFiltered$DelmaOther, 
				 list(DelmaFiltered$GridCMA, DelmaFiltered$yearnum), 
				 function(x){min(sum(x), 1)  }  ) 
z.init[z.init==0]<-NA #just assign one to years with known occurences. otherwise NA for init of Z.


inits <- function(){  
	list(B=rnorm(5,0,1),
			 C=rnorm(5,0,1),
			 D=rnorm(2,0,1), 
			 BETA=rnorm(14,0,0.2),
			 Z=z.init,
			 clus_var_occ=runif(1, 0, 5),
			 clus_var_persist=runif(1, 0, 5))
}

out <- jags(data = jags_dat,
						parameters.to.save = params,
						inits=inits,
						model.file = modfile,
						parallel=para, 
						n.chains = n.chains,
						n.adapt = n.adapt,
						n.iter = n.iter,
						n.burnin = n.burnin,
						n.thin = n.thin)

save.image("fitted_model.Rdata")


