require(jagsUI)
require(lubridate)
require(dplyr)

load("formatted_for_JAGS.Rdata")

#MCMC settings
n.chains=4
n.adapt=2000
n.burnin=10000
n.iter=10000+n.burnin
n.thin=4
para=TRUE

#Random seed
set.seed(435)

modfile <- 'R/prototype_occmod.txt'
params <- c('B', 'C', 'D', 'BETA', 
						   'Numocc', 'Ext_t', 'Col_t', 'Deficit', 'numexi', 'numcoli')

z.init<-
	tapply(DelmaFiltered$DelmaLizards+DelmaFiltered$DelmaOther, 
				 list(DelmaFiltered$GridCMA, DelmaFiltered$yearnum), 
				 function(x){min(sum(x), 1)  }  ) 

#function to assign initial occupancy states to the Z matrix.
z.initiator<-function(){
z.init<-
	tapply(DelmaFiltered$DelmaLizards+DelmaFiltered$DelmaOther, 
				 list(DelmaFiltered$GridCMA, DelmaFiltered$yearnum), 
				 function(x){min(sum(x), 1)  }  ) 
z.init[z.init==0]<-NA #just assign one to years with known occurences. otherwise NA for init of Z.
#number of sites that are NA
N.na<-length(z.init[is.na(z.init)])
z.init[is.na(z.init)]<-rbinom(N.na, 1, 0.1)
return(z.init)
}

#function to generate starting values
inits <- function(){  
	list(B=rnorm(5,0,0.2),
			 C=rnorm(5,0,0.2),
			 D=rnorm(2,0,0.2), 
			 BETA=rnorm(16,0,0.2),
			 Z=z.initiator()  )
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


