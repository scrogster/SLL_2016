

This repository contains all data and code necessary to reproduce the numerical results contained in: 

Scroggie, M.P, Peterson, G.N.L., Rohr, D.H., Nicholson, E. & Heard, G.W. "Disturbance has benefits as well as costs for fragmented populations of a cryptic grassland reptile".

## Install required dependencies

A number of *R* packages and their dependencies are required to run the analysis and generate the figures and table. These can be installed by typing the following command into R.

```
install.packages(readxl, dplyr, tidyr, lubridate, ggplot2, stringr,  gridExtra, sp, maptools, rgdal, jagsUI, rmarkdown, knitr)
```

Just another Gibbs Sampler (JAGS) is used to fit the Bayesian dynamic occupancy model to the data using Markov Chain Monte Carlo methods, and should be downloaded and installed from:

[https://sourceforge.net/projects/mcmc-jags/files/](https://sourceforge.net/projects/mcmc-jags/files/)


The *Makefile* scripts all required steps to format the data, fit the statistical model and generate the tables and figures.

```
make
```



 
