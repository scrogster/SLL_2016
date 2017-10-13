

This git repository contains all data and code associated with the manuscript: 

Scroggie, M.P, Peterson, G.N.L., Rohr, D.H., Nicholson, E. & Heard, G.W. "Fragmentation and disturbance drive the occupancy dynamics of a cryptic grassland reptile: evidence from a long-term, spatially extensive study".

## Dependencies

A number of *R* packages and their dependencies are required to run the analysis and generate the figures, tables and manuscript. These can be installed by typing the following command into R.

```
install.packages(readxl, dplyr, tidyr, lubridate, ggplot2, stringr,  sp, maptools, rgdal, jagsUI, rmarkdown, knitr)
```

Just another Gibbs Sampler (JAGS) version 4.3.0 is used to fit the Bayesian dynamic occupancy model to the data using Markov Chain Monte Carlo methods, and should be downloaded and installed from:

[https://sourceforge.net/projects/mcmc-jags/files/](https://sourceforge.net/projects/mcmc-jags/files/)


## Reproducing the analysis

Beware that running the full analysis takes several hours, mainly due to the time required for the model to converge satisfactorily in *JAGS*. To reduce the required time for model fitting, four Markov chains are run in parallel on seperate processor cores using the parallel processing facilities provided by the *R* package [*jagsUI*](https://cran.r-project.org/web/packages/jagsUI/index.html)

The full analysis, including generating the figures, tables and manuscript can be reproduced by opening a shell, and typing:

```
make
```



 
