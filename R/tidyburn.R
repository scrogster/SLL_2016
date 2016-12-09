

#script to tidy the fire data in Garry's spreadsheet BurnInfoWorkingCopy.xlsx

library(readxl)
library(dplyr)
library(tidyr)


burndata<-read_excel("DataFromGarry/BurnInfoWorkingCopy.xlsx")

names(burndata)


tidyburn<-burndata %>%
	select(Grid, Summer_2004:Autumn_2013) %>%
	gather( SeasonYear , status, Summer_2004:Autumn_2013) %>%
	separate(SeasonYear , into = c("Season", "Year"), sep = "_") 

#kind of OK, but is there repetition of grid names between CMAs?

#my other data uses a gridnumber-cma pair to uniquely id sites.

load("fitted_model.Rdata")
head(grassjoin)