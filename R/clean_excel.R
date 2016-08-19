require(readxl)
require(dplyr)
require(tidyr)
require(lubridate)

wcma<-read_excel("DataFromGarry/SLL monitoring database formatted 13May13.xls", 1)
ccma<-read_excel("DataFromGarry/SLL monitoring database formatted 13May13.xls", 2)
ghcma<-read_excel("DataFromGarry/SLL monitoring database formatted 13May13.xls", 3)

#filter out blank lines
wcma <- wcma %>% filter(!is.na(`Grid No`)) %>% select (-`Time End (EST)`) %>% mutate(CMA="wcma", Species=trimws(Species))
ccma <- ccma %>% filter(!is.na(`Grid No`)) %>% select (-`Time End (EST)`) %>% mutate(CMA="ccma", Species=trimws(Species))
ghcma <- ghcma %>% filter(!is.na(`Grid No`)) %>% select (-`Time End (EST)`) %>% mutate(CMA = "ghcma", Species=trimws(Species))

combined_raw<-rbind(wcma, ccma, ghcma)


#first cut of a filtering pipe for the data.

#The following code summarises each survey's results - survey level vars, and detections of various spp.
DelmaFiltered<- combined_raw %>%
	select(Grid=`Grid No`, Cluster, CMA, Date, Season=`Field Season`,
				 T1=`Time Start (EST)`,
				 TempA1=`Temp (a) Start`, TempA2=`Temp (a) End`, TempS1=`Temp (s) Start`, TempS2=`Temp (s) End`,
				 HumidA1=`Humidity (a) Start`, HumidA2=`Humidity (a) End`, HumidS1=`Humidity (s) Start`,  HumidS2=`Humidity (s) End`,
				 Sun1=`Sun Start`,  Sun2=`Sun End`,
				 Cloud1=`Cloud Cover Start`, Cloud2=`Cloud Cover End`,
				 `Species`, `Number`, `Evidence`) %>%
	 select(Grid, Cluster, CMA, Season, Date, Time=T1, AirTemp = TempA1, SoilTemp=TempS1, 
	 			 HumidA=HumidA1, HumidS=HumidS1, Sun=Sun2, Cloud=Cloud1, Species, Number, Evidence) %>%
	   mutate(GridCMA = paste0(Grid, CMA), Number=as.numeric(gsub("[^1-9]", "", Number))  ) %>%
	group_by(CMA, GridCMA, Date) %>%
	summarise(Time=first(Time), Grid=first(Grid), Cluster=first(Cluster), Season=first(Season),
						AirTemp=first(AirTemp), SoilTemp=first(SoilTemp), HumidA=first(HumidA), HumidS=first(HumidS), 
						Sun=first(Sun), Cloud=first(Cloud), 
						DelmaLizards = sum(Number[Species=="Delma impar" & is.na(Evidence)], na.rm=TRUE),
						DelmaOther = sum(Number[Species=="Delma impar" & !is.na(Evidence)] , na.rm=TRUE),
						SutaFlag = sum(Number[Species=="Suta flagellum"], na.rm=TRUE),
						BassDup = sum(Number[grep("duperreyi", Species)], na.rm=TRUE),
						PseudPag = sum(Number[grep("pagenstech", Species)], na.rm=TRUE),
						Sminthopsis = sum(Number[grep("Sminth", Species)], na.rm=TRUE)
						)%>% 
	  mutate(yearnum = year(Date)-2003, 
	  			 sitenum=as.numeric(factor(GridCMA)),
	  			 yeardayfrac=yday(Date)/365 )%>%   #fraction of the calendar year
	arrange(GridCMA, CMA, Cluster, Grid, Date)

save.image("prepped_data.Rdata")

