require(readxl)
require(dplyr)
require(tidyr)
require(lubridate)

wcma<-read_excel("DataFromGarry/SLL monitoring database formatted 13May13.xls", 1, 
								 col_types=c(rep("guess", 8), "date", "date", rep("guess", 27))) 
ccma<-read_excel("DataFromGarry/SLL monitoring database formatted 13May13.xls", 2, 
								 col_types=c(rep("guess", 8), "date", "date", rep("guess", 27))) 
ghcma<-read_excel("DataFromGarry/SLL monitoring database formatted 13May13.xls", 3, 
									col_types=c(rep("guess", 8), "date", "date", rep("guess", 27))) 

#own trimws, so don't need newer R/dplyr
trim_ws<-function (x, which = c("both", "left", "right")) 
{
	which <- match.arg(which)
	mysub <- function(re, x) sub(re, "", x, perl = TRUE)
	if (which == "left") 
		return(mysub("^[ \t\r\n]+", x))
	if (which == "right") 
		return(mysub("[ \t\r\n]+$", x))
	mysub("[ \t\r\n]+$", mysub("^[ \t\r\n]+", x))
}

#filter out blank lines
wcma <- wcma %>% filter(!is.na(`Grid No`)) %>% 
	select (-`Time End (EST)`) %>% 
	mutate(CMA="wcma", Species=trim_ws(Species))
ccma <- ccma %>% filter(!is.na(`Grid No`)) %>% 
	select (-`Time End (EST)`) %>% 
	mutate(CMA="ccma", Species=trim_ws(Species))
ghcma <- ghcma %>% filter(!is.na(`Grid No`)) %>% 
	select (-`Time End (EST)`) %>% 
	mutate(CMA = "ghcma", Species=trim_ws(Species))

#merge the three data files together
combined_raw<-rbind(wcma, ccma, ghcma)

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
	arrange(GridCMA, CMA, Cluster, Grid, Date) %>%
	group_by(GridCMA) %>%
	mutate(FirstSurveyDate=min(Date)) %>% #this is a variable giving the first survey date for each site
	ungroup() %>%
	mutate(WeeksSinceFirstSurvey=as.numeric(difftime(Date, FirstSurveyDate, units="weeks")))

#these sites to be dropped - only single surveys, and no site coords or other information available.
dropsites<-c("17.3.1ghcma", "17.6.1ghcma", "10.3.2ccma", "10.3.3ccma")

DelmaFiltered<-DelmaFiltered %>%
	          filter(!GridCMA %in% dropsites )
	

save.image("prepped_data.Rdata")

