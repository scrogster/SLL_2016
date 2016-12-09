#Idea here is to filter ABC reports to identify sites that report some mention of burning.
#we can then look in to what happened at these sites in a bit more detail

library(dplyr)
library(readr)

abc<-read_csv("ABCreporting/delma_abc_report_working.csv")
names(abc)<-gsub("\n","_", names(abc)) #strip the carriage returns out of the column names.
names(abc)<-gsub("\r","", names(abc)) 
length(unique(abc$LOCATION_NAME))


#filter by whether there is mention of burn or fire in action details, standard action or results details
burnsites<-abc %>%
	filter(grepl('Burn|burn|Fire|fire', ACTION_DETAILS) | 
				 	grepl('Burn|burn|Fire|fire', STANDARD_ACTION) |
				 	grepl('Burn|burn|Fire|fire', RESULT_DETAILS) ) 

#%>%
#select(LOCTN_TYPE, LOCATION_NAME, LOCATION_MONITOR) 

write_excel_csv(burnsites, "ABCreporting/ABC_burn_actions.csv")
