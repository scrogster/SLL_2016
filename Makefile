
all: prepped_data.Rdata \
     prepped_data_plusGIS.Rdata \
     formatted_for_JAGS.Rdata \
     fitted_model.Rdata \
     figs \
     Tables.docx

figs: Figures/detection_plot.pdf Figures/detection_plot.png \
      Figures/site_occ_plot.pdf Figures/site_occ_plot.png \
      Figures/parameter_plot.pdf Figures/parameter_plot.png \
      Figures/response_plot.pdf Figures/response_plot.png \
      Traceplots/initocc_trace.pdf 

#extracting the survey data from the spreadsheets
prepped_data.Rdata:  R/clean_excel.R DataFromGarry/SLL\ monitoring\ database\ formatted\ 13May13.xls
	Rscript $^

#formatting GIS and other site related variables, and making spatial data sets
#will need code here to compile the fire histories...
prepped_data_plusGIS.Rdata:   R/get_spatial_points.R  prepped_data.Rdata SpatialData/DelmaGridVariables_15Feb2017.xlsx
	Rscript $^ 

#format the data for JAGS and tidy up workspace
formatted_for_JAGS.Rdata: R/format_data.R prepped_data_plusGIS.Rdata DataFromGarry/Season_Burn_Summary.csv
	Rscript $^

#fit a dynamic occupancy model to the data
fitted_model.Rdata: R/fit_occ_model.R formatted_for_JAGS.Rdata R/dynoccmod.txt
	Rscript $^

#render a docx file with the tables ready to insert in the manuscript.
Tables.docx: Tables.Rmd fitted_model.Rdata
	Rscript -e 'rmarkdown::render("Tables.Rmd")'
###################################################
#pattern rule to make pdf figures from Rscript
###################################################
Figures/%.pdf: R/%.R fitted_model.Rdata
	Rscript $^
	
Figures/%.png : Figures/%.pdf
	convert -density 300 $< $@
	
###################################################
#Rule to make traceplots of model parameters (only made in pdf for diagnostic purposes)
###################################################
Traceplots/initocc_trace.pdf: R/traceplots.R fitted_model.Rdata
	Rscript $^


	

