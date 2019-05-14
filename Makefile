
all: prepped_data.Rdata \
     prepped_data_plusGIS.Rdata \
     formatted_for_JAGS.Rdata \
     fitted_model.Rdata \
     figs 
     #Delma.docx \
     #Delma.pdf  \
     #SiteShapefile/tilesites.shp \
     #ppcheck 

figs: Figures/detection_plot.pdf Figures/detection_plot.png \
      Figures/site_occ_plot.pdf Figures/site_occ_plot.png \
      Figures/parameter_plot.pdf Figures/parameter_plot.png \
      Figures/response_plot.pdf Figures/response_plot.png \
      Traceplots/initocc_trace.pdf \
      Figures/Site-map.pdf Figures/Site-map.png 
      
ppcheck: PP_check.Rdata Figures/PP_check_plot.pdf Figures/PP_check_plot.png

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

#do another run for the posterior predictive checks	
PP_check.Rdata: R/run_PPcheck.R formatted_for_JAGS.Rdata R/dynoccmod.txt
	Rscript $^
	
Figures/PP_check_plot.pdf: R/plot_PPcheck.R PP_check.Rdata
	Rscript $^

#make a shapefile of the sites and site covariates used in the model for mapping purposes
SiteShapefile/tilesites.shp:   R/MakeSiteShapefile.R  fitted_model.Rdata
	Rscript $^

###################################################
#pattern rule to make pdf figure from Rscript
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
	
##################################################
#Rules to render the paper as pdf and docx if either markdown or bibtex files change
##################################################

Delma.docx:  R/render.R Delma.Rmd figs
	Rscript R/render.R Delma.Rmd word_document
Delma.pdf:  R/render.R Delma.Rmd figs
	Rscript R/render.R Delma.Rmd pdf_document
	

