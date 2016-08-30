
all: prepped_data.Rdata \
     prepped_data_plusGIS.Rdata \
     formatted_for_JAGS.Rdata \
     fitted_model.Rdata \
     figs

figs: Figures/detection_plot.pdf \
      Figures/initial_occ_params.pdf \
      Figures/extinction_params.pdf \
      Figures/colonisation_params.pdf

#extracting the survey data from the spreadsheets
prepped_data.Rdata:  R/clean_excel.R
	Rscript $^

#formatting GIS and other site related variables, and making spatial data sets
#will need code here to compile the fire histories...
prepped_data_plusGIS.Rdata:   R/get_spatial_points.R  prepped_data.Rdata
	Rscript $^ 

#format the data for JAGS and tidy up workspace
formatted_for_JAGS.Rdata: R/format_data.R prepped_data_plusGIS.Rdata
	Rscript $^

#fit a dynamic occupancy model to the data
fitted_model.Rdata: R/fit_occ_model.R formatted_for_JAGS.Rdata R/prototype_occmod.txt
	Rscript $^

###################################################
#pattern rule to make pdf figure from Rscript
###################################################
Figures/%.pdf: R/%.R fitted_model.Rdata
	Rscript $^

