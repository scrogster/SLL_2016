
all: prepped_data.Rdata \
     prepped_data_plusGIS.Rdata \
     formatted_for_JAGS.Rdata \
     fitted_model.Rdata \
     figs \
     Delma.docx \
     Delma.pdf

figs: Figures/detection_plot.pdf Figures/detection_plot.png \
      Figures/initial_occ_params.pdf Figures/initial_occ_params.png \
      Figures/extinction_params.pdf Figures/extinction_params.png \
      Figures/colonisation_params.pdf Figures/colonisation_params.png \
      Figures/num_occ_plot.pdf Figures/num_occ_plot.png \
      Figures/num_ext_plot.pdf Figures/num_ext_plot.png \
      Figures/flux_plot.pdf Figures/flux_plot.png \
      Figures/Site-map.pdf Figures/Site-map.png \
      Traceplots/initocc_trace.pdf

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
	

