
DATAXL = 'DataFromGarry/SLL monitoring database formatted 13May13.xls'

#all: file.md file.pdf file.docx
 
#file.md: file.Rmd
#	Rscript -e "library(knitr); knit('file.Rmd')"
 
#file.pdf: file.md
#	pandoc -H format.sty -V fontsize=12pt file.md -o file.pdf
 
#file.docx: file.md
#	pandoc -s -S --reference-docx=format.docx file.md -o file.docx

#extracting the survey data from the spreadsheets
prepped_data.Rdata:  R/clean_excel.R $DATAXL 
	Rscript $^  $@

#formatting GIS and other site related variables, and making spatial data sets
#will need code here to compile the fire histories...
gis_prep.Rdata:   R/extract_gis.R  prepped_data.Rdata
	Rscript $^ $@

#format the data for JAGS and tidy up workspace
formatted_for_JAGS.Rdata: R/format_data.R gis_prep.Rdata
	Rscript $^ $@

#fit model with JAGS
fitted_models.Rdata: R/fit_model.R formatted_for_JAGS.Rdata
	Rscript $^ $@

#make figures from raw data and inferences. One R script for each figure.
Figures/%.pdf: R/%.R
    cd $(<D);Rscript $(<F)

