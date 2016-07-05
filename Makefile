
DATAXL = 'DataFromGarry/SLL monitoring database formatted 13May13.xls'

#all: file.md file.pdf file.docx
 
#file.md: file.Rmd
#	Rscript -e "library(knitr); knit('file.Rmd')"
 
#file.pdf: file.md
#	pandoc -H format.sty -V fontsize=12pt file.md -o file.pdf
 
#file.docx: file.md
#	pandoc -s -S --reference-docx=format.docx file.md -o file.docx

prepped_data.Rdata:  R/clean_excel.R $DATAXL 
	Rscript $^  $@
	
gis_prep.Rdata:   R/extract_gis.R  prepped_data.Rdata
	Rscript $^ $@
	
fitted_model.Rdata: R/fit_model.R gis_prep.Rdata
	Rscript $^ $@

Figures/%.pdf: R/%.R
    cd $(<D);Rscript $(<F)

