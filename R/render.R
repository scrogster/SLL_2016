library(rmarkdown)

args = commandArgs(trailingOnly=TRUE)

#desired format for ouput paper
out_format=args[2]

if(out_format=="word_document"){render("Delma.Rmd", "word_document")}
if(out_format=="pdf_document") {render("Delma.Rmd", "pdf_document")}

