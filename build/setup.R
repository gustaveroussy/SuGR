source("https://bioconductor.org/biocLite.R")

if(!require(shiny))  
  install.packages("shiny",repos='https://cloud.r-project.org/')

if(!require(DT))  
  install.packages("DT",repos='https://cloud.r-project.org/')

if(!require(shinythemes))  
  install.packages("shinythemes",repos='https://cloud.r-project.org/')

if(!require(dplyr))  
  install.packages("dplyr",repos='https://cloud.r-project.org/')

if(!require(stringr))  
  install.packages("stringr",repos='https://cloud.r-project.org/')

if(!require(data.table))  
  install.packages("data.table",repos='https://cloud.r-project.org/')

if(!require(affxparser))
  BiocInstaller::biocLite("affxparser")

if(!require(R.utils))  
  install.packages("R.utils",repos='https://cloud.r-project.org/')

source("http://depot.sagebase.org/CRAN.R")
pkgInstall(c("synapseClient"))

if(!require(shiny.semantic))  
  install.packages("shiny.semantic",repos='https://cloud.r-project.org/')

if(!require(rhandsontable))  
  install.packages("rhandsontable",repos='https://cloud.r-project.org/')

if(!require(shinyjs))  
  install.packages("shinyjs",repos='https://cloud.r-project.org/')

if(!require(htmlwidgets))  
  install.packages("htmlwidgets",repos='https://cloud.r-project.org/')

