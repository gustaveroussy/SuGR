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

if(!require(digest))  
  install.packages("digest",repos='https://cloud.r-project.org/')

if(!require(pathview))
  BiocInstaller::biocLite("rCGH")

if(!require(pathview))
  BiocInstaller::biocLite("cytoScanLinux")

if(!require(pathview))
  BiocInstaller::biocLite("affxparser")

if(!require(R.utils))  
  install.packages("R.utils",repos='https://cloud.r-project.org/')

if(!require(synapseClient))  
  install.packages("synapseClient",repos='https://cloud.r-project.org/')

if(!require(shiny.semantic))  
  install.packages("shiny.semantic",repos='https://cloud.r-project.org/')

if(!require(rhandsontable))  
  install.packages("rhandsontable",repos='https://cloud.r-project.org/')

if(!require(shinyjs))  
  install.packages("shinyjs",repos='https://cloud.r-project.org/')

if(!require(htmlwidgets))  
  install.packages("htmlwidgets",repos='https://cloud.r-project.org/')

