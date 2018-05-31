

########### LOADING LIBRARIES ######################################################################################################################

library(shiny)
library(DT)
library(shinythemes)
library(dplyr)
library(stringr)
library(data.table)
library(digest)
require(rCGH)
require(cytoScanLinux)
require(affxparser)
library(R.utils)
require(synapseClient)
library(shiny.semantic)
library(rhandsontable)
library(shinyjs)
library(htmlwidgets)

#source("https://bioconductor.org/biocLite.R")
#biocLite("affxparser")


########### END OF LIBRARIES #######################################################################################################################
####################################################################################################################################################





fluidPage(theme = shinytheme("flatly"),
          
          titlePanel(div(img(src = "UNICANCER.png", height = 200, width = 200 ), 
                         p(style = "text-align:center;font-size:50px;color:#19334d;",
                           "SuGR"))),
          
          uiOutput("ui")
          
) #end of fluidPage