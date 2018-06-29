

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


########### END OF LIBRARIES #######################################################################################################################
####################################################################################################################################################





fluidPage(theme = shinytheme("flatly"),
          
          titlePanel(div(img(src = "UNICANCER.png", height = 150, width = 350 ), 
                         p(style = "text-align:center;font-size:80px;font-weight:bold;color:#19334d;",
                           "SuGR"))),
          
          uiOutput("ui")
          
) #end of fluidPage