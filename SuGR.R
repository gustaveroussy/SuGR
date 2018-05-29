#Input Request size
options(shiny.maxRequestSize=200*1024^2)

#number of fails for the password
num_fails_to_lockout <- 5


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







### Reactive Values ################################################################################################################################

files.list_ngs = reactiveValues(valeur=NULL)
files.list_cel = reactiveValues(valeur=NULL)
in.file_ngs = reactiveValues(valeur=NULL)
innew.file_ngs = reactiveValues(valeur=NULL)
in.file_cel = reactiveValues(valeur=NULL)
innew.file_cel = reactiveValues(valeur=NULL)
reading.cel = reactiveValues(valeur=NULL)
final.table = reactiveValues(valeur=NULL)
row.number = reactiveValues(valeur=NULL)
syn.ID = reactiveValues(valeur=NULL)
syn.IDCEL = reactiveValues(valeur=NULL)
table.synIDNGS = reactiveValues(valeur=NULL)
table.synIDCEL  = reactiveValues(valeur=NULL)
cel.ID = reactiveValues(valeur=NULL)
dezip.cel = reactiveValues(valeur=NULL)

########### END OF LIBRARIES #######################################################################################################################
####################################################################################################################################################








# create a tmp file in user computer ###############################################################################################################


dir_file = NULL
if (is.null(dir_file)){
  dir_file <- file.path(tempdir(),"SAFI02upload/")
}

if (dir.exists(dir_file)){
  print("Romoving existing files in")
  print(dir_file)
  file.remove(list.files(dir_file,full.names=T))
  
} else{
  dir.create(dir_file)
}

########## create a tmp file in user computer ######################################################################################################
####################################################################################################################################################










# Login page #######################################################################################################################################

ui <- fluidPage(theme = shinytheme("flatly"),
                
                titlePanel(div(img(src = "UNICANCER.png", height = 200, width = 200 ), 
                               p(style = "text-align:center;font-size:50px;color:#19334d;",
                                  "SuGR"))),
                
                uiOutput("ui")
                
) #end of fluidPage


########### END OF Login Page ######################################################################################################################
####################################################################################################################################################












# Server Code ######################################################################################################################################


server <- shinyServer(function(input, output, session) {
  
  #### UI code --------------------------------------------------------------
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
          
        ),
        hr(),
        div(style='color:grey', p(style = "text-align:center", 
                                  "Please contact Alicia TRAN DIEN (alicia.tran-dien@gustaveroussy.fr) from", 
                                  img(src = "logo.jpg", height = 50, width = 80),
                                  "Bioinformatics plateform for any requests.")),
        div (style='color:grey', p(style = "text-align:center","Or download the help file for more information.")),
        br(),
        div(style="text-align: center;",
            actionButton("helper", "Help document", onclick = "window.open('SuGR.doc')"))
        
      )
    } else {
      
      # Define UI for data upload app -------------------------------------------------------
      withProgress(expr = {synapseLogin(input$user_name, input$password)}, message = "Connecting to Synapse... Please wait") 
      ui <- fluidPage(
        useShinyjs(),
        #navigation bar
        
        navbarPage(
          textOutput(""),
          
          
          
          
          
          #Input for CEL ############################################################################################################################
          tabPanel("CEL",
                   sidebarLayout(
                     
                     # Sidebar panel for inputs --------------------------------------------------------
                     
                     sidebarPanel(
                       
                       fileInput("file2", "Select CEL file",
                                 multiple = TRUE,
                                 accept = c(".CEL",
                                            ".CEL.bz2"), 
                                 placeholder = "Select your CEL file"
                                 
                       ) # end of filInput
                       
                       
                     ), # end of sidebarPanel
                     
                     
                     # Main panel for displaying outputs -------------------------------------------------
                     
                     mainPanel(
                       uiOutput("select_input_cel"),
                       # Output: Data file ---------------------------------------------------------------
                       htmlOutput("CELcheckTOR"), # check if TOR
                       br(),
                       htmlOutput("CELcheck1"), # check input name
                       br(),
                       htmlOutput("CELcheck3"), # check if Oncoscan
                       br(),
                       htmlOutput("CELcheck4"), # check if Cytoscan
                       br(),
                       uiOutput("UploadCEL"), # upload on synapse
                       br(),
                       uiOutput("UploadButtonALLCEL"), # upload file on Synapse
                       br(),
                       uiOutput("UploadALLCEL"),
                       br(),
                       htmlOutput("UploadTextCEL"), # upload on Synapse confirmation
                       br(),
                       uiOutput("UploadButtonCEL") # button to upload CEL on Synapse
                       
                       
                     ) # end of main panel
                     
                     
                   ) # end of sidebarLayout
                   
          ), #end of tabCEL #########################################################################################################################
          ###########################################################################################################################################
          
          
          
          #INPUT FOR NGS ############################################################################################################################
          
          tabPanel("NGS",
                   
                   # Sidebar layout with input and output definitions ----------------------------------
                   
                   sidebarLayout(
                     
                     # Sidebar panel for inputs --------------------------------------------------------
                     
                     sidebarPanel(
                       
                       # Input: Select a file ----------------------------------------------------------
                       
                       fileInput("file1", "Choose NGS File",
                                 multiple = TRUE,
                                 accept = c("text/tsv",
                                            ".tsv"),
                                 placeholder = "Select your NGS file"
                                 
                       ), #end of file input
                       
                       
                       hr(style = 'border-color:darkgrey'),
                       div (style='color:grey',strong ("You can download a SampleSheet Example if you want")),
                       br(),
                       actionButton("tsv", "SampleSheetExample", onclick = "window.open('SampleSheetExample.tsv')"),
                       hr(style = 'border-color:darkgrey'),
                       div (style='color:grey',strong ("You can download the list of genes name below")),
                       br(),
                       actionButton("gename", "GeneName", onclick = "window.open('list_genes.txt')")
                       
                     ), #end of sidebarPanel
                     
                     
                     # Main panel for displaying outputs -------------------------------------------------
                     
                     mainPanel(
                       
                       uiOutput("select_input_ngs"),
                       # Output: Data file ---------------------------------------------------------------
                       htmlOutput("NGScheckName"), # check input name
                       br(),
                       htmlOutput("NGScheckTOR"), # check if TOR
                       br(),
                       htmlOutput("NGScheckColumn"), # check columns
                       br(),
                       htmlOutput("NGScheckGene"), # check gene name
                       br(),
                       uiOutput("UploadButtonNGS"), # upload file on Synapse
                       br(),
                       br(),
                       uiOutput("UploadButtonALLNGS"), # upload file on Synapse
                       br(),
                       uiOutput("UploadNGS"),
                       br(),
                       uiOutput("UploadALLNGS"),
                       br(),
                       DT::DTOutput("table"), # main table
                       br(),
                       uiOutput("SaveChanges"),
                       br(),br(),br(),
                       rHandsontableOutput("hot")
                       
                     ) #end of main panel
                     
                     
                   ) #end of sidebarLayout
                   
          ) #end of tabNGS #########################################################################################################################
          ###########################################################################################################################################
          
          
          
        ), #end of navbarpage
        hr(),
        div(style='color:grey', p(style = "text-align:center", 
                                  "Please contact Alicia TRAN DIEN (alicia.tran-dien@gustaveroussy.fr) from", 
                                  img(src = "logo.jpg", height = 50, width = 80),
                                  "Bioinformatics plateform for any requests.")),
        div (style='color:grey', p(style = "text-align:center","Or download the help document for further information.")),
        br(),
        div(style="text-align: center;",
            actionButton("helper", "Help document", onclick = "window.open('SuGR.doc')"))
        
        
      ) #end of fluidPage
      
    } # end of else if password good
    
  }) # end of rendUI
  
  #####################################################################################################################################################
  #####################################################################################################################################################
  #####################################################################################################################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################################################################################################################################################
  
  # CEL INPUT #
  
  ##################################################################################################################################################
  
  
  ####################
  # Check file name  #
  ####################
  
  
  # create folder tmp and create dropdown menu with all the inputs selected ------------------------------------------------------------------------
  observeEvent(input$file2, {
    
    
    
    file.remove(list.files(dir_file,full.names = T))
    file.copy(from = input$file2$datapath,to = paste0(dir_file,input$file2$name))
    
    
    files=list.files(dir_file,full.names =T) 
    file_list=list()
    for(i in files){
      
      file_list[[basename(i)]]=
        
        if (length(grep (pattern = ".CEL|.CEL.bz2", i)) > 0 ) {}
    }
    files.list_cel$valeur=file_list
    
    
    output$select_input_cel=renderUI(
      selectInput(inputId = "uploaded_files_cel",label = "Uploaded files:",choices = basename(files)))
    
    
    
  }) ########################################################################################################################### end of dropdown menu
  
  
  
  
  
  # check if TOR file ------------------------------------------------------------------------------------------------------------------------------    
  observeEvent(input$uploaded_files_cel,{
    
    tmp_fileCEL <- paste0(dir_file,input$uploaded_files_cel)
    in.file_cel$valeur = tmp_fileCEL
    
    new_names = gsub(" |-_","_",tmp_fileCEL)
    innew.file_cel$valeur = new_names
    
    torgrep <- grep(pattern = "^T[0-9]", input$uploaded_files_cel)
    
    if (length(torgrep) > 0){
      output$CELcheckTOR <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:red',
                                            p("!!! WARNING !!! The file uploaded", input$file2, "is a SAFIR02-Tor File")))))
      
      
    } else {
      
      
      output$CELcheckTOR <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:green',
                                            p("The file uploaded", input$file2, "is a SAFIR02 File")))))
      
    }
    
    
    
  }) ##################################################################################################################### end of check if TOR file
  
  
  
  # check if wrong typo in file name -----------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_cel,{
    spacegrep <- grep(pattern = " |-_", in.file_cel$valeur)
    if (length(spacegrep) > 0 ) {
      file.rename(in.file_cel$valeur,innew.file_cel$valeur)
      output$CELcheck1 <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:orange',
                                            p("!! WARNING !! A special character has been found in file:", 
                                              in.file_cel$valeur, "It has now been renamed:", innew.file_cel$valeur)))))
      
    } else{
      
      output$CELcheck1 <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:green',
                                            p(in.file_cel$valeur,"name is good")))))
      in.file_cel$valeur <- innew.file_cel$valeur
      
    }
    
  })  ################################################################################################################"# end of check file name   
  
  
  
  
  # check if Onco or Cyto scan ---------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_cel,{
    platform <- readRDS("credentials/plateform.rds")
    row_username <- which(platform$user == input$user_name)
    
    if (length(grep (pattern = ".bz2",  innew.file_cel$valeur)) > 0 ) {
      celtodezip <- gsub (pattern = "\\.bz2$", "",  innew.file_cel$valeur)
      dezipcel <- bunzip2( innew.file_cel$valeur, celtodezip , remove = FALSE, skip = TRUE)
    } else {
      dezipcel <-  innew.file_cel$valeur
    }
    
    dezip.cel$valeur = dezipcel
    
    withProgress(expr = {read <- readCelHeader(dezip.cel$valeur)}, message = "Reading file... Please wait") 
    reading.cel$valeur = read
    
    
    if (reading.cel$valeur$chiptype == "OncoScan_CNV") {
      output$CELcheck3 <- renderText(
        paste0(div(style='color:grey', p("The file uploaded",innew.file_cel$valeur,"is an OncoScan",sep = ""))))
      
      celId <-  as.character(platform[row_username, "synonco"])
      syn.IDCEL$valeur = celId
      
    } else if (reading.cel$valeur$chiptype == "CytoScanHD_Array") {
      
      output$CELcheck3 <- renderText(
        paste0(div(style='color:grey', p("The file uploaded",innew.file_cel$valeur,"is a CytoScan", sep = ""))))
      
      celId <-  as.character(platform[row_username, "syncyto"])
      syn.IDCEL$valeur = celId
      
    }
    
  }) #################################################################################################################### end of onco or cytoscan
  
  
  
  # upload to synapse ---------------------------------------------------------------------------------------------------------------------------
  
  
  table_synIDCEL = read.table("credentials/table_syn2.tsv", header = TRUE , sep = "\t")
  table.synIDCEL$valeur = table_synIDCEL
  
  
  observeEvent(input$uploaded_files_cel,{
    
    table.synIDCEL$valeur <- rbind (table.synIDCEL$valeur, data.frame(inputfile = innew.file_cel$valeur,
                                                                      synID = syn.IDCEL$valeur))
    
    table.synIDCEL$valeur <- table.synIDCEL$valeur[!duplicated(table.synIDCEL$valeur[,c('inputfile')]),]
    table.synIDCEL$valeur <- table.synIDCEL$valeur[grep("T[0-9]", table.synIDCEL$valeur$inputfile, invert = TRUE),]
    print(table.synIDCEL$valeur)
    
    
    output$UploadButtonALLCEL <- renderUI({
      mainPanel(
        actionButton(inputId = "uploadALLCEL", label = "Upload ALL CEL On Synapse")) 
      
    })
    
    output$UploadALLCEL <- eventReactive(input$uploadALLCEL, ignoreInit = TRUE, {
      
      
      for (row in 1:nrow(table.synIDCEL$valeur)) {
        infile <- as.character(table.synIDCEL$valeur[row, "inputfile"])
        synID  <- as.character(table.synIDCEL$valeur[row, "synID"])
        file <- File(infile, parentId=synID)
        withProgress(expr = {file <- synStore(file)}, message = "Uploading to Synapse... Please wait")
      }
    })
    
     
  }) ############################################################################################################################## end of uploadCEL
  
  
  ##################################################################################################################################################
  
  # END OF CEL INPUT ###############################################################################################################################
  
  ##################################################################################################################################################
  
  
  
  
  
  
  
  
  
  
  ##################################################################################################################################################
  
  # NGS INPUT #
  
  ##################################################################################################################################################
  
  
  ####################
  # Check file name  #
  ####################
  
  
  # create folder tmp and create dropdown menu with all the inputs selected ------------------------------------------------------------------------
  
  observeEvent(input$file1,{
    
    
    file.remove(list.files(dir_file,full.names = T))
    file.copy(from = input$file1$datapath,to = paste0(dir_file,input$file1$name))
    files=list.files(dir_file,full.names =T) 
    file_list=list()
    for(i in files){
      
      if (length(grep(".tsv", i) >0)){
        file_list[[basename(i)]]=read.csv(i, header = TRUE, sep = "\t", fill = TRUE)
        } else {
        }
    }
    
    files.list_ngs$valeur=file_list
    
    
    output$select_input_ngs=renderUI(
      selectInput(inputId = "uploaded_files_ngs",label = "Uploaded files:",choices = basename(files))
      
    )
    
    
    
  }) ######################################################################################################## end of observe event dropdown menu
  
  
  
  
  # check if it s a TOR file --------------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{
    tmp_fileNGS=paste0(dir_file,input$uploaded_files_ngs)
    in.file_ngs$valeur = tmp_fileNGS
    
    new_namesNGS = gsub(" |-_","_",tmp_fileNGS)
    innew.file_ngs$valeur = new_namesNGS
    
    
    torgrep <- grep(pattern = "T[0-9]", tmp_fileNGS)
    if (length(torgrep) > 0){
      output$NGScheckTOR <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:red',
                                            p("!!! WARNING !!! The file uploaded", input$file1, "is a SAFIR02-Tor File")))))
      
    } else {
      
      output$NGScheckTOR <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:green',
                                            p("The file uploaded", input$file1, "is a SAFIR02 File")))))
    } 
    
  }) ############################################################################################################ end of else if not TOR its safir 
  
  
  
  
  
  
  # check file name if so and rename it if needed --------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{
    
    
    spacegrepNGS <- grep(pattern = " |-_", in.file_ngs$valeur)
    if (length(spacegrepNGS) > 0 ) {
      
      
      file.rename(in.file_ngs$valeur,innew.file_ngs$valeur )
      
      output$NGScheckName <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:orange',
                                            p("!! WARNING !! A special character has been found in file:", 
                                              in.file_ngs$valeur,"It has now been renamed",innew.file_ngs$valeur )))))
      
      
    } else{
      
      output$NGScheckName <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:green',
                                            p(in.file_ngs$valeur,"name is good")))))
      
      innew.file_ngs$valeur  <- in.file_ngs$valeur
      
    } 
    
    
  }) #################################################################################################################### end of else if good name
  
  
  
  
  
  #############################################
  #   Now that file name is ok process file   #
  #############################################
  
  
  
  
  # check columns ---------------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{
    
    ACfiles <- grep(pattern = "_AC", in.file_ngs$valeur)
    if (length(ACfiles) > 0 ) {
      df <- files.list_ngs$valeur[[basename(in.file_ngs$valeur)]]
      final.table$valeur = df
      output$NGScheckColumn <- renderText(
        paste0(p("")))
      
      
      
    } else {
      
      df <- files.list_ngs$valeur[[basename(in.file_ngs$valeur)]]
      final <- subset(df, select=c(PatId,Global_Conclusion,Manual_Var_Comment,Manual_Var_Classif,Gene_Symbol,Protein_Change,Exon,
                                   Variant_Freq,Position_Cov,RefSeq_Id,cDNA_Change,Codon,Chr,Start_Position,End_Position,
                                   Strand,Type,Reference_Seq,Variant_Seq,Variant_Cov,MAF_classification,ESP_Freq,by1000G_Freq,
                                   DbSNP_Id,COSMIC_Id,Quality,Strand_Bias,Amplicon_Ref))
      final.table$valeur = final
      
      
      pattern = c("Polyphen2_Prediction" , "SIFT_Prediction" ,"POLYPHEN2_HDIV_SCORE" , 
                  "POLYPHEN2_HDIV_INTERPRETATION" , "POLYPHEN2_HVAR_SCORE" , "POLYPHEN2_HVAR_INTERPRETATION")
      columns <- colnames(df)
      
      
      if (length(grep(paste(pattern, collapse = "|"), columns) > 0)){
        output$NGScheckColumn <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:red', p("!!!! WARNING !!!!",innew.file_ngs$valeur,"Columns have been changed.", 
                                                                                strong("Please download the new table.")))))) 
        
        
        
      }  else {
        
        output$NGScheckColumn <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:green', p("Columns are ok.")))))
        
      } 
    }
    
  }) ################################################################################################################################# end of else 
  
  
  
  
  # check gene typo --------------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{
    ACfiles <- grep(pattern = "_AC", in.file_ngs$valeur)
    if (length(ACfiles) > 0 ) {
      output$NGScheckGene <- renderText(
        paste0(p("")))
      
      
    } else {
      gene <- read.delim("www/list_genes.txt")
      gene_symbol <- select(final.table$valeur,Gene_Symbol)
      
      
      difference <- data.frame(lapply(1:ncol(gene_symbol),function(i)setdiff(gene_symbol[,i],gene[,i])))
      colnames(difference) <- colnames(gene_symbol)
      row.number$valeur = as.character(rownames(difference))
      TEST = (as.character(difference))
      lenrow <- nrow(difference)
      
      if (lenrow > 0) {
        output$NGScheckGene <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:red',
                                              p("!!! WARNING !!! Gene typo wrong (red cell(s)),
                                                please check gene name nomenclature and edit the table")))))
        
        
        
      } else {
        output$NGScheckGene <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:green', p("Gene name ok")))))
        
        
      }
    }
    
  }) ######################################################################################################################## end of checking gene
  
  
  # display editable table -----------------------------------------------------------------------------------------------------------------------
  
  observeEvent(input$uploaded_files_ngs,{
    ACfiles <- grep(pattern = "_AC", in.file_ngs$valeur)
    if (length(ACfiles) > 0 ) {
      
      DF = final.table$valeur
      rhandsontable(DF) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE, allowRowEdit=TRUE)
      
      
    } else {
      
      
      
      output$SaveChanges <- renderUI({
        mainPanel(
          actionButton("saveBtn", "Save changes"))
        
      }) # end of renderUI uploadbuttonNGS    
      
      # remove button and isolate to update file automatically
      # after each table change
      observe({input$saveBtn
      hot = isolate(input$hot)
      
      if (!is.null(hot)) {
        write.table(hot_to_r(input$hot), innew.file_ngs$valeur , quote = FALSE, sep = "\t",row.names = FALSE)
      }
      })
      
      
      output$hot <- renderRHandsontable({
        
        DF = final.table$valeur
        gene <- read.delim("www/list_genes.txt")
        gene_symbol <- select(final.table$valeur,Gene_Symbol)
        difference <- lapply(1:ncol(gene_symbol),function(i)setdiff(gene_symbol[,i],gene[,i]))
        list_gene <- unlist(difference)
        
        rhandsontable(DF, list_gene = list_gene) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, allowRowEdit=TRUE) %>%
          hot_col(col = c("Manual_Var_Comment","Manual_Var_Classif","Gene_Symbol","Protein_Change","Exon","Variant_Freq",
                          "Position_Cov","RefSeq_Id","cDNA_Change","Codon","Chr","Start_Position","End_Position","Strand","Type","Reference_Seq","Variant_Seq",
                          "Variant_Cov","MAF_classification","ESP_Freq","by1000G_Freq","DbSNP_Id","COSMIC_Id","Quality","Strand_Bias","Amplicon_Ref"), 
                  type = "autocomplete") %>%        
          hot_cols(renderer = "function(instance, td, row, col, prop, value, cellProperties) {
                   Handsontable.renderers.TextRenderer.apply(this, arguments);
                   if (instance.params) {
                   mhrows = instance.params.list_gene;
                   mhrows = mhrows instanceof Array ? mhrows : [mhrows];
                   }
                   if (instance.params && mhrows.includes(value)) td.style.background = 'red';
      }"
        )
        
    })
      
}
    
    
}) ############################################################################################################## end of display editable table
  
  
  
  
  # Check if AC or VC ----------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{
    
    # connect to synapse
    
    
    platform <- readRDS("credentials/plateform.rds")
    
    row_username <- which(platform$user == input$user_name)
    
    ACfiles <- grep(pattern = "_AC", in.file_ngs$valeur)
    VCfiles <- grep(pattern = "_VC", in.file_ngs$valeur)
    if (length(ACfiles) > 0 ) {
      NGSId <-  as.character(platform[row_username, "synac"])
      syn.ID$valeur = NGSId
      
      
    } else if (length(VCfiles) > 0 ) {
      NGSId <-  as.character(platform[row_username, "synvc"])
      syn.ID$valeur = NGSId
      
      
    }
    
   
    
  }) ############################################################################################################################ end of AC or VC
  
  
  
  
  # sign in and upload to synapse -----------------------------------------------------------------------------------------------------------------
  table_synIDNGS = read.table("credentials/table_syn.tsv", header = TRUE , sep = "\t")
  table.synIDNGS$valeur = table_synIDNGS
  
  
  observeEvent(input$uploaded_files_ngs,{
    table.synIDNGS$valeur <- rbind ( table.synIDNGS$valeur, data.frame(inputfile = innew.file_ngs$valeur,
                                                                       synID = syn.ID$valeur))
    table.synIDNGS$valeur <- table.synIDNGS$valeur[!duplicated(table.synIDNGS$valeur[,c('inputfile')]),]
    table.synIDNGS$valeur <- table.synIDNGS$valeur[grep("T[0-9]", table.synIDNGS$valeur$inputfile, invert = TRUE),]
    print(table.synIDNGS$valeur)
    
    
    output$UploadButtonALLNGS <- renderUI({
      mainPanel(
        actionButton(inputId = "uploadALLNGS", label = "Upload ALL NGS On Synapse")) 
      
    })
    
    output$UploadALLNGS <- eventReactive(input$uploadALLNGS, ignoreInit = TRUE, {
      
      
      for (row in 1:nrow(table.synIDNGS$valeur)) {
        infile <- as.character(table.synIDNGS$valeur[row, "inputfile"])
        synID  <- as.character(table.synIDNGS$valeur[row, "synID"])
        
        file <- File(infile, parentId=synID)
        withProgress(expr = {file <- synStore(file)}, message = "Uploading to Synapse... Please wait")
      }
    })
    
    
    
  })
  
  
  #################################################################################################################### sign in and upload to synapse 
  
  
  
  ##################################################################################################################################################
  
  # END OF NGS INPUT ###############################################################################################################################
  
  ##################################################################################################################################################
  

  
  
  
  
  ##################################################################################################################################################
  
  # PASSWORD SETTING #
  
  ##################################################################################################################################################
  
  
  #### PASSWORD server code ---------------------------------------------------- 
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "")
  
  # authenticate user by:
  #   1. checking whether their user name and password are in the credentials 
  #       data frame and on the same row (credentials are valid)
  #   2. if credentials are valid, retrieve their lockout status from the data frame
  #   3. if user has failed login too many times and is not currently locked out, 
  #       change locked out status to TRUE in credentials DF and save DF to file
  #   4. if user is not authenticated, determine whether the user name or the password 
  #       is bad (username precedent over pw) or he is locked out. set status value for
  #       error message code below
  observeEvent(input$login_button, {
    credentials <- readRDS("credentials/credentials.rds")
    
    row_username <- which(credentials$user == input$user_name)
    row_password <- which(credentials$pw == input$password)
    #row_password <- which(credentials$pw == digest(input$password)) # digest() makes md5 hash of password
    
    # if user name row and password name row are same, credentials are valid
    #   and retrieve locked out status
    if (length(row_username) == 1 && 
        length(row_password) >= 1 &&  # more than one user may have same pw
        (row_username %in% row_password)) {
      user_input$valid_credentials <- TRUE
      user_input$user_locked_out <- credentials$locked_out[row_username]
    }
    
    # if user is not currently locked out but has now failed login too many times:
    #   1. set current lockout status to TRUE
    #   2. if username is present in credentials DF, set locked out status in 
    #     credentials DF to TRUE and save DF
    if (input$login_button == num_fails_to_lockout & 
        user_input$user_locked_out == FALSE) {
      
      user_input$user_locked_out <- TRUE
      
      if (length(row_username) == 1) {
        credentials$locked_out[row_username] <- TRUE
        
        saveRDS(credentials, "credentials/credentials.rds")
      }
    }
    
    # if a user has valid credentials and is not locked out, he is authenticated      
    if (user_input$valid_credentials == TRUE & user_input$user_locked_out == FALSE) {
      user_input$authenticated <- TRUE
    } else {
      user_input$authenticated <- FALSE
    }
    
    # if user is not authenticated, set login status variable for error messages below
    if (user_input$authenticated == FALSE) {
      if (user_input$user_locked_out == TRUE) {
        user_input$status <- "locked_out"  
      } else if (length(row_username) > 1) {
        user_input$status <- "credentials_data_error"  
      } else if (input$user_name == "" || length(row_username) == 0) {
        user_input$status <- "bad_user"
      } else if (input$password == "" || length(row_password) == 0) {
        user_input$status <- "bad_password"
      }
    }
  })   
  
  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    wellPanel(
      
      # select a platform
      semanticPage(
        suppressDependencies("bootstrap")
      ),
      
      textInput("user_name", "User Name:"),
      
      passwordInput("password", "Password:"),
      
      actionButton("login_button", "Log in")
    )
  })
  
  # red error message if bad credentials
  output$pass <- renderUI({
    if (user_input$status == "locked_out") {
      h5(strong(paste0("Your account is locked because of too many\n",
                       "failed login attempts. Contact administrator."), style = "color:red"), align = "center")
    } else if (user_input$status == "credentials_data_error") {    
      h5(strong("Credentials data error - contact administrator!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_user") {
      h5(strong("User name not found!", style = "color:red"), align = "center")
    } else if (user_input$status == "bad_password") {
      h5(strong("Incorrect password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })
  
  ##################################################################################################################################################
  
  # END PASSWORD SETTING ###########################################################################################################################
  
  ##################################################################################################################################################
  
  
  
  }) # end of server function ########################################################################################################################




#####################################################################################################################################################
#####################################################################################################################################################
#####################################################################################################################################################





# Run the app -----------------------------------------------------------------------

shinyApp(ui, server)
