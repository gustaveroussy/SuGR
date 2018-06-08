#Input Request size
options(shiny.maxRequestSize=200*1024^2)

#number of fails for the password
num_fails_to_lockout <- 3


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


# END OF LIBRARIES =================================================================================================================================
#===================================================================================================================================================







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
toStop=reactiveValues(status=F)
dir_file = reactiveValues(val=NULL)

# END OF reactive values ===========================================================================================================================
#===================================================================================================================================================






# server ###########################################################################################################################################

shinyServer(function(input, output, session) {
  
  #### UI code --------------------------------------------------------------
  output$ui <- renderUI({
    if (user_input$authenticated == FALSE) {
      
      ##### UI code for login page
      fluidPage(
        
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass"),
                 uiOutput("connect")
                 )
          
        ),
        hr(),
        div(style='color:grey', p(style = "text-align:center", 
                                  "Please contact Alicia TRAN DIEN (alicia.tran-dien@gustaveroussy.fr) from", 
                                  img(src = "logo.jpg", height = 50, width = 80),
                                  "Bioinformatics plateform for any requests.")),
        div (style='color:grey', p(style = "text-align:center","Or download the help document for further information.")),
        br(),
        div(style="text-align: center;",
            actionButton("helper", "Help document", onclick = "window.open('SuGR.pdf')"))
        
      )
    } else {
      
      #create tmp_username folder
      dir_file$val=paste0("data/tmpData/",input$user_name,"/")
      
      if (dir.exists(dir_file$val)){
        file.remove(list.files(dir_file$val,full.names=T))
        
      } else{
        dir.create(dir_file$val)
      }
      
      
      # Define UI for data upload app -------------------------------------------------------
      
      jscode <- "shinyjs.closeWindow = function() { window.close(); }"
      ui <- fluidPage(
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        
        
        #navigation bar
        
        navbarPage(
          textOutput(""),
          
          
          
          
          
          #Input for CEL ############################################################################################################################
          tabPanel(p(style = "font-size:20px;font-weight:bold","CEL"),
                   sidebarLayout(
                     
                     # Sidebar panel for inputs --------------------------------------------------------
                     
                     sidebarPanel(
                       
                       fileInput("file2", "Select CEL file",
                                 multiple = TRUE,
                                 accept = c(".CEL",
                                            ".CEL.bz2"), 
                                 placeholder = "Select your CEL file"
                                 
                       ),
                       br(),
                       br(),
                       div(style='color:grey',strong ("List of files that will be uploaded on Synapse")),
                       br(),br(),
                       DT::dataTableOutput("tablesynIDCEL"), # output list of files to be uploaded on synapse
                       br(),
                       uiOutput("PurgeListCelButton"), # purge list of files to be uploaded on synapse button
                       br(),br(),br()
                     ), # end of sidebarPanel
                     
                     
                     # Main panel for displaying outputs -------------------------------------------------
                     
                     mainPanel(
                       uiOutput("select_input_cel"),
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
                       uiOutput("UploadButtonALLCEL"), # upload file on Synapse button
                       br(),
                       uiOutput("UploadALLCEL"), # upload file on synapse
                       br(),br(),br(),
                       uiOutput("PurgeListCel"), # purge list of files to upload
                       br(),
                       htmlOutput("UploadTextCEL"), # upload on Synapse confirmation
                       br(),
                       uiOutput("UploadButtonCEL") # button to upload CEL on Synapse
                       
                       
                     ) # end of main panel
                     
                     
                   ) # end of sidebarLayout
                   
          ), #end of tabCEL =========================================================================================================================
          #==========================================================================================================================================
          
          
          
          #INPUT FOR NGS ############################################################################################################################
          
          tabPanel(p(style = "font-size:20px;font-weight:bold","NGS"),
                   
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
                      
                       br(),
                       br(),
                       div(style='color:grey',strong ("List of files that will be uploaded on Synapse")),
                       br(),br(),
                       DT::dataTableOutput("tablesynIDNGS"), # list of files to be uploaded on synapse
                       br(),
                       uiOutput("PurgeListNgsButton"), # purge list of files to be uploaded on synapse button
                       br(),br(),br(),  
                       hr(style = 'border-color:darkgrey'),
                       div (style='color:grey',strong ("VC SampleSheet Example")),
                       br(),
                       actionButton("tsv", "SampleSheetExample", onclick = "window.open('SampleSheetExample_VC.tsv')"), #downadload samplesheet
                       hr(style = 'border-color:darkgrey'),
                       div (style='color:grey',strong ("list of HUGO genes name")), # dowload list of genes from HUGO
                       br(),
                       actionButton("gename", "GeneName", onclick = "window.open('list_genes.txt')")
                       
                     ), #end of sidebarPanel
                     
                     
                     # Main panel for displaying outputs -------------------------------------------------
                     
                     mainPanel(
                       
                       uiOutput("select_input_ngs"),
                       htmlOutput("NGScheckName"), # check input name
                       br(),
                       htmlOutput("NGScheckTOR"), # check if TOR
                       br(),
                       htmlOutput("NGScheckColumn"), # check columns
                       br(),
                       htmlOutput("NGScheckGene"), # check gene name
                       br(),
                       br(),
                       br(),
                       uiOutput("UploadButtonALLNGS"), # upload file on Synapse button
                       br(),
                       uiOutput("UploadALLNGS"), # upload all the files on synapse
                       br(),
                       uiOutput("PurgeListNgs"), # purge list of files to be uploaded
                       br(),br(),br(),
                       uiOutput("SaveChanges"), # save changed made on edit table
                       br(),br(),br(),
                       rHandsontableOutput("hot") # table to be edited
                       
                     ) #end of main panel
                     
                     
                   ) #end of sidebarLayout
                   
          ) #end of tabNGS ==========================================================================================================================
          #==========================================================================================================================================
          
          
          
        ), #end of navbarpage
        
        # providing help
        hr(),
        div(style='color:grey', p(style = "text-align:center", 
                                  "Please contact Alicia TRAN DIEN (alicia.tran-dien@gustaveroussy.fr) from", 
                                  img(src = "logo.jpg", height = 50, width = 80),
                                  "Bioinformatics plateform for any requests.")),
        div (style='color:grey', p(style = "text-align:center","Or download the help document for further information.")),
        br(),
        div(style="text-align: center;",
            actionButton("helper", "Help document", onclick = "window.open('SuGR.pdf')")),
        div(style="text-align: right;",
            actionButton("close", "Close window"))
        
      ) #end of fluidPage
      
    } # end of else if password good
    
  }) # end of rendUI
  
  #===================================================================================================================================================
  #===================================================================================================================================================
  #===================================================================================================================================================
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################################################################################################################################################
  
  # CEL INPUT #
  
  ##################################################################################################################################################
  
  
  ####################
  # Check file name  #
  ####################
  
  
  # create folder tmp and create dropdown menu with all the inputs selected ------------------------------------------------------------------------
  observeEvent(input$file2, {
    
    
    
    file.remove(list.files(dir_file$val,full.names = T))
    file.copy(from = input$file2$datapath,to = paste0(dir_file$val,input$file2$name))
    
    
    files=list.files(dir_file$val,full.names =T) 
    file_list=list()
    for(i in files){
      
      file_list[[basename(i)]]=
        
        if (length(grep (pattern = ".CEL|.CEL.bz2", i)) > 0 ) {}
    }
    files.list_cel$valeur=file_list
    
    
    output$select_input_cel=renderUI(
      selectInput(inputId = "uploaded_files_cel",label = "Uploaded files:",choices = basename(files)))
    
    
    
  }) #========================================================================================================================= end of dropdown menu
 
  
  
  
  # check if SAFIR-TOR ----------------------------------------------------------------------------------------------------------------------------
   observeEvent(input$uploaded_files_cel,{
    
  
      tmp_fileCEL <- paste0(dir_file$val,input$uploaded_files_cel)
      in.file_cel$valeur = tmp_fileCEL
      
      new_names = gsub(" |-_|_-|-|\\(|\\)","_",tmp_fileCEL)
      innew.file_cel$valeur = new_names
      
      torgrep <- grep(pattern = "^T[0-9]", input$uploaded_files_cel)
      
      if (length(torgrep) > 0){
        output$CELcheckTOR <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:red',
                                              p("!!! WARNING !!! The file uploaded", basename(input$uploaded_files_cel), "is a SAFIR02-Tor File")))))
        
        output$CELcheck1<-renderUI(p(""))
        output$CELcheck3<-renderUI(p(""))
      } else {
        
        
        output$CELcheckTOR <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:green',
                                              p("The file uploaded", basename(input$uploaded_files_cel), "is a SAFIR02 File")))))
        
      }

  }) #================================================================================================================ End of check if SAFIR-TOR

  
  
  
    
  # check if wrong typo in file name -----------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_cel,{
      spacegrep <- grep(pattern = " |-_|_-|-|\\(|\\)", in.file_cel$valeur)
      if (length(spacegrep) > 0 ) {
        file.rename(in.file_cel$valeur,innew.file_cel$valeur)
        output$CELcheck1 <- renderText(
          paste0(div(style='color:black', div(style = 'background-color:orange',
                                              p("!! WARNING !! A special character has been found in file:", 
                                                basename(in.file_cel$valeur), "It has now been renamed:", basename(innew.file_cel$valeur))))))
        
      } else{
        
        output$CELcheck1 <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:green',
                                              p(basename(in.file_cel$valeur),"name is good")))))
        in.file_cel$valeur <- innew.file_cel$valeur
        
      }

  })  #================================================================================================================ end of check file name   
  
  
  
  
  # check if Onco or Cyto scan ---------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_cel,{
    platform <- readRDS("data/appData/plateform.rds")
    row_username <- which(platform$user == input$user_name)
    
    if (length(grep (pattern = ".bz2",  innew.file_cel$valeur)) > 0 ) {
      celtodezip <- gsub (pattern = "\\.bz2$", "",  innew.file_cel$valeur)
      dezipcel=NULL
      
      withProgress(expr = {try( {dezipcel <- bunzip2( innew.file_cel$valeur, celtodezip , remove = FALSE, skip = TRUE)})
      }, message = "Reading file... Please wait")
      
    } else {
      dezipcel <-  innew.file_cel$valeur
    }
    
    dezip.cel$valeur = dezipcel
    
    
    withProgress(expr = {
      read= NULL
      try({read <- readCelHeader(dezip.cel$valeur)})
    }, message = "Reading file... Please wait") 
    
    if(is.null(read)){
      output$select_input_cel=renderUI(div(style='color:white',div(style = 'background-color:red',
                                          p("Oups!! The file format is incorrect please try again"))))
      output$CELcheckTOR=renderUI(p(""))
      output$CELcheck1=renderUI(p(""))
      output$CELcheck3=renderUI(p(""))
      toStop$status=T
  
      }else{
      
      toStop$status=F
      
      reading.cel$valeur = read
      
      
      if (reading.cel$valeur$chiptype == "OncoScan_CNV") {
        output$CELcheck3 <- renderText(
          paste0(div(style='color:grey', p("The file uploaded",basename(innew.file_cel$valeur),"is an OncoScan",sep = ""))))
        
        celId <-  as.character(platform[row_username, "synonco"])
        syn.IDCEL$valeur = celId
        
      } else if (reading.cel$valeur$chiptype == "CytoScanHD_Array") {
        
        output$CELcheck3 <- renderText(
          paste0(div(style='color:grey', p("The file uploaded",basename(innew.file_cel$valeur),"is a CytoScan", sep = ""))))
        
        celId <-  as.character(platform[row_username, "syncyto"])
        syn.IDCEL$valeur = celId
        
      }
    }
  }) #=================================================================================================================== end of onco or cytoscan
  
  
  
  
  # upload to synapse ---------------------------------------------------------------------------------------------------------------------------
  
  
  table_synIDCEL = read.table("credentials/table_syn2.tsv", header = TRUE , sep = "\t")
  table.synIDCEL$valeur = table_synIDCEL
  
  
  observeEvent(input$uploaded_files_cel,{
    
    if(!toStop$status){
      
    
    
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
    }
    
  }) #============================================================================================================================== end of uploadCEL
  
  
  #==================================================================================================================================================
  # END OF CEL INPUT ================================================================================================================================
  #===================================================================================================================================================
  
  
  
  
  
  
  
  
  
  
  ##################################################################################################################################################
  
  # NGS INPUT #
  
  ##################################################################################################################################################
  
  
  ####################
  # Check file name  #
  ####################
  
  
  # create folder tmp and create dropdown menu with all the inputs selected ------------------------------------------------------------------------
  
  observeEvent(input$file1,{
    
    
    
    file.remove(list.files(dir_file$val,full.names = T))
    file.copy(from = input$file1$datapath,to = paste0(dir_file$val,input$file1$name))
    files=list.files(dir_file$val,full.names =T) 
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
    
    
    
  }) #======================================================================================================== end of observe event dropdown menu
  
  
  
  
  # check if it s a TOR file --------------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{
    tmp_fileNGS=paste0(dir_file$val,input$uploaded_files_ngs)
    in.file_ngs$valeur = tmp_fileNGS
    
    new_namesNGS = gsub(" |-_|_-|-|\\(|\\)","_",tmp_fileNGS)
    innew.file_ngs$valeur = new_namesNGS
    
    
    torgrep <- grep(pattern = "T[0-9]", tmp_fileNGS)
    if (length(torgrep) > 0){
      output$NGScheckTOR <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:red',
                                            p("!!! WARNING !!! The file uploaded", basename(input$uploaded_files_ngs), "is a SAFIR02-Tor File")))))
      output$NGScheckName<-renderUI(p(""))
      output$NGScheckColumn<-renderUI(p(""))
      output$NGScheckGene<-renderUI(p(""))
    } else {
      
      output$NGScheckTOR <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:green',
                                            p("The file uploaded", basename(input$uploaded_files_ngs), "is a SAFIR02 File")))))
    } 
    
  }) #============================================================================================================== end of else if not TOR its safir 
  
  
  
  
  
  
  # check file name if so and rename it if needed --------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{
    
    
    spacegrepNGS <- grep(pattern = " |-_|_-|-|\\(|\\)", in.file_ngs$valeur)
    if (length(spacegrepNGS) > 0 ) {
      
      
      file.rename(in.file_ngs$valeur,innew.file_ngs$valeur )
      
      output$NGScheckName <- renderText(
        paste0(div(style='color:black', div(style = 'background-color:orange',
                                            p("!! WARNING !! A special character has been found in file:", 
                                              basename(in.file_ngs$valeur),"It has now been renamed",basename(innew.file_ngs$valeur) )))))
      
      
    } else{
      
      output$NGScheckName <- renderText(
        paste0(div(style='color:white', div(style = 'background-color:green',
                                            p(basename(in.file_ngs$valeur),"name is good")))))
      
      innew.file_ngs$valeur  <- in.file_ngs$valeur
      
    } 
    
    
  }) #===================================================================================================================== end of else if good name
  
  
  
  
  
  #############################################
  #   Now that file name is ok process file   #
  #############################################
  
  
  
  
  # check columns ---------------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{
    
    ACfiles <- grep(pattern = "_AC", in.file_ngs$valeur)
    if (length(ACfiles) > 0 ) {
      
      
      
      
      output$hot <- renderRHandsontable({

                df <- files.list_ngs$valeur[[basename(in.file_ngs$valeur)]]
        
                withProgress(expr = {rhandsontable(df, height = 550) %>%
                  hot_table(highlightCol = TRUE, highlightRow = TRUE, allowRowEdit=TRUE)}, message = "Reading file... Please wait")
      })
      
  
      
    } else {
      
      df <- files.list_ngs$valeur[[basename(in.file_ngs$valeur)]]
      final = NULL
      try({final <- subset(df, select=c(PatId,Global_Conclusion,Manual_Var_Comment,Manual_Var_Classif,Gene_Symbol,Protein_Change,Exon,
                                   Variant_Freq,Position_Cov,RefSeq_Id,cDNA_Change,Codon,Chr,Start_Position,End_Position,
                                   Strand,Type,Reference_Seq,Variant_Seq,Variant_Cov,MAF_classification,ESP_Freq,by1000G_Freq,
                                   DbSNP_Id,COSMIC_Id,Quality,Strand_Bias,Amplicon_Ref))})
      if(is.null(final)){
        output$select_input_ngs <- renderUI(div(style='color:white',div(style = 'background-color:red',
                                                                        p("Oups!! The file format is incorrect please try again"))))
        output$NGScheckTOR=renderUI(p(""))
        output$NGScheckName=renderUI(p(""))
        output$NGScheckGene=renderUI(p(""))
        output$NGScheckColumn=renderUI(p(""))
        toStop$status=T
      } else {
        toStop$status=F
      final.table$valeur = final
      
      
      pattern = c("Polyphen2_Prediction" , "SIFT_Prediction" ,"POLYPHEN2_HDIV_SCORE" , 
                  "POLYPHEN2_HDIV_INTERPRETATION" , "POLYPHEN2_HVAR_SCORE" , "POLYPHEN2_HVAR_INTERPRETATION")
      columns <- colnames(df)
      
      
      if (length(grep(paste(pattern, collapse = "|"), columns) > 0)){
        output$NGScheckColumn <- renderText(
          paste0(div(style='color:black', div(style = 'background-color:orange', p("!!!! WARNING !!!!",basename(innew.file_ngs$valeur),"Columns have been changed.", 
                                                                                strong("Please download the new table.")))))) 
        
        
        
      }  else {
        
        output$NGScheckColumn <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:green', p("Columns are ok.")))))
        
      } 
    }
    }
  }) #================================================================================================================================= end of else 
  
  
  
  
  # check gene typo --------------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{
    ACfiles <- grep(pattern = "_AC", in.file_ngs$valeur)
    if (length(ACfiles) > 0 ) {
      df <- files.list_ngs$valeur[[basename(in.file_ngs$valeur)]]
      final.table$valeur = df

      
    } else {
      gene <- read.delim("www/list_genes.txt")
      gene_symbol = NULL
      try({gene_symbol <- select(final.table$valeur,Gene_Symbol)})
      if(is.null(gene_symbol)){
        output$select_input_ngs=renderUI(div(style='color:white',div(style = 'background-color:red',
                                                                     p("Oups!! The file format is incorrect please try again"))))
        output$NGScheckTOR=renderUI(p(""))
        output$NGScheckName=renderUI(p(""))
        output$NGScheckGene =renderUI(p(""))
        output$NGScheckColumn=renderUI(p(""))
        toStop$status=T
      }else{
        
        toStop$status=F
      
        gene_symbol <- select(final.table$valeur,Gene_Symbol)
      difference <- data.frame(lapply(1:ncol(gene_symbol),function(i)setdiff(gene_symbol[,i],gene[,i])))
      colnames(difference) <- colnames(gene_symbol)
      row.number$valeur = as.character(rownames(difference))
      TEST = (as.character(difference))
      lenrow <- nrow(difference)

      if (lenrow > 0) {
        output$NGScheckGene <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:red',
                                              p("!!! WARNING !!! Gene typo wrong,
                                                please check gene name nomenclature and edit the table")))))



      } else {
        output$NGScheckGene <- renderText(
          paste0(div(style='color:white', div(style = 'background-color:green', p("Gene name ok")))))


      }
    }
    }
  }) #======================================================================================================================= end of checking gene


  # display editable table -----------------------------------------------------------------------------------------------------------------------

  observeEvent(input$uploaded_files_ngs,{
    ACfiles <- grep(pattern = "_AC", in.file_ngs$valeur)
    if (length(ACfiles) > 0 ) {

   
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
          withProgress(expr = {write.table(hot_to_r(input$hot), innew.file_ngs$valeur , quote = FALSE, sep = "\t",row.names = FALSE)},
                       message = "Saving modifications... Please wait")
          
          
        }
      })


      output$hot <- renderRHandsontable({

        DF = final.table$valeur
        gene <- read.delim("www/list_genes.txt")

        gene_symbol=NULL
        try({gene_symbol <- select(DF,Gene_Symbol)})
        if(is.null(gene_symbol)){
          output$select_input_ngs=renderUI(div(style='color:white',div(style = 'background-color:red',
                                                                       p("Oups!! The file format is incorrect please try again"))))
          toStop$status=T
        } else {
        toStop$status=F  
        gene_symbol <- select(DF,Gene_Symbol)
        difference <- lapply(1:ncol(gene_symbol),function(i)setdiff(gene_symbol[,i],gene[,i]))
        list_gene <- unlist(difference)
        
       rhandsontable(DF, list_gene = list_gene, height = 550) %>%
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
        }
    })

}


}) #================================================================================================================= end of display editable table




  # Check if AC or VC ----------------------------------------------------------------------------------------------------------------------------
  observeEvent(input$uploaded_files_ngs,{

    # connect to synapse


    platform <- readRDS("data/appData/plateform.rds")

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



  }) #============================================================================================================================= end of AC or VC




  # sign in and upload to synapse -----------------------------------------------------------------------------------------------------------------
  table_synIDNGS = read.table("credentials/table_syn.tsv", header = TRUE , sep = "\t")
  table.synIDNGS$valeur = table_synIDNGS


  observeEvent(input$uploaded_files_ngs,{
    if(!toStop$status){
      
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

     }

  })


  #=================================================================================================================================================
  # END OF NGS INPUT ===============================================================================================================================
  #=================================================================================================================================================
  
  
  
  
  # purge list to upload button ###################################################################################################################


  observeEvent(input$file2,{

    output$PurgeListCelButton <- renderUI({
      mainPanel(
        actionButton("purge_list_cel", "Purge list to upload"))})

    output$PurgeListCel <- eventReactive(input$purge_list_cel, {
      files_to_delete = paste0(dir_file$val,"/*")
      withProgress(expr = {unlink(files_to_delete, recursive=TRUE)}, message = "Deleting tmp file... Please wait")
      table.synIDCEL$valeur=table.synIDCEL$valeur[0,]
      print(table.synIDCEL$valeur)
      shinyjs::disable("tablesynIDCEL")
      
    })
  })


  observeEvent(input$file1,{

    output$PurgeListNgsButton <- renderUI({
      mainPanel(
        actionButton("purge_list_ngs", "Purge list to upload"))})

    output$PurgeListNgs <- eventReactive(input$purge_list_ngs, {
      files_to_delete = paste0(dir_file$val,"/*")
      withProgress(expr = {unlink(files_to_delete, recursive=TRUE)}, message = "Deleting tmp file... Please wait")
      table.synIDNGS$valeur=table.synIDNGS$valeur[0,]
      print(table.synIDNGS$valeur)
      shinyjs::disable("table.synIDNGS")
      
    })
  })

  
#====================================================================================================================== purge list to upload button 
#==================================================================================================================================================
#==================================================================================================================================================
  
  
  
  
  
  
  
  
  
  # print list of file that will be uploaded #####################################################################################################
  observeEvent(input$file2,{
    output$tablesynIDCEL = renderDataTable(
      table.synIDCEL$valeur,
      options = list(dom = 't',
                     autoWidth = TRUE,
                     scrollX = TRUE,
                     columnDefs = list(list(width = '50%', targets = list(1,2)))
                     
      )
      ) 
    })
  
  
  observeEvent(input$file1,{
    output$tablesynIDNGS = renderDataTable(
      table.synIDNGS$valeur,
      options = list(dom = 't',
                     autoWidth = TRUE,
                     scrollX = TRUE,
                     columnDefs = list(list(width = '50%', targets = list(1,2)))
                     
      )
    ) 
  })
  
#======================================================================================================================= end of file to be uploaded
#==================================================================================================================================================
#==================================================================================================================================================
  
  
  
  
  ##################################################################################################################################################
  
  # PASSWORD SETTING #
  
  ##################################################################################################################################################
  
  
  #### PASSWORD server code ---------------------------------------------------- 
  # reactive value containing user's authentication status
  user_input <- reactiveValues(authenticated = FALSE, valid_credentials = FALSE, 
                               user_locked_out = FALSE, status = "",user_name=NULL)
  

  
  
  
  observeEvent(input$login_button, {
    
    plateform <- readRDS("data/appData/plateform.rds")
    row_username <- which(plateform$user == input$user_name)
    if (length(row_username) == 1){
    withProgress(expr = {synapseLogin(input$user_name, input$password)}, message = "Connecting to Synapse... Please wait")   
    user_input$authenticated <- TRUE
    } else {
      output$connect=renderUI(div(style='color:white',div(style = 'background-color:red',
                                                                   p("Oups!! Log in incorrect. Please enter your Synapse user name and password. Contact administrator for help."))))
      
    }  
  })
    output$uiLogin <- renderUI({
    wellPanel(
        semanticPage(
        suppressDependencies("bootstrap")
      ),
      
      textInput("user_name", "User Name:"),
      
      passwordInput("password", "Password:"),
      
      actionButton("login_button", "Log in")
    )
  })
  
  #=================================================================================================================================================
  # END PASSWORD SETTING ===========================================================================================================================
  #=================================================================================================================================================
  
    
    
    
    
    
    
    
    
    
###################################################################################################################################################
# CLOSING APP SETTINGS AND DISCONNECT FROM SYNAPSE ################################################################################################
###################################################################################################################################################
    
observeEvent(input$close, {
withProgress(expr = {synapseLogout()}, message = "Disconnecting from Synapse... Please wait")
withProgress(expr = {unlink(dir_file$val, recursive=TRUE)}, message = "Deleting tmp file... Please wait")
js$closeWindow()
stopApp()
    
})

#===================================================================================================================================================
# CLOSING APP SETTINGS AND DISCONNECT FROM SYNAPSE =================================================================================================
#===================================================================================================================================================   
    
    
    
    
   }) # end of server