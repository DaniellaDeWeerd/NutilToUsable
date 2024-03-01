library(shiny)
library(readr)
library(stringr)
library(ggplot2)
library(dplyr)
library(pheatmap)
library(viridis)
library(tidyverse)
library(colourpicker)
library(grid)
library(gridExtra)

library(data.tree)


options(shiny.maxRequestSize = 300 * 1024^2)

#You shouldn't have to touch this
##### PAGE LAYOUT #####
ui <- fluidPage(
  titlePanel("N2U"),
  tabsetPanel(
    tabPanel("Nutil Data",
      sidebarLayout(
        sidebarPanel(
          style = "height: 95vh; overflow-y: auto;", 
          #Load in Nutil Files (left and right)
          fileInput("left","Please input all Nutil Reference Atlas files for the left side:",multiple = TRUE),
          fileInput("right","Please input all Nutil Reference Atlas files for the right side:",multiple = TRUE),
          #load in tree
          actionButton("tree","Click this when all files are done loading"),
          tags$h1(""),
          #download to get blank annotation file
          downloadButton("blankAnnotation","Download blank annotation file"),
          tags$h1(""),
          #Checkpoint 1
          downloadButton("saveOutput1","Download Checkpoint 1"),
          tags$h1(""),
          #input checkpoint 1
          fileInput("reLoad1","Input checkpoint 1 here"),
          #load in annotation file
          fileInput("annotation","Please input the csv file that has the annotation information."),
          #Checkpoint 2
          downloadButton("saveOutput2","Download Checkpoint 2"),
          #input checkpoint 2
          fileInput("reLoad2","Input checkpoint 2 here (RDS|Old Checkpoint)"),
          fileInput("reLoad2csv","Input checkpoint 2 here (csv)"),
          #choose X variables
          selectizeInput("x","Select your x axis variables to create heatmap",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
          #choose Y variables
          selectizeInput("y","Select your y axis variables to create heatmap",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
          #choose display X variables
          # selectizeInput("displayX","Select your variables to display on x axis",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
          #choose display Y variables
          selectizeInput("displayY","Select your variables to display on y axis",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
          
          #button to run initial steps of heatmap
          actionButton("options","Get Options"),
          tags$h1(""),
          #x axis displays to see
          selectizeInput("optionsX","Select your variables to see segmented on x axis",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
          #choose variables to sum with
          selectizeInput("sumWith","Select the nutil variable to create value with",choices=c("Don't select yet"),multiple = F,options = list(create = FALSE)),
          #Choose if percentage is created using the following selections
          checkboxInput("percent", "Multiply by 100 to make a percent?", value = FALSE),
          # #create a percentage using variables
          # checkboxInput("toGroup", "Use variables to create a percentage?", value = FALSE),
          # #choose variables to group by
          # selectizeInput("groupBy","Select the variables to group by",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
          
          #log or not?
          checkboxInput("log", "Should it be in log 10 scale?", value = FALSE),
          #trimmed?
          checkboxInput("trim", "Should it be trimmed?", value = FALSE),
          #Select variables to remove
          selectizeInput('toRemove', "Select any regions to remove",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
          #Choose x and y names to display
          selectizeInput("xNames","Select the display variable to use as row names",choices=c("Don't select yet"),multiple = F,options = list(create = FALSE)),
          selectizeInput("yNames","Select the display variable to use as col names",choices=c("Don't select yet"),multiple = F,options = list(create = FALSE)),
          #slider for color scale
          sliderInput("colorScale", "Color Scale:", min = -100, max = 100, value = c(0,100),step = 0.5),
          numericInput("min_value", "Color Min Value", 0, min = -100, max = 100,step=0.5),
          numericInput("max_value", "Color Max Value", 100, min = -100, max = 100,step=0.5),
          
          #button to display normal heatmap
          actionButton("normal","Create heatmap"),
          tags$h1("Custom Colors"),
          #invert the scales if viridis
          checkboxInput("inverted", "Invert the viridis color scale", value = FALSE),
          #checkbox to choose from viridis color scales
          checkboxInput("viridis", "Choose a different viridis color scale", value = FALSE),
          #choose viridis color scale
          selectInput("viridisScale", "Choose viridis color scale", choices = c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako","turbo"), selected = "inferno"),
          #checkbox to choose 2 color scale
          checkboxInput("color2", "Choose 2 color scale", value = FALSE),
          #choose colors for 2 color scale
          colourpicker :: colourInput("col1", "Select colour low", "white", showColour = "background"),
          colourpicker :: colourInput("col2", "Select colour high", "red", showColour = "background"),
          tags$h1(""),
          #checkbox to choose 3 color scale
          checkboxInput("color3", "Choose 3 color scale", value = FALSE),
          #choose colors for 3 color scale
          colourpicker :: colourInput("colo1", "Select colour low", "white", showColour = "background"),
          colourpicker :: colourInput("colo2", "Select colour medium", "orange", showColour = "background"),
          colourpicker :: colourInput("colo3", "Select colour high", "red", showColour = "background"),
          tags$h1(""),
          #Load in SVG
          # actionButton("SVG","Click this to initiate anatomical heatmap"),
          # #choose from all variables to display anatomically  
          # selectizeInput("annoVariable","Select your variable to display on anatomical heatmap",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
          # #button to display anatomical heatmap
          # actionButton("anatomical","Create anatomical heatmap"),
          tags$h1(""),
          downloadButton("data","Download Data"),
          downloadButton("annoX","Download AnnoX"),
          downloadButton("annoY","Download AnnoY"),
          downloadButton("heatmap","Download full Heatmap"),
          downloadButton("colors","Download Colors"),
          downloadButton("justPlot","Download Just the Plot"),
          # downloadButton("anatomical","Download Anatomical"),
          downloadButton("mbh","Download MBH table"),
        ),
        mainPanel(
          textOutput("print"),
          textOutput("minMax"),
          #normal heatmap
          plotOutput("normPlot"),
          #anatomical heatmap
          plotOutput("anatoPlot"),
        )
      )
    ),
    tabPanel("Non Nutil Data",
     sidebarLayout(
       sidebarPanel(
         style = "height: 95vh; overflow-y: auto;", 
         #Load in Nutil Files (left and right)
         tags$p("The column names should include 'region', 'side', and then any columns of data with unique names."),
         fileInput("theInput","Please input your file:",multiple = F),
         #load in tree
         actionButton("tree_2","Click this when all files are done loading"),
         tags$h1(""),
         #download to get blank annotation file
         downloadButton("blankAnnotation_2","Download blank annotation file"),
         tags$h1(""),
         #Checkpoint 1
         downloadButton("saveOutput1_2","Download Checkpoint 1"),
         tags$h1(""),
         #input checkpoint 1
         fileInput("reLoad1_2","Input checkpoint 1 here"),
         #load in annotation file
         fileInput("annotation_2","Please input the csv file that has the annotation information."),
         #Checkpoint 2
         downloadButton("saveOutput2_2","Download Checkpoint 2"),
         #checkpoint2 load
         fileInput("reLoad2csv_2","Input checkpoint 2 here (csv)"),
         #choose X variables
         selectizeInput("x_2","Select your x axis variables to create heatmap",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
         #choose Y variables
         selectizeInput("y_2","Select your y axis variables to create heatmap",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
         #choose display X variables
         # selectizeInput("displayX","Select your variables to display on x axis",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
         #choose display Y variables
         selectizeInput("displayY_2","Select your variables to display on y axis",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
         
         #button to run initial steps of heatmap
         actionButton("options_2","Get Options"),
         tags$h1(""),
         #x axis displays to see
         selectizeInput("optionsX_2","Select your variables to see segmented on x axis",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
         #Choose if percentage is created using the following selections
         checkboxInput("percent_2", "Multiply by 100 to make a percent?", value = FALSE),
         #log or not?
         checkboxInput("log_2", "Should it be in log 10 scale?", value = FALSE),
         #trimmed?
         checkboxInput("trim_2", "Should it be trimmed?", value = FALSE),
         #Select variables to remove
         selectizeInput('toRemove_2', "Select any regions to remove",choices=c("Don't select yet"),multiple = T,options = list(create = FALSE)),
         #Choose x and y names to display
         selectizeInput("xNames_2","Select the display variable to use as row names",choices=c("Don't select yet"),multiple = F,options = list(create = FALSE)),
         selectizeInput("yNames_2","Select the display variable to use as col names",choices=c("Don't select yet"),multiple = F,options = list(create = FALSE)),
         #slider for color scale
         sliderInput("colorScale_2", "Color Scale:", min = -100, max = 100, value = c(0,100),step = 0.5),
         numericInput("min_value_2", "Color Min Value", 0, min = -100, max = 100,step=0.5),
         numericInput("max_value_2", "Color Max Value", 100, min = -100, max = 100,step=0.5),
         
         #button to display normal heatmap
         actionButton("normal_2","Create heatmap"),
         tags$h1("Custom Colors"),
         #invert the scales if viridis
         checkboxInput("inverted_2", "Invert the viridis color scale", value = FALSE),
         #checkbox to choose from viridis color scales
         checkboxInput("viridis_2", "Choose a different viridis color scale", value = FALSE),
         #choose viridis color scale
         selectInput("viridisScale_2", "Choose viridis color scale", choices = c("viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako","turbo"), selected = "inferno"),
         #checkbox to choose 2 color scale
         checkboxInput("color2_2", "Choose 2 color scale", value = FALSE),
         #choose colors for 2 color scale
         colourpicker :: colourInput("col1_2", "Select colour low", "white", showColour = "background"),
         colourpicker :: colourInput("col2_2", "Select colour high", "red", showColour = "background"),
         tags$h1(""),
         #checkbox to choose 3 color scale
         checkboxInput("color3_2", "Choose 3 color scale", value = FALSE),
         #choose colors for 3 color scale
         colourpicker :: colourInput("colo1_2", "Select colour low", "white", showColour = "background"),
         colourpicker :: colourInput("colo2_2", "Select colour medium", "orange", showColour = "background"),
         colourpicker :: colourInput("colo3_2", "Select colour high", "red", showColour = "background"),
         tags$h1(""),
         tags$h1(""),
         downloadButton("data_2","Download Data"),
         downloadButton("annoX_2","Download AnnoX"),
         downloadButton("annoY_2","Download AnnoY"),
         downloadButton("heatmap_2","Download full Heatmap"),
         downloadButton("colors_2","Download Colors"),
         downloadButton("justPlot_2","Download Just the Plot"),
         # downloadButton("anatomical","Download Anatomical"),
         downloadButton("mbh_2","Download MBH table"),
       ),
       mainPanel(
         textOutput("print_2"),
         textOutput("minMax_2"),
         #normal heatmap
         plotOutput("normPlot_2"),
         #anatomical heatmap
         plotOutput("anatoPlot_2"),
       )
     )
    )
  )
)


server <- function(session, input, output) {
  ##### GLOBAL VARIABLES #####
  vals <<- reactiveValues(
    fullData = data.frame(matrix(nrow = 0, ncol = 13)),
  )
  
  observeEvent(input$colorScale,{
    updateNumericInput(session,"min_value","Min Value", input$colorScale[1], min = -100, max = 100,step=0.5)
    updateNumericInput(session,"max_value","Max Value", input$colorScale[2], min = -100, max = 100,step=0.5)
  })
  
  ##### LOAD IN DATA #####
  observeEvent(input$left,{
    the_files <- input$left
    names <- the_files$name
    paths <- the_files$datapath
    for (i in 1:length(paths)){
      toNormal <- read_delim(paths[i],delim = ";", escape_double = FALSE, trim_ws = TRUE)
      toNormal <- toNormal[,1:10]
      name <- names[i]
      for (j in 2:length(toNormal[,1])) {
        name <- rbind(name,name)
      }
      side <- rep("left", length(toNormal[,1]))
      toAdd <- cbind(name,side,toNormal)
      vals$fullData <- rbind(vals$fullData,toAdd)
    }
    vals$fullData <- vals$fullData[vals$fullData$`Region pixels` != 0,]
    
    output$print <- renderText({
      print(paste0(Sys.time()," Done Left"))
    })
  })
  
  observeEvent(input$right,{
    the_files <- input$right
    names <- the_files$name
    paths <- the_files$datapath
    for (i in 1:length(paths)){
      toNormal <- read_delim(paths[i],delim = ";", escape_double = FALSE, trim_ws = TRUE)
      toNormal <- toNormal[,1:10]
      name <- names[i]
      for (j in 2:length(toNormal[,1])) {
        name <- rbind(name,name)
      }
      side <- rep("right", length(toNormal[,1]))
      toAdd <- cbind(name,side,toNormal)
      vals$fullData <- rbind(vals$fullData,toAdd)
    }
    vals$fullData <- vals$fullData[vals$fullData$`Region pixels` != 0,]
    
    output$print <- renderText({
      print(paste0(Sys.time()," Done Right"))
    })
  })
  
  #Load tree
  observeEvent(input$tree,{
    # file <- input$tree
    tree <<- readRDS("RequiredFiles/flippedTree.rds")#readRDS(file = file$datapath)
    print("Data read")
    
    fullData <<- vals$fullData
    
    #fix ectorhinal area (ECT)
    ECTrows <- grep("ecto",fullData$`Region Name`,ignore.case = T)
    ECTSubset <- fullData[ECTrows,]
    
    fullData[ECTrows,"Region Name"] <- sub("/", ",", fullData[ECTrows,"Region Name"], fixed=TRUE)
    
    #Find daughter, parent, and big picture regions and add to vals$fullData
    depth <- c()
    daughter <- c()
    parent <- c()
    for (i in 1:length(fullData$`Region ID`)){
      id <- fullData$`Region ID`[i]
      dep <- str_count(fullData$`Region Name`[i],',')
      depth <- c(depth,dep)
      dau <- tree[tree$X10 == id,1]
      if (length(dau) == 0){dau <- NA}
      daughter <- c(daughter, dau)
      if (dep > 1) {dep <- 1}
      par <- tree[tree$X10 == id,1+dep]
      if (length(par) == 0){par <- NA}
      parent <- c(parent,par)
    }
    fullData$depth <- depth
    fullData$daughter <- daughter
    fullData$parent <- parent
    
    vals$fullData <<- fullData
    
    
    #CHANGE THIS to have it search for particular regions
    search <- c("CTXsp","fiber tracts","HPF","HY","Isocortex","OLF","PAL","STR","TH","MB","P","MY","CB")
    specials <- c()
    i <- 1
    for (i in 1:length(vals$fullData$`Region ID`)){
      id <- vals$fullData$`Region ID`[i]
      row <- tree[tree$X10 == id,-10]
      searched <- search %in% row
      if (sum(searched)>0) {
        specials <- c(specials,search[searched])
      }
      else {
        specials <- c(specials,NA)
      }
    }
    vals$fullData$specials <- specials
    check1 <<- vals$fullData
    fullData <<- vals$fullData
    
    #TO REMOVE
    #search <- c("fiber tracts","VS")
    regionsToRemove <<- c("")
    remove <- c()
    i <- 1
    for (i in 1:length(vals$fullData$`Region ID`)){
      id <- vals$fullData$`Region ID`[i]
      row <- tree[tree$X10 == id,-10]
      searched <- regionsToRemove %in% row
      if (sum(searched)>0) {
        remove <- c(remove,"remove")
      }
      else {
        remove <- c(remove,"keep")
      }
    }
    vals$fullData <- vals$fullData[remove == "keep",]
    fullData <<- vals$fullData
    
    output$saveOutput1 <- downloadHandler(
      filename = function() {
        paste0("checkpoint1", ".rds")
      },
      content = function(file) {
        saveRDS(fullData, file)
        print("Done Downloading checkpoint 1")
      }
    )
    
    #for blank annotation
    table = vals$fullData %>% group_by(name,side)  %>%
      summarise()
    table1 <- cbind (table, mouse=NA,sex = NA,treatment=NA,mpi=NA,genotype=NA,marker=NA,batch = NA,include="Y")
    
    output$blankAnnotation <- downloadHandler(
      filename = function() {
        paste0("annotation", ".csv")
      },
      content = function(file) {
        write.csv(table1, file ,row.names = FALSE)
        print("Done Downloading Blank Annotation")
      }
    )
    
    output$print <- renderText({
      print(paste0(Sys.time()," Done Tree, Blank Annotation, and Checkpoint1"))
    })
  })
  
  #Load AfterLoad.RData
  observeEvent(input$reLoad1,{
    file <- input$reLoad1
    fullData <- readRDS(file$datapath)
    vals$fullData <- fullData
    fullData <<- fullData
    tree <<- readRDS("RequiredFiles/flippedTree.rds")
    
    output$print <- renderText({
      print(paste0(Sys.time()," Done Loading Checkpoint 1"))
    })
  })
  
  #Load annotation file
  observeEvent(input$annotation,{
    #read in annotation file
    file <- input$annotation
    annotation <<- read.csv(file$datapath)
    print("Data read")
    
    #Merge it with vals$fullData
    marker <- c()
    mouse <- c()
    treatment <- c()
    mpi <- c()
    genotype <- c()
    include <- c()
    sex <- c()
    batch <- c()
    for (i in 1:length(vals$fullData$name)){
      name <- vals$fullData$name[i]
      side <- vals$fullData$side[i]
      ### MARKER ###
      mark <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "marker"]
      if (length(mark) == 0) {mark <- NA}
      marker <- c(marker,mark)
      ### MOUSE ###
      mou <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mouse"]
      if (length(mou) == 0) {mou <- NA}
      mouse <- c(mouse,mou)
      ### TREATMENT ###
      treat <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "treatment"]
      if (length(treat) == 0) {treat <- NA}
      treatment <- c(treatment,treat)
      ### SEX ###
      se <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "sex"]
      if (length(se) == 0) {se <- NA}
      sex <- c(sex,se)
      ### MPI ###
      mp <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mpi"]
      if (length(mp) == 0) {mp <- NA}
      mpi <- c(mpi,mp)
      ### GENOTYPE ###
      geno <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "genotype"]
      if (length(geno) == 0) {geno <- NA}
      genotype <- c(genotype,geno)
      ### INCLUDE ###
      inc <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "include"]
      if (length(inc) == 0) {inc <- NA}
      include <- c(include,inc)
      ### BATCH ###
      ba <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "batch"]
      if (length(ba) == 0) {ba <- NA}
      batch <- c(batch,ba)
    }
    vals$fullData$marker <- marker
    vals$fullData$mouse <- mouse
    vals$fullData$sex <- sex
    vals$fullData$treatment <- treatment
    vals$fullData$mpi <- mpi
    vals$fullData$genotype <- genotype
    vals$fullData$include <- include
    vals$fullData$batch <- batch
    
    vals$fullData <- vals$fullData[vals$fullData$include == "Y",]
    
    check2 <<- vals$fullData
    
    print("Done Merging Annotation and Check 2")
    
    forAnno <<- vals$fullData
    
    #add options to x,y,displayX, displayY
    options <- colnames(vals$fullData)
    updateSelectizeInput(session, 'x', "Select your x axis variables to create heatmap", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'y', "Select your y axis variables to create heatmap", choices = options,options = list(create = F))
    # updateSelectizeInput(session, 'displayX', "Select your variables to display on x axis", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'displayY', "Select your variables to display on y axis", choices = options,options = list(create = F))
    
    print("Done Adding Options")
    
    fullData <<- vals$fullData
    
    output$saveOutput2 <- downloadHandler(
      filename = function() {
        paste0("checkpoint2", ".csv")
      },
      content = function(file) {
        #saveRDS(fullData, file = file)
        write.csv(fullData, file = file, row.names=FALSE)
        print("done saving checkpoint 2")
      }
    )
    
    output$print <- renderText({
      print(paste0(Sys.time()," Done Adding Options, Merging Annotation, and Checkpoint 2"))
    })
  })
  
  observeEvent(input$reLoad2,{
    file <- input$reLoad2
    fullData <- readRDS(file$datapath)
    if (!"batch" %in% colnames(fullData)){
      fullData$batch <- 1
    }
    vals$fullData <- fullData
    
    #Fix ECT
    fullData <<- vals$fullData 
    theCol <- grepl("name",colnames(fullData),ignore.case = T) & grepl("region",colnames(fullData),ignore.case = T)
    theCol <- which(theCol)
    theRows <- grep("ecto",fullData[,theCol],ignore.case = T)
    fullData[theRows,"parent"] <- "ECT"
    vals$fullData <<- fullData
    
    checking1 <<- vals$fullData
    tree <<- readRDS("RequiredFiles/flippedTree.rds")
    
    options <- colnames(vals$fullData)
    updateSelectizeInput(session, 'x', "Select your x axis variables to create heatmap", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'y', "Select your y axis variables to create heatmap", choices = options,options = list(create = F))
    # updateSelectizeInput(session, 'displayX', "Select your variables to display on x axis", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'displayY', "Select your variables to display on y axis", choices = options,options = list(create = F))
    
    output$print <- renderText({
      print(paste0(Sys.time()," Done Loading Checkpoint 2"))
    })
  })
  
  observeEvent(input$reLoad2csv,{
    file <- input$reLoad2csv
    fullData <- read.csv(file$datapath)
    if (!"batch" %in% colnames(fullData)){
      fullData$batch <- 1
    }
    vals$fullData <- fullData
    
    #Fix ECT
    fullData <<- vals$fullData 
    theCol <- grepl("name",colnames(fullData),ignore.case = T) & grepl("region",colnames(fullData),ignore.case = T)
    theCol <- which(theCol)
    theRows <- grep("ectorh",fullData[,theCol],ignore.case = T)
    fullData[theRows,"parent"] <- "ECT"
    vals$fullData <<- fullData
    
    checking1 <<- vals$fullData
    tree <<- readRDS("RequiredFiles/flippedTree.rds")
    
    options <- colnames(vals$fullData)
    updateSelectizeInput(session, 'x', "Select your x axis variables to create heatmap", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'y', "Select your y axis variables to create heatmap", choices = options,options = list(create = F))
    # updateSelectizeInput(session, 'displayX', "Select your variables to display on x axis", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'displayY', "Select your variables to display on y axis", choices = options,options = list(create = F))
    
    output$print <- renderText({
      print(paste0(Sys.time()," Done Loading Checkpoint 2"))
    })
  })
  
  ##### GET OPTIONS #####
  observeEvent(input$options,{
    x <<- input$x # recommended x : "mouse" and "marker" 
    y <<- input$y # recommended y : "daughter" (or "parent") and "side"
    displayX <<- input$x
    displayY <<- input$displayY
    
    options1 <- unique(unlist(vals$fullData[,c(x)]))
    print(options1)
    updateSelectizeInput(session,"optionsX","Select your variables to see segmented on x axis", choices = options1,options = list(create = F), selected = options1)
    options1 <- unique(unlist(vals$fullData[,c(x,y)]))
    updateSelectizeInput(session,"annoVariable","Select your variable to display on anatomical heatmap", choices = options1,options = list(create = F))
    
    options2 <- colnames(vals$fullData)
    options2 <- options2[!grepl("load",options2,ignore.case = T)]
    
    if (any(grepl("inclusion",options2,ignore.case = T))){
      options2 <- c("ParentNeuriteLoad","DaughterNeuriteLoad",options2)
    }
    
    #choose variables to sum with
    updateSelectizeInput(session,"sumWith","Select the nutil variable to create value with",choices=c("ParentLoad","DaughterLoad","ParentInclPerMM2","DaughterInclPerMM2",options2),options = list(create = FALSE))
    
    combinedvars <<- c(x,y)
    #choose variables to group by
    updateSelectizeInput(session,"groupBy","Select the variables to group by",choices=combinedvars,options = list(create = FALSE))
    
    #for col and row names display
    
    options3 <-c("None",displayX,"X")
    options4 <-c("None",displayY,"Y")
    updateSelectizeInput(session,"xNames","Select the display variable to use as row names", choices = options3,options = list(create = F))
    updateSelectizeInput(session,"yNames","Select the display variable to use as col names", choices = options4,options = list(create = F))
    
    updateSelectizeInput(session, 'toRemove', "Select any regions to remove", choices = tree$X1,options = list(create = F), selected = c("VS"))
    
    output$print <- renderText({
      print(paste0(Sys.time()," Done Updating More Options"))
    })
  })
  
  ##### CREATE HEATMAP #####
  observeEvent(input$normal,{
    print("heatmap started")
    chosenOptions <<- input$optionsX
    nutilVariable <<- input$sumWith
    groupBy <<- input$groupBy
    xNames <<- input$xNames
    yNames <<- input$yNames
    fullData <<- vals$fullData
    updateSliderInput(session,"colorScale", "Color Scale:", min = -100, max = 100, value = c(input$min_value,input$max_value),step = 0.5)
    # fullData <- check2
    vals$fullData$mouse <- as.character(vals$fullData$mouse)
    
    variableCheckPoint <<- vals$fullData
    ToMessWith <<- vals$fullData
    variableCheckPoint <- ToMessWith
    print(colnames(variableCheckPoint))
    
    colnames(variableCheckPoint) <- gsub("\\."," ",colnames(variableCheckPoint))
    print("colnames updated")
    print(colnames(variableCheckPoint))
    
    if (grepl("Parent",nutilVariable)){
      variableCheckPoint$level <- variableCheckPoint$parent
      vals$fullData$level <- vals$fullData$parent
      vals$fullData <<- vals$fullData
      variableCheckPoint <<- variableCheckPoint
    } else if (grepl("Daughter",nutilVariable)){
      variableCheckPoint$level <- variableCheckPoint$daughter
      variableCheckPoint <<- variableCheckPoint
      vals$fullData$level <- vals$fullData$daughter
      vals$fullData <<- vals$fullData
    }
    
    print("level designated")
    
    if (grepl("Load",nutilVariable) & !grepl("Neurite",nutilVariable)) {
      print("Normal Load")
      test <- variableCheckPoint %>% group_by(mouse,side,level) %>% 
        summarise(`Object area` = mean(`Object area`), `Region area` = mean(`Region area`))
      
      test$baseLoad <- test$`Object area`/test$`Region area`
      #remove object.area and region.area cols
      test <- test[,!colnames(test) %in% c("Object area","Region area")]
      
      #merge test with chekckpoint2 by mouse, side, and level
      variableCheckPoint <- merge(variableCheckPoint, test, by = c("mouse", "side", "level"))
      variableCheckPoint$nutilVariable <- variableCheckPoint$baseLoad
      variableCheckPoint <<- variableCheckPoint
      vals$fullData <- merge(vals$fullData,variableCheckPoint[,c("mouse","side","level","nutilVariable")], by = c("mouse", "side", "level"))
      vals$fullData <<- vals$fullData
      print("NormalLoad Done")
      
    } else if (grepl("Neurite",nutilVariable)) {
      print("NeuriteLoad")
      variableCheckPoint$`Inclusion area` <- variableCheckPoint[,grepl("inclusion",colnames(variableCheckPoint),ignore.case = T)]
      test <- variableCheckPoint %>% group_by(mouse,side,level) %>% 
        summarise(`Object area` = mean(`Object area`), `Inclusion area` = mean(`Inclusion area`),
                  `Region area` = mean(`Region area`))
      test$ParentNeurite <- test$`Object area` - test$`Inclusion area`
      
      test$NeuriteLoad <- test$ParentNeurite/test$`Region area`
      #remove object.area and region.area cols
      test <- test[,!colnames(test) %in% c("Object area","Region area",
                                           "Inclusion area")]
      
      #merge test with chekckpoint2 by mouse, side, and level
      variableCheckPoint <- merge(variableCheckPoint, test, by = c("mouse", "side", "level"))
      variableCheckPoint$nutilVariable <- variableCheckPoint$NeuriteLoad
      variableCheckPoint <<- variableCheckPoint
      vals$fullData <- merge(vals$fullData,variableCheckPoint[,c("mouse","side","level","nutilVariable")], by = c("mouse", "side", "level"))
      vals$fullData <<- vals$fullData
      print("NeuriteLoad Done")
      
    } else if (grepl("InclPerMM2",nutilVariable)) {
      ("InclPerMM2")
      variableCheckPoint$`Region area` <- variableCheckPoint$`Region area` * .000001
      test <- variableCheckPoint %>% group_by(mouse,side,level) %>% 
        summarise(`Object count` = mean(`Object count`), `Region area` = mean(`Region area`))
      
      test$InclPerMM2 <- test$`Object count`/test$`Region area`
      #remove object.area and region.area cols
      test <- test[,!colnames(test) %in% c("Object count","Region area")]
      
      #merge test with chekckpoint2 by mouse, side, and level
      variableCheckPoint <- merge(variableCheckPoint, test, by = c("mouse", "side", "level"))
      variableCheckPoint$nutilVariable <- variableCheckPoint$InclPerMM2
      variableCheckPoint <<- variableCheckPoint
      vals$fullData <- merge(vals$fullData,variableCheckPoint[,c("mouse","side","level","nutilVariable")], by = c("mouse", "side", "level"))
      vals$fullData <<- vals$fullData
      print("InclPerMM2 Done")
      
    } else {
      vals$fullData$nutilVariable <- vals$fullData[,nutilVariable]
      vals$fullData <<- vals$fullData
    }
    vals$fullData$nutilVariable <- as.numeric(vals$fullData$nutilVariable)
    print("Done setting everything for nutil Variable")
    # vals$fullData <- vals$fullData[,!colnames(vals$fullData) %in% c("Inclusion.area")]
    
    ###### COLOR SECTION ######
    dcolor <- c()
    pcolor <- c()
    scolor <- c()
    
    toMerge <- tree[,c(1,11)]
    #left join
    
    vals$fullData$daughterColor <- toMerge[match(vals$fullData$daughter, toMerge$X1),2]
    vals$fullData$parentColor <- toMerge[match(vals$fullData$parent, toMerge$X1),2]
    vals$fullData$specialsColor <- toMerge[match(vals$fullData$specials, toMerge$X1),2]
    
    checkcolo <<-  vals$fullData
    print("here")
    
    dauColors <- setNames(as.character(vals$fullData$daughterColor), vals$fullData$daughter)
    dauColors <- dauColors[unique(names(dauColors))]
    testing <- match(names(dauColors),tree$X1)
    testing1 <- c(1:length(dauColors))
    test <- data.frame(testing,testing1)
    test <- test[order(test$testing),]
    dauColors <<- dauColors[test$testing1]
    
    parColors <- setNames(as.character(vals$fullData$parentColor), vals$fullData$parent)
    parColors <- parColors[unique(names(parColors))]
    testing <- match(names(parColors),tree$X1)
    testing1 <- c(1:length(parColors))
    test <- data.frame(testing,testing1)
    test <- test[order(test$testing),]
    parColors <<- parColors[test$testing1]
    
    print("midway")
    
    specColors <- setNames(as.character(vals$fullData$specialsColor), vals$fullData$specials)
    specColors <- specColors[unique(names(specColors))]
    testing <- match(names(specColors),tree$X1)
    testing1 <- c(1:length(specColors))
    test <- data.frame(testing,testing1)
    test <- test[order(test$testing),]
    specColors <<- specColors[test$testing1]
    
    print("colors done")
    
    if (length(x) > 1) {
      vals$fullData$x <- apply(vals$fullData[ ,x] , 1 , paste , collapse = "_" )
    }
    else {
      vals$fullData$x <-vals$fullData[ ,x]
    }
    
    if (length(y) > 1) {
      vals$fullData$y <- apply(vals$fullData[ ,y] , 1 , paste , collapse = "_" )
    }
    else {
      vals$fullData$y <-vals$fullData[ ,y]
    }
    print("created new cols")
   
    #Remove these regions
    # theSearch <<- input$toRemove
    # remove <- c()
    # i <- 1
    # for (i in 1:length(vals$fullData$`Region ID`)){
    #   id <- vals$fullData$`Region ID`[i]
    #   row <- tree[tree$X10 == id,-10]
    #   searched <- theSearch %in% row
    #   if (sum(searched)>0) {
    #     remove <- c(remove,"remove")
    #   }
    #   else {
    #     remove <- c(remove,"keep")
    #   }
    # }
    # vals$fullData <- vals$fullData[remove == "keep",]
    colnames(vals$fullData) <- gsub("\\."," ",colnames(vals$fullData))
    vals$fullData$forRemove <- vals$fullData$`Region ID`
    ZBasics <<- c("mouse","Region ID","mpi","side","parent","daughter","genotype",'treatment','sex','marker','forRemove','batch')
    fullData <<- vals$fullData
    ##calculate value and add to table
    if (input$percent == F){ # & input$toGroup == F){
      print("testing1")
      colnames(vals$fullData) <- gsub("\\."," ",colnames(vals$fullData))
      fullData <<- vals$fullData
      table <- aggregate(fullData[,c(displayX,displayY,"nutilVariable",ZBasics)], by =fullData[,c("x","y")], FUN=unique)
      print("testing1.5")
      table$value <- table[,"nutilVariable"]
      print("testing2")
      sum <- c()
      for (i in 1:length(table$value)){
        sum <- c(sum,mean(table$value[[i]]))
      }
      table$value1 <- sum
      table$value <- table$value1
      table$ZachValue <- table$value
      print("testing3")
      if (input$log == T) {
        table$value <- table$value + 0.00001
        table$value <- log10(table$value)
      }
      print("testing4")
      table$displayValue <- table$value
      newTable <- table
      newTable <- newTable[,! colnames(newTable) %in% c("nutilVariable")]
      print("testing5")
    }
    else if (input$percent == TRUE) {
      print("if percent")
      colnames(vals$fullData) <- gsub("\\."," ",colnames(vals$fullData))
      table <- aggregate(vals$fullData[,c(displayX,displayY,"nutilVariable",ZBasics)], by =vals$fullData[,c("x","y")], FUN=unique)
      
      table$value <- table[,"nutilVariable"]
      
      sum <- c()
      for (i in 1:length(table$value)){
        sum <- c(sum,mean(table$value[[i]]))
      }
      table$value1 <- sum
      
      table$value <- table$value1*100
      table$ZachValue <- table$value
      
      if (input$log == T) {
        table$value <- table$value + 0.00001
        table$value <- log10(table$value)
        # table[is.na(table$value),17] <- 0
      }
      table$displayValue <- table$value
      newTable <- table
      newTable <- newTable[,! colnames(newTable) %in% c("nutilVariable")]
      #Logit scale?
      #Log scale maybe
    }
    # else if (input$toGroup == T){
    #   print("to Group")
    #   colnames(fullData) <- gsub("\\."," ",colnames(fullData))
    #   nutilVariable <<- gsub("\\."," ",nutilVariable)
    #   ZBasics <<- gsub("\\."," ",ZBasics)
    #   
    #   print("1")
    #   #aggregate by x and y first
    #   theTablePt1 <- aggregate(fullData[,c(displayX,displayY,groupBy,ZBasics)], by = fullData[,c("x","y")], FUN=unique)
    #   theTablePt2 <- aggregate(fullData[,c(nutilVariable)], by = fullData[,c("x","y")], FUN=as.vector)
    #   colnames(theTablePt2)[3] <- nutilVariable
    #   theTable <<- cbind(theTablePt1,theTablePt2)
    #   
    #   theTable$nutilVariable <- theTable[,nutilVariable]
    #   theTable <- theTable[, !duplicated(colnames(theTable))]
    #   theTable <- theTable %>% select(-contains('.1'))
    #   theTable <- theTable %>% select(-contains('.2'))
    #   
    #   print("2")
    #   #sum each row
    #   theTable1 <<- theTable %>% rowwise() %>% 
    #     mutate(Sum = sum(as.numeric(nutilVariable),na.rm = T))
    #   
    #   print("3")
    #   #add up sums to get totals
    #   # totalsTable <- aggregate(theTable1[,c(groupBy,"Sum")], by = theTable1[,c(groupBy)], FUN=unique)
    #   totalsTablePt1 <- aggregate(theTable1[,c(groupBy)], by = theTable1[,c(groupBy)], FUN=unique)
    #   totalsTablePt2 <- aggregate(theTable1[,c("Sum")], by = theTable1[,c(groupBy)], FUN=as.vector)
    #   
    #   totalsTable <<- cbind(totalsTablePt1,totalsTablePt2)
    #   
    #   totalsTable <- totalsTable[, !duplicated(colnames(totalsTable))]
    #   totalsTable <- totalsTable %>% rowwise() %>% 
    #     mutate(totalSum = sum(as.numeric(Sum),na.rm = T))
    #   totalsTable1 <<- totalsTable[,!colnames(totalsTable) %in% c("Sum")]
    #   print("4")
    #   #combine tables
    #   
    #   newTable <<- theTable1 %>% left_join(totalsTable1,by=groupBy)
    #   newTable <- newTable[, !duplicated(colnames(newTable))]
    #   print("5")
    #   #do math to get percents
    #   
    #   newTable$value <- newTable$Sum/newTable$totalSum*100
    #   newTable$ZachValue <- newTable$value
    #   print("6")
    #   if (input$log == T) {
    #     newTable$value <- newTable$value + 0.00001
    #     newTable$value <- log10(newTable$value)
    #     # table[is.na(table$value),17] <- 0
    #   }
    #   newTable$displayValue <- newTable$value
    #   
    # }
    else {
      table <- aggregate(vals$fullData[,c(displayX,displayY,"nutilVariable",ZBasics)], by =vals$fullData[,c("x","y")], FUN=unique)
      
      table$value <- table[,"nutilVariable"]
      
      sum <- c()
      for (i in 1:length(table$value)){
        sum <- c(sum,mean(table$value[[i]]))
      }
      table$value1 <- sum
      table$value <- table$value1
      table$ZachValue <- table$value
      
      if (input$log == T) {
        table$value <- table$value + 0.00001
        table$value <- log10(table$value)
      }
      table$displayValue <- table$value
      newTable <- table
      newTable <- newTable[,! colnames(newTable) %in% c("nutilVariable")]
    }
    valueTable <<- newTable
    print("done making table")
    
    #Remove these regions
    theSearch <<- input$toRemove
    remove <- c()
    i <- 1
    for (i in 1:length(valueTable$forRemove)){
      id <- valueTable$forRemove[i]
      row <- tree[tree$X10 == id,-10]
      searched <- theSearch %in% row
      if (sum(searched)>0) {
        remove <- c(remove,"remove")
      }
      else {
        remove <- c(remove,"keep")
      }
    }
    valueTable <- valueTable[remove == "keep",]
    
    check3 <<- valueTable
    
    print("Done getting values for table and Check 3")
    
    #ZACHS Portion
    ZTable <- valueTable[,c(ZBasics,"ZachValue","displayValue")]
    theMax <<- input$max_value
    theMin <<- input$min_value
    toChangeMax <- ZTable$displayValue > theMax
    toChangeMin <- ZTable$displayValue < theMin
    toChangeMax[is.na(toChangeMax)] <- FALSE
    toChangeMin[is.na(toChangeMin)] <- FALSE
    ZTable[toChangeMax,"displayValue"] <- theMax
    ZTable[toChangeMin,"displayValue"] <- theMin
    
    ZTable <- ZTable %>% mutate_all(as.character)
    
    ZTable[is.na(ZTable)] <- "None"
    ZTable$max <- theMax
    ZTable$min <- theMin
    #add column to ZTable for min_value
    ZTable <<- apply(ZTable,2,as.character)
    ZTable <<- as.data.frame(ZTable)
    print(class(ZTable$mouse))
    
    output$mbh <- downloadHandler(
      filename = function() {
        paste0("MBHTable", ".csv")
      },
      content = function(file) {
        ZTable <- apply(ZTable,2,as.character)
        write.csv(ZTable, file)
        print("Done Downloading the MBH Table")
      }
    )
    print("done thingy")
    
    # library(tidyverse)
    # 
    uniqueX <- unique(valueTable$x)
    uniqueY <- unique(valueTable$y)

    forData <- valueTable[,c("x","y","value")]
    attempt <- forData %>%
      pivot_wider(names_from=c(y), values_from=value)
    attempt <- data.frame(attempt)
    rownames(attempt) <- attempt$x
    data <- attempt[,-1]
    #organize attempt rows by rownames
    data <- data[order(rownames(data)), ]
    print("done Creating Data")
    
    #create y annotation using displayY
    annoY <- c()
    for (i in uniqueY) {
      row <- valueTable[valueTable$y == i,c(displayY)]
      if (length(displayY) == 1) {
        annoY <- rbind(annoY,row[1])
      }
      else {
        annoY <- rbind(annoY,row[1,])
      }
    }
    annoY <- data.frame(annoY)
    colnames(annoY) <- displayY
    rownames(annoY) <- uniqueY
    annoY <<- annoY
    
    annoX <- c()
    for (i in uniqueX) {
      row <- valueTable[valueTable$x == i,c(displayX)]
      if (length(displayX) == 1) {
        annoX <- rbind(annoX,row[1])
      }
      else {
        annoX <- rbind(annoX,row[1,])
      }
    }
    annoX <- data.frame(annoX)
    colnames(annoX) <- displayX
    rownames(annoX) <- uniqueX
    annoX <<- annoX
    
    check4 <<- data
    data <- check4
    
    if (input$trim == TRUE) {
      moreThan <- 0.5 #if more than this percent is missing, remove
      #remove data if in less than half the brains
      if (length(data[1,]) < length(data[,1])) {
        testing <- rowSums(is.na(data))
        rowLength <- length(data[1,])
        rowThings <- names(testing)
        remove <- c()
        for (item in rowThings){
          value <- testing[item]
          if (value > rowLength*moreThan) {
            remove <- c(remove,item)
          }
        }
        data <- data[!rownames(data) %in% remove,]
        saved <- colnames(annoX)
        annoX <- annoX[!rownames(annoX) %in% remove,]
        annoX <- data.frame(annoX)
        colnames(annoX) <- saved
        rownames(annoX) <- uniqueX[!uniqueX %in% remove]
        
        
        testing <- colSums(is.na(data))
        colLength <- length(data[,1])
        colThings <- names(testing)
        remove <- c()
        for (item in colThings){
          value <- testing[item]
          if (value > colLength*moreThan) {
            remove <- c(remove,item)
          }
        }
        data <- data[,!colnames(data) %in% remove]
        saved <- colnames(annoY)
        annoY <- annoY[!rownames(annoY) %in% remove,]
        annoY <- data.frame(annoY)
        colnames(annoY) <- saved
        rownames(annoY) <- uniqueY[!uniqueY %in% remove]
      }
      else {
        testing <- colSums(is.na(data))
        colLength <- length(data[,1])
        colThings <- names(testing)
        remove <- c()
        for (item in colThings){
          value <- testing[item]
          if (value > colLength*moreThan) {
            remove <- c(remove,T)
          }
          else {
            remove <- c(remove, F)
          }
        }
        data <- data[,remove == F, drop = F]
        annoY <- annoY[remove == F,,drop = F]
        
        testing <- rowSums(is.na(data))
        rowLength <- length(data[1,])
        rowThings <- names(testing)
        remove <- c()
        for (item in rowThings){
          value <- testing[item]
          if (value > rowLength*moreThan) {
            remove <- c(remove,T)
          }
          else {
            remove <- c(remove, F)
          }
        }
        data <- data[remove == F,,drop = F]
        annoX <- annoX[remove ==F,,drop = F]

      }
    }
    print("done creating annotation")
    
    #Create save files to mess with
    write.csv(annoY,"annoY.csv")
    write.csv(data,"data.csv")
    write.csv(annoX,"annoX.csv")
    annoY1 <<- annoY
    annoX1 <<- annoX
    dataTest <<- data
    data1 <<- data
    
    output$data <- downloadHandler(
      filename = function() {
        paste0("data", ".csv")
      },
      content = function(file) {
        write.csv(data, file)
        print("Done Downloading Data")
      }
    )
    
    output$annoX <- downloadHandler(
      filename = function() {
        paste0("annoX", ".csv")
      },
      content = function(file) {
        write.csv(annoX, file)
        print("Done Downloading Anno X")
      }
    )
    
    output$annoY <- downloadHandler(
      filename = function() {
        paste0("annoY", ".csv")
      },
      content = function(file) {
        write.csv(annoY, file)
        print("Done Downloading Anno Y")
      }
    )
    
    print("done creating downloads")
    
    annoX <- annoX1
    correct <- c()
    for (i in 1:length(annoX[,1])) {
      good = F
      for (j in 1:length(annoX[1,])) {
        for (item in chosenOptions) {
          if (item == annoX[i,j]){
            good = T
          }
        }
      }
      correct <- c(correct,good)
    }
    annoX <- annoX[correct == T,,drop = F]
    data1 <- data1[correct == T,,drop = F]
    
    print("done segmenting")
    
    #Create the variables for the color palette
    breaks <- 20
    newMin <<- input$min_value
    newMax <<- input$max_value
    difference <- newMin-newMax
    breakNum <- abs(difference/(breaks+1))
    seqBreaks <<- seq(newMin, newMax, by=breakNum)
    data1[data1 > newMax] <- newMax
    data1[data1 < newMin] <- newMin
    
    print("colors")
    if (exists("dauColors")& exists("parColors") & exists("specColors")) {
      if ("daughter" %in% colnames(annoX)) {
        if ("parent" %in% colnames(annoX)){
          if ("specials" %in% colnames(annoX)) {
            parColors <- parColors[unique(annoX$parent)]
            specColors <- specColors[unique(annoX$specials)]
            dauColors <- dauColors[unique(annoX$daughter)]
            
            xAnnoColors <- list(parent = parColors,specials = specColors,daughter = dauColors)
          }
          else {
            parColors <- parColors[unique(annoX$parent)]
            dauColors <- dauColors[unique(annoX$daughter)]
            
            xAnnoColors <- list(parent = parColors, daughter = dauColors)
          }
        }
        else {
          dauColors <- dauColors[unique(annoX$daughter)]
          
          xAnnoColors <- list(daughter = dauColors)
        }
      }
      else if ("parent" %in% colnames(annoX)) {
        if ("specials" %in% colnames(annoX)) {
          parColors <- parColors[unique(annoX$parent)]
          specColors <- specColors[unique(annoX$specials)]
          
          xAnnoColors <- list(parent = parColors,specials = specColors)
        }
        else {
          parColors <- parColors[unique(annoX$parent)]
          
          xAnnoColors <- list(parent = parColors)
        }
      }
      else if ("specials" %in% colnames(annoX)){
        specColors <- specColors[unique(annoX$specials)]
        
        xAnnoColors <- list(specials = specColors)
      }
      else {
        xAnnoColors <- list()
      }
      
      #Colors for AnnoY!
      if ("daughter" %in% colnames(annoY)) {
        if ("parent" %in% colnames(annoY)){
          if ("specials" %in% colnames(annoY)) {
            parColors <- parColors[unique(annoY$parent)]
            specColors <- specColors[unique(annoY$specials)]
            dauColors <- dauColors[unique(annoY$daughter)]
            
            yAnnoColors <- list(parent = parColors,specials = specColors, daughter = dauColors)
          }
          else {
            parColors <- parColors[unique(annoY$parent)]
            dauColors <- dauColors[unique(annoY$daughter)]
            
            yAnnoColors <- list(parent = parColors, daughter = dauColors)
          }
        }
        else {
          dauColors <- dauColors[unique(annoY$daughter)] 
          
          yAnnoColors <- list(daughter = dauColors)
        }
      }
      else if ("parent" %in% colnames(annoY)) {
        if ("specials" %in% colnames(annoY)) {
          parColors <- parColors[unique(annoY$parent)]
          specColors <- specColors[unique(annoY$specials)]
          
          yAnnoColors <- list(parent = parColors,specials = specColors)
        }
        else {
          parColors <- parColors[unique(annoY$parent)]
          
          yAnnoColors <- list(parent = parColors)
        }
      }
      else if ("specials" %in% colnames(annoY)){
        specColors <- specColors[unique(annoY$specials)]
        
        yAnnoColors <- list(specials = specColors)
      }
      else {
        yAnnoColors <- list()
      }
    }
    else if (exists("parColors")){
      if ("daughter" %in% colnames(annoX)) {
        if ("parent" %in% colnames(annoX)){
          if ("specials" %in% colnames(annoX)) {
            parColors <- parColors[unique(annoX$parent)]
            
            xAnnoColors <- list(parent = parColors)
          }
          else {
            parColors <- parColors[unique(annoX$parent)]
            
            xAnnoColors <- list(parent = parColors)
          }
        }
        else {
          xAnnoColors <- list()
        }
      }
      else if ("parent" %in% colnames(annoX)) {
        if ("specials" %in% colnames(annoX)) {
          parColors <- parColors[unique(annoX$parent)]
          
          xAnnoColors <- list(parent = parColors)
        }
        else {
          parColors <- parColors[unique(annoX$parent)]
          
          xAnnoColors <- list(parent = parColors)
        }
      }
      else if ("specials" %in% colnames(annoX)){
        xAnnoColors <- list()
      }
      else {
        xAnnoColors <- list()
      }
      
      #Colors for AnnoY!
      if ("daughter" %in% colnames(annoY)) {
        if ("parent" %in% colnames(annoY)){
          if ("specials" %in% colnames(annoY)) {
            parColors <- parColors[unique(annoY$parent)]
            
            yAnnoColors <- list(parent = parColors)
          }
          else {
            parColors <- parColors[unique(annoY$parent)]
            
            yAnnoColors <- list(parent = parColors)
          }
        }
        else {
          yAnnoColors <- list()
        }
      }
      else if ("parent" %in% colnames(annoY)) {
        if ("specials" %in% colnames(annoY)) {
          parColors <- parColors[unique(annoY$parent)]
          
          yAnnoColors <- list(parent = parColors)
        }
        else {
          parColors <- parColors[unique(annoY$parent)]
          
          yAnnoColors <- list(parent = parColors)
        }
      }
      else if ("specials" %in% colnames(annoY)){
        yAnnoColors <- list()
      }
      else {
        yAnnoColors <- list()
      }
    }
    else if (exists("specColors")) {
      if ("daughter" %in% colnames(annoX)) {
        if ("parent" %in% colnames(annoX)){
          if ("specials" %in% colnames(annoX)) {
            specColors <- specColors[unique(annoX$specials)]
            
            xAnnoColors <- list(specials = specColors)
          }
          else {
            xAnnoColors <- list()
          }
        }
        else {
          xAnnoColors <- list()
        }
      }
      else if ("parent" %in% colnames(annoX)) {
        if ("specials" %in% colnames(annoX)) {
          specColors <- specColors[unique(annoX$specials)]
          
          xAnnoColors <- list(specials = specColors)
        }
        else {
          xAnnoColors <- list()
        }
      }
      else if ("specials" %in% colnames(annoX)){
        specColors <- specColors[unique(annoX$specials)]
        
        xAnnoColors <- list(specials = specColors)
      }
      else {
        xAnnoColors <- list()
      }
      
      #Colors for AnnoY!
      if ("daughter" %in% colnames(annoY)) {
        if ("parent" %in% colnames(annoY)){
          if ("specials" %in% colnames(annoY)) {
            specColors <- specColors[unique(annoY$specials)]
            
            yAnnoColors <- list(specials = specColors)
          }
          else {
            xAnnoColors <- list()
          }
        }
        else {
          yAnnoColors <- list()
        }
      }
      else if ("parent" %in% colnames(annoY)) {
        if ("specials" %in% colnames(annoY)) {
          specColors <- specColors[unique(annoY$specials)]
          
          yAnnoColors <- list(specials = specColors)
        }
        else {
          xAnnoColors <- list()
        }
      }
      else if ("specials" %in% colnames(annoY)){
        specColors <- specColors[unique(annoY$specials)]
        
        yAnnoColors <- list(specials = specColors)
      }
      else {
        yAnnoColors <- list()
      }
    }
    else {
      xAnnoColors <- list()
      yAnnoColors <- list()
    }
    
    annoColors <<- append(xAnnoColors,yAnnoColors)
    print(annoColors)
    
    print("start sorting")
    annoYtest <<- annoY
    annoXtest <<- annoX
    data1test <<- data1
    # copy <- annoYtest
    rownames(annoY) <-  gsub("/", ".", rownames(annoY), fixed=TRUE)
    rownames(annoY) <-  gsub("-", ".", rownames(annoY), fixed=TRUE)
    rownames(annoY) <-  gsub(" ", ".", rownames(annoY), fixed=TRUE)
    colnames(data1) <-  gsub("/", ".", colnames(data1), fixed=TRUE)
    colnames(data1) <-  gsub("-", ".", colnames(data1), fixed=TRUE)
    colnames(data1) <-  gsub(" ", ".", colnames(data1), fixed=TRUE)
    # data1 <- data1test
    # annoX <- annoXtest
    # annoY <- annoYtest
    #Sorting by Side then ABA
    if ("side" %in% colnames(annoY) | "side" %in% colnames(annoX)) {
      print("yes side")
      if ("parent" %in% colnames(annoY)) {
        testing <- match(annoY$parent,tree$X1)
        testing1 <- c(1:length(annoY$parent))
        side <- annoY$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("parent" %in% colnames(annoX)) {
        testing <- match(annoX$parent,tree$X1)
        testing1 <- c(1:length(annoX$parent))
        side <- annoX$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
      else if ("specials" %in% colnames(annoY)) {
        testing <- match(annoY$specials,tree$X1)
        testing1 <- c(1:length(annoY$specials))
        side <- annoY$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("specials" %in% colnames(annoX)) {
        testing <- match(annoX$specials,tree$X1)
        testing1 <- c(1:length(annoX$specials))
        side <- annoX$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
      else if ("daughter" %in% colnames(annoY)) {
        testing <- match(annoY$daughter,tree$X1)
        testing1 <- c(1:length(annoY$daughter))
        side <- annoY$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("daughter" %in% colnames(annoX)) {
        testing <- match(annoX$daughter,tree$X1)
        testing1 <- c(1:length(annoX$daughter))
        side <- annoX$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
    }
    else {
      print("no side")
      if ("parent" %in% colnames(annoY)) {
        testing <- match(annoY$parent,tree$X1)
        testing1 <- c(1:length(annoY$parent))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("parent" %in% colnames(annoX)) {
        testing <- match(annoX$parent,tree$X1)
        testing1 <- c(1:length(annoX$parent))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
      else if ("specials" %in% colnames(annoY)) {
        testing <- match(annoY$specials,tree$X1)
        testing1 <- c(1:length(annoY$specials))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("specials" %in% colnames(annoX)) {
        testing <- match(annoX$specials,tree$X1)
        testing1 <- c(1:length(annoX$specials))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
      else if ("daughter" %in% colnames(annoY)) {
        testing <- match(annoY$daughter,tree$X1)
        testing1 <- c(1:length(annoY$daughter))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("daughter" %in% colnames(annoX)) {
        testing <- match(annoX$daughter,tree$X1)
        testing1 <- c(1:length(annoX$daughter))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
    }
    print("done sorting")
    
    dataKeep <<- data1
    annoXKeep <<- annoX
    annoYKeep <<- annoY
    
    #labels_col and labels_row
    print(xNames)
    print(colnames(annoX))
    annoX <- annoXKeep
    annoY <- annoYKeep
    
    #sort annox and y 
    
    #sort annoX and annoY based on the organization of the data
    keyAnnoX <- data.frame(DataRows = (rownames(data1)),sortid = c(1:length(rownames(data1))))
    keyAnnoY <- data.frame(DataRows = (colnames(data1)),sortid = c(1:length(colnames(data1))))
    
    annoX <- data.frame(annoX,DataRows = (rownames(annoX)))
    annoY <- data.frame(annoY,DataRows = (rownames(annoY)))
    
    
    #merge with colname and row name, then sort
    annoX <- merge(annoX,keyAnnoX,by = "DataRows")
    annoY <- merge(annoY,keyAnnoY,by = "DataRows")
    rownames(annoX) <- annoX$DataRows
    rownames(annoY) <- annoY$DataRows
    
    annoX <- annoX[order(annoX$sortid), ]
    annoX <- annoX[,!colnames(annoX) %in% c("sortid","DataRows"), drop = FALSE]
    annoY <- annoY[order(annoY$sortid), ]
    annoY <- annoY[,!colnames(annoY) %in% c("sortid","DataRows"), drop = FALSE]

    
    
    if (xNames %in% colnames(annoX)) {
      rowName <- unlist(annoX[,xNames])
    } else if (xNames == "None") {
      rowName <- vector( "character" , length(unlist(rownames(annoX))))
    } else {
      rowName <- unlist(rownames(annoX))
    }
    
    if (yNames %in% colnames(annoY)) {
      colName <- unlist(annoY[,yNames])
    } else if (yNames == "None") {
      colName <- vector( "character" , length(unlist(rownames(annoY))))
    } else {
      colName <- unlist(rownames(annoY))
    }
    
    
    print("heatmap colors")
    breaks <<- breaks
    #default colors
    if (input$inverted == T){
      heatmapColors <- viridis::inferno(breaks,direction = 1)
    } else {
      heatmapColors <- viridis::inferno(breaks,direction = -1)
    }
    #if two or more custom colors are selected, default colors are used
    if (input$color2 == T & input$color3 == T | input$viridis == T & input$color3 == T | input$viridis == T & input$color2 == T) {
      output$minMax <- renderText({
        print("Select only one of the custom color options please. The heatmap output will be in the default colors for now.")
      })
    }
    else if (input$viridis == T) {
      choice <- input$viridisScale
      if (input$inverted == T){
        heatmapColors <- viridis::viridis(breaks,direction = 1,option = choice)
      } else {
        heatmapColors <- viridis::viridis(breaks,direction = -1,option = choice)
      }
      
      output$minMax <- renderText({
        print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
      })
    }
    else if (input$color2 == T) {
      range <- colorRampPalette(c(input$col1,input$col2))
      heatmapColors <- range(length(heatmapColors)) 
      
      output$minMax <- renderText({
        print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
      })
    }
    else if (input$color3 == T) {
      range <- colorRampPalette(c(input$colo1,input$colo2,input$colo3))
      heatmapColors <- range(length(heatmapColors))   
      
      output$minMax <- renderText({
        print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
      })
    }

    heatmapColorsForZach <<- heatmapColors
    #update colors download
    output$colors <- downloadHandler(
      filename = function() {
        paste("heatmapColors.rds")
      },
      content = function(file) {
        saveRDS(heatmapColorsForZach, file)
      }
    )
    
    laAnnoX <<- annoX
    laAnnoY <<- annoY
    annoX <- annoX %>%
      mutate(across(everything(), as.character))
    annoY <- annoY %>%
      mutate(across(everything(), as.character))
    
    print("start creating thePalette")
    thePalette <<- readRDS("RequiredFiles/colorPalette.rds") #made using distinctthePalette
    NamesCols <- c()
    uniqueVals <- list()
    matchingColors <- list()
    location <- 1
    for (i in 1:length(annoX[1,])) {
      NamesCols <- c(NamesCols,colnames(annoX)[i])
      
      len <- length(uniqueVals)
      uniques <- unique(annoX[,i])
      uniqueVals[[len+1]] <- uniques
      
      colors <- c()
      for (item in uniques) {
        colo <- thePalette[location]
        colors <- c(colors,colo)
        location <- location + 1
      }
      
      len <- length(matchingColors)
      matchingColors[[len+1]] <- colors
    }
    
    for (i in 1:length(annoY[1,])) {
      NamesCols <- c(NamesCols,colnames(annoY)[i])
      
      len <- length(uniqueVals)
      uniques <- unique(annoY[,i])
      uniqueVals[[len+1]] <- uniques
      
      colors <- c()
      for (item in uniques) {
        colo <- thePalette[location]
        colors <- c(colors,colo)
        location <- location + 1
      }
      
      len <- length(matchingColors)
      matchingColors[[len+1]] <- colors
    }
    
    blankList <- list()
    for (i in 1:length(uniqueVals)) {
      namePart <- uniqueVals[[i]]
      valuePart <- matchingColors[[i]]
      len <- length(blankList)
      blankList[[len+1]] <- setNames(as.vector(valuePart),namePart)
    }
    names(blankList) <- NamesCols
    
    fullColors <- blankList
    names(fullColors)
    if ("parent" %in% names(fullColors)) {
      fullColors$parent <- annoColors$parent
    }
    if ("specials" %in% names(fullColors)) {
      fullColors$specials <- annoColors$specials
    }
    if("daughter" %in% names(fullColors)) {
      fullColors$daughter <- annoColors$daughter
    }
    print("finished creating thePalette")
    
    fullColors <<- fullColors
    saveAnnoX <<- annoX
    saveAnnoY <<- annoY
    # annoX <- saveAnnoX
    # annoY <- saveAnnoY
    
    colName <<- colName
    rowName <<- rowName
    heatmapColors <<- heatmapColors
    seqBreaks <<- seqBreaks
    saveData <<- data1
    # data1 <- saveData
    annoXWithout <- annoX[,!colnames(annoX) %in% c("daughter","parent","specials"), drop = FALSE]
    annoYWithout <- annoY[,!colnames(annoY) %in% c("daughter","parent","specials"), drop = FALSE]
    

    set.seed(100)
    
    print("Here")
    
    output$minMax <- renderText({
      print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
    })
    if (length(annoYWithout) != 0 & length(annoXWithout) != 0) {
      heatmap2 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                            labels_col = colName,labels_row = rowName, color = heatmapColors, border_color=NA,
                            annotation_row = annoXWithout,annotation_col = annoYWithout, annotation_colors = fullColors)
    }
    else if (length(annoYWithout) != 0) {
      heatmap2 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                            labels_col = colName,labels_row = rowName, color = heatmapColors, border_color=NA,
                            annotation_col = annoYWithout, annotation_colors = fullColors)
    }
    else if (length(annoXWithout) != 0) {
      heatmap2 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                            labels_col = colName,labels_row = rowName, color = heatmapColors, border_color=NA,
                            annotation_row = annoXWithout, annotation_colors = fullColors)
    }
    else {
      heatmap2 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                            labels_col = colName,labels_row = rowName, color = heatmapColors, border_color=NA,
                            annotation_colors = fullColors)
    }
    
    
    print("Testing")
    
    
    heatmap1 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                        labels_col = colName,labels_row = rowName, color = heatmapColors,border_color=NA,
                        annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)
    
    heatmap3 <- pheatmap(data1,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F, 
                          labels_col = colName,labels_row = rowName, color = heatmapColors,border_color=NA,
                          annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)
    # saveRDS(heatmap1,"heatmapSave.rds")
    # 
    # theHeatmap <- readRDS("heatmapSave.rds")
    print("start creating merge")
    grid.newpage()
    grid.draw(heatmap2)
    grid.force()
    see <- grid.ls()
    testing <- see$gPath
    partial <- testing[!str_detect(testing, "layout::annotation_legend") ]
    check1 <- unlist(strsplit(partial, split='::', fixed=TRUE))
    check2 <- check1[!str_detect(check1, "layout") ]

    for (item in check2) {
      tryCatch(
        expr = {
          grid.remove(item)
        },
        error = function(e){
          message('Caught an error!')
        },
        warning = function(w){
          message('Caught a warning!')
        },
        finally = {
          message('All done, quitting.')
        }
      )
    }
    theLegend <- grid.grab()

    grid.newpage()
    grid.draw(heatmap1)
    grid.force()
    see <- grid.ls()
    partial <- testing[str_detect(testing, "layout::annotation_legend") ]
    check1 <- unlist(strsplit(partial, split='::', fixed=TRUE))
    check2 <- check1[!str_detect(check1, "layout") ]

    for (item in check2) {
      tryCatch(
        expr = {
          grid.remove(item)
        },
        error = function(e){
          message('Caught an error!')
        },
        warning = function(w){
          message('Caught a warning!')
        },
        finally = {
          message('All done, quitting.')
        }
      )
    }
    theGraph <- grid.grab()


    grid.newpage()
    grid.draw(theLegend)
    grid.draw(theGraph)
    FullPlot <<- grid.grab()
    
    saveRDS(FullPlot,"fullPlot.rds")
    
    
    heatmap3 <- pheatmap(data1,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F, 
                         labels_col = colName,labels_row = rowName, color = heatmapColors,border_color=NA,
                         annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)
    grid.newpage()
    grid.draw(heatmap3)
    grid.force()
    toUse <- grid.grab()
    look1 <- grid.get("layout")
    vpLayout <- look1[["vp"]][[2]][["layout"]]
    vpLayout$ncol <- as.integer(5)
    vpLayout$widths <- vpLayout$widths[-c(6)]
    vpLayout$respect.mat <- vpLayout$respect.mat[,-c(6)]
    grid.show.layout(vpLayout)
    grid.newpage()
    pushViewport(viewport(layout = vpLayout, name = "testing" ))
    
    look <- getGrob(toUse, "layout")
    theGrobs <- look[["grobs"]]
    while(length(grep("legend",theGrobs)) >0) {
      theGrobs[[grep("legend",theGrobs)[1]]] <- NULL
    }
    theGrobs[[grep("col_names",theGrobs)]] <- NULL
    theGrobs[[grep("row_names",theGrobs)]] <- NULL
    
    for (i in 1:length(theGrobs)) {
      grid.draw(theGrobs[[i]])
    }
    
    noExtras <- grid.grab()
    
    print("done creating merge")
    
    
    
    print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
    
    output$normPlot <- renderPlot({
      grid.draw(FullPlot)
    })
    
    save_heatmap_pdf <- function(x, filename, width=100, height=50) {
      stopifnot(!missing(x))
      stopifnot(!missing(filename))
      pdf(filename, width=width, height=height)
      grid::grid.newpage()
      grid::grid.draw(x)
      dev.off()
    }
    # save_pheatmap_pdf(heatmap1, "testing.pdf")
    
    
    output$heatmap <- downloadHandler(
      filename = function() {
        paste0("heatmap", ".pdf")
      },
      content = function(file) {
        save_heatmap_pdf(FullPlot, file)
        print("Done Downloading Heatmap")
      }
    )
    
    output$justPlot <- downloadHandler(
      filename = function() {
        paste0("justPlot", ".pdf")
      },
      content = function(file) {
        save_heatmap_pdf(noExtras, file, width = 6.5, height = 3)
        print("Done Downloading Heatmap")
      }
    )
    
    # save(list=c("checkData","checkData1","chosenOptions","nutilVariable","groupBy","xNames","yNames","valueTable","check3","annoY1","annoX1","data1","annoColors","origData","origAnnoY","origAnnoX","dataKeep","annoXKeep","annoYKeep","x","y","displayX","displayY","check4"), file="finished.RData")
    
    output$print <- renderText({
      print(paste0(Sys.time()," Done Heatmap, Check 3, Anno Y, Anno X, and Data"))
    })
    
  })
  
  ##### SECOND TAB #####
  
  observeEvent(input$colorScale_2,{
    updateNumericInput(session,"min_value_2","Min Value", input$colorScale_2[1], min = -100, max = 100,step=0.5)
    updateNumericInput(session,"max_value_2","Max Value", input$colorScale_2[2], min = -100, max = 100,step=0.5)
  })
  
  ##### LOAD IN DATA #####
  observeEvent(input$theInput,{
    print("start Downloading")
    the_file <- input$theInput
    path <- the_file$datapath
    
    #load the file
    fullData <- read.csv(path)
    
    regionCol <- grep("region",colnames(fullData),ignore.case = T)[1]
    colnames(fullData)[regionCol] <- "region"
    sideCol <- grep("side",colnames(fullData),ignore.case = T)[1]
    colnames(fullData)[sideCol] <- "side"
    otherCols <- colnames(fullData)[-c(sideCol,regionCol)]
    
    #Check for the correct cols
    if (length(regionCol) < 1){
      shiny::showNotification("Your 'region' column does not exist. Please fix that.", duration = NULL)
    } else if (length(sideCol) < 1){
      shiny::showNotification("Your 'side' column does not exist. Please fix that.", duration = NULL)
    } else if (length(otherCols) != length(unique(otherCols))){
      shiny::showNotification("Your data columns don't have unique names. Please fix that.", duration = NULL)
    }
    
    otherCols <- which(colnames(fullData) %in% otherCols)
    if (length(otherCols) > 1) {
      #pivot so that each column has its own row other than region and side
      fullData <- fullData %>% pivot_longer(cols=all_of(otherCols),
                                            names_to='name',
                                            values_to='theValue')
    } else {
      fullData$name <- colnames(fullData)[otherCols]
      colnames(fullData)[otherCols] <- "theValue"
    }
    
    fullData <<- fullData
    
    output$print_2 <- renderText({
      print(paste0(Sys.time()," Done loading Input file"))
    })
  })
  
  #Load tree
  observeEvent(input$tree_2,{
    # file <- input$tree
    tree <<- readRDS("RequiredFiles/flippedTreeMouse.rds")#readRDS(file = file$datapath)
    print("Data read")
    
    #give Full data its 'Region Name'
    partTree <- tree[,c(1,10,12)]
    colnames(partTree) <- c("region","id","regionName")
    fullData <- merge(fullData,partTree,by = "region")
    
    #Find daughter, parent, and big picture regions and add to fullData
    depth <- c()
    daughter <- c()
    parent <- c()
    for (i in 1:length(fullData$id)){
      id <- fullData$id[i]
      dep <- str_count(fullData$regionName[i],',')
      depth <- c(depth,dep)
      dau <- tree[tree$X10 == id,1]
      if (length(dau) == 0){dau <- NA}
      daughter <- c(daughter, dau)
      if (dep > 1) {dep <- 1}
      par <- tree[tree$X10 == id,1+dep]
      if (length(par) == 0){par <- NA}
      parent <- c(parent,par)
    }
    fullData$depth <- depth
    fullData$daughter <- daughter
    fullData$parent <- parent
    
    #CHANGE THIS to have it search for particular regions
    search <- c("CTXsp","fiber tracts","HPF","HY","Isocortex","OLF","PAL","STR","TH","MB","P","MY","CB")
    specials <- c()
    for (i in 1:length(fullData$id)){
      id <- fullData$id[i]
      row <- tree[tree$X10 == id,-10]
      searched <- search %in% row
      if (sum(searched)>0) {
        specials <- c(specials,search[searched])
      }
      else {
        specials <- c(specials,NA)
      }
    }
    fullData$specials <- specials
    check1 <<- fullData
    
    fullData <<- fullData
    
    output$saveOutput1_2 <- downloadHandler(
      filename = function() {
        paste0("checkpoint1", ".rds")
      },
      content = function(file) {
        saveRDS(fullData, file)
        print("Done Downloading checkpoint 1")
      }
    )
    
    #for blank annotation
    table = fullData %>% group_by(name,side)  %>%
      summarise()
    table1 <- cbind (table, mouse=NA,sex = NA,treatment=NA,mpi=NA,genotype=NA,marker=NA,batch = NA,include="Y")
    
    output$blankAnnotation_2 <- downloadHandler(
      filename = function() {
        paste0("annotation", ".csv")
      },
      content = function(file) {
        write.csv(table1, file ,row.names = FALSE)
        print("Done Downloading Blank Annotation")
      }
    )
    
    output$print_2 <- renderText({
      print(paste0(Sys.time()," Done Tree, Blank Annotation, and Checkpoint1"))
    })
  })
  
  #Load AfterLoad.RData
  observeEvent(input$reLoad1_2,{
    file <- input$reLoad1_2
    fullData <- readRDS(file$datapath)
    fullData <- fullData
    fullData <<- fullData
    tree <<- readRDS("RequiredFiles/flippedTreeMouse.rds")
    
    output$print_2 <- renderText({
      print(paste0(Sys.time()," Done Loading Checkpoint 1"))
    })
  })
  
  #Load annotation file
  observeEvent(input$annotation_2,{
    #read in annotation file
    file <- input$annotation_2
    annotation <<- read.csv(file$datapath)
    print("Data read")
    
    #Merge it with fullData
    marker <- c()
    mouse <- c()
    treatment <- c()
    mpi <- c()
    genotype <- c()
    include <- c()
    sex <- c()
    batch <- c()
    for (i in 1:length(fullData$name)){
      name <- fullData$name[i]
      side <- fullData$side[i]
      ### MARKER ###
      mark <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "marker"]
      if (length(mark) == 0) {mark <- NA}
      marker <- c(marker,mark)
      ### MOUSE ###
      mou <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mouse"]
      if (length(mou) == 0) {mou <- NA}
      mouse <- c(mouse,mou)
      ### TREATMENT ###
      treat <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "treatment"]
      if (length(treat) == 0) {treat <- NA}
      treatment <- c(treatment,treat)
      ### SEX ###
      se <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "sex"]
      if (length(se) == 0) {se <- NA}
      sex <- c(sex,se)
      ### MPI ###
      mp <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mpi"]
      if (length(mp) == 0) {mp <- NA}
      mpi <- c(mpi,mp)
      ### GENOTYPE ###
      geno <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "genotype"]
      if (length(geno) == 0) {geno <- NA}
      genotype <- c(genotype,geno)
      ### INCLUDE ###
      inc <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "include"]
      if (length(inc) == 0) {inc <- NA}
      include <- c(include,inc)
      ### BATCH ###
      ba <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "batch"]
      if (length(ba) == 0) {ba <- NA}
      batch <- c(batch,ba)
    }
    fullData$marker <- marker
    fullData$mouse <- mouse
    fullData$sex <- sex
    fullData$treatment <- treatment
    fullData$mpi <- mpi
    fullData$genotype <- genotype
    fullData$include <- include
    fullData$batch <- batch
    
    fullData <- fullData[fullData$include == "Y",]
    
    check2 <<- fullData
    
    print("Done Merging Annotation and Check 2")
    
    forAnno <<- fullData
    
    #add options to x,y,displayX, displayY
    options <- colnames(fullData)
    updateSelectizeInput(session, 'x_2', "Select your x axis variables to create heatmap", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'y_2', "Select your y axis variables to create heatmap", choices = options,options = list(create = F))
    # updateSelectizeInput(session, 'displayX', "Select your variables to display on x axis", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'displayY_2', "Select your variables to display on y axis", choices = options,options = list(create = F))
    
    print("Done Adding Options")
    
    fullData <<- fullData
    
    output$saveOutput2_2 <- downloadHandler(
      filename = function() {
        paste0("checkpoint2", ".csv")
      },
      content = function(file) {
        #saveRDS(fullData, file = file)
        write.csv(fullData, file = file, row.names=FALSE)
        print("done saving checkpoint 2")
      }
    )
    
    output$print_2 <- renderText({
      print(paste0(Sys.time()," Done Adding Options, Merging Annotation, and Checkpoint 2"))
    })
  })
  
  observeEvent(input$reLoad2csv_2,{
    file <- input$reLoad2csv_2
    fullData <- read.csv(file$datapath)
    if (!"batch" %in% colnames(fullData)){
      fullData$batch <- 1
    }
    fullData <<- fullData
    
    checking1 <<- fullData
    tree <<- readRDS("RequiredFiles/flippedTreeMouse.rds")
    
    options <- colnames(fullData)
    updateSelectizeInput(session, 'x_2', "Select your x axis variables to create heatmap", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'y_2', "Select your y axis variables to create heatmap", choices = options,options = list(create = F))
    # updateSelectizeInput(session, 'displayX', "Select your variables to display on x axis", choices = options,options = list(create = F))
    updateSelectizeInput(session, 'displayY_2', "Select your variables to display on y axis", choices = options,options = list(create = F))
    
    output$print_2 <- renderText({
      print(paste0(Sys.time()," Done Loading Checkpoint 2"))
    })
  })
  
  ##### GET OPTIONS #####
  observeEvent(input$options_2,{
    x <<- input$x_2 # recommended x : "mouse" and "marker" 
    y <<- input$y_2 # recommended y : "daughter" (or "parent") and "side"
    displayX <<- input$x_2
    displayY <<- input$displayY_2
    
    options1 <- unique(unlist(fullData[,c(x)]))
    print(options1)
    updateSelectizeInput(session,"optionsX_2","Select your variables to see segmented on x axis", choices = options1,options = list(create = F), selected = options1)
    options1 <- unique(unlist(fullData[,c(x,y)]))
    updateSelectizeInput(session,"annoVariable_2","Select your variable to display on anatomical heatmap", choices = options1,options = list(create = F))
    
    options3 <-c("None",displayX,"X")
    options4 <-c("None",displayY,"Y")
    updateSelectizeInput(session,"xNames_2","Select the display variable to use as row names", choices = options3,options = list(create = F))
    updateSelectizeInput(session,"yNames_2","Select the display variable to use as col names", choices = options4,options = list(create = F))
    
    updateSelectizeInput(session, 'toRemove_2', "Select any regions to remove", choices = tree$X1,options = list(create = F), selected = c("VS"))
    
    output$print_2 <- renderText({
      print(paste0(Sys.time()," Done Updating More Options"))
    })
  })
  
  ##### CREATE HEATMAP #####
  observeEvent(input$normal_2,{
    print("heatmap started")
    chosenOptions <<- input$optionsX_2
    nutilVariable <<- "theValue"
    xNames <<- input$xNames_2
    yNames <<- input$yNames_2
    fullData <<- fullData
    updateSliderInput(session,"colorScale_2", "Color Scale:", min = -100, max = 100, value = c(input$min_value_2,input$max_value_2),step = 0.5)
    # fullData <- check2
    fullData$mouse <- as.character(fullData$mouse)
    fullData$nutilVariable <- fullData$theValue
    fullData$nutilVariable <- as.numeric(fullData$nutilVariable)
    
    ###### COLOR SECTION ######
    dcolor <- c()
    pcolor <- c()
    scolor <- c()
    
    toMerge <- tree[,c(1,11)]
    #left join
    
    fullData$daughterColor <- toMerge[match(fullData$daughter, toMerge$X1),2]
    fullData$parentColor <- toMerge[match(fullData$parent, toMerge$X1),2]
    fullData$specialsColor <- toMerge[match(fullData$specials, toMerge$X1),2]
    
    checkcolo <<-  fullData
    print("here")
    
    dauColors <- setNames(as.character(fullData$daughterColor), fullData$daughter)
    dauColors <- dauColors[unique(names(dauColors))]
    testing <- match(names(dauColors),tree$X1)
    testing1 <- c(1:length(dauColors))
    test <- data.frame(testing,testing1)
    test <- test[order(test$testing),]
    dauColors <<- dauColors[test$testing1]
    
    parColors <- setNames(as.character(fullData$parentColor), fullData$parent)
    parColors <- parColors[unique(names(parColors))]
    testing <- match(names(parColors),tree$X1)
    testing1 <- c(1:length(parColors))
    test <- data.frame(testing,testing1)
    test <- test[order(test$testing),]
    parColors <<- parColors[test$testing1]
    
    print("midway")
    
    specColors <- setNames(as.character(fullData$specialsColor), fullData$specials)
    specColors <- specColors[unique(names(specColors))]
    testing <- match(names(specColors),tree$X1)
    testing1 <- c(1:length(specColors))
    test <- data.frame(testing,testing1)
    test <- test[order(test$testing),]
    specColors <<- specColors[test$testing1]
    
    print("colors done")
    
    if (length(x) > 1) {
      fullData$x <- apply(fullData[ ,x] , 1 , paste , collapse = "_" )
    }
    else {
      fullData$x <-fullData[ ,x]
    }
    
    if (length(y) > 1) {
      fullData$y <- apply(fullData[ ,y] , 1 , paste , collapse = "_" )
    }
    else {
      fullData$y <-fullData[ ,y]
    }
    print("created new cols")
    colnames(fullData) <- gsub("\\."," ",colnames(fullData))
    fullData$`Region ID` <- fullData$id
    fullData$forRemove <- fullData$`Region ID`
    ZBasics <<- c("mouse","Region ID","mpi","side","parent","daughter","genotype",'treatment','sex','marker','forRemove','batch')
    fullData <<- fullData
    ##calculate value and add to table
    if (input$percent_2 == F){ # & input$toGroup == F){
      print("testing1")
      colnames(fullData) <- gsub("\\."," ",colnames(fullData))
      fullData <<- fullData
      table <- aggregate(fullData[,c(displayX,displayY,"nutilVariable",ZBasics)], by =fullData[,c("x","y")], FUN=unique)
      print("testing1.5")
      table$value <- table[,"nutilVariable"]
      print("testing2")
      sum <- c()
      for (i in 1:length(table$value)){
        sum <- c(sum,mean(table$value[[i]]))
      }
      table$value1 <- sum
      table$value <- table$value1
      table$ZachValue <- table$value
      print("testing3")
      if (input$log_2 == T) {
        table$value <- table$value + 0.00001
        table$value <- log10(table$value)
      }
      print("testing4")
      table$displayValue <- table$value
      newTable <- table
      newTable <- newTable[,! colnames(newTable) %in% c("nutilVariable")]
      print("testing5")
    }
    else if (input$percent_2 == TRUE) {
      print("if percent")
      colnames(fullData) <- gsub("\\."," ",colnames(fullData))
      table <- aggregate(fullData[,c(displayX,displayY,"nutilVariable",ZBasics)], by =fullData[,c("x","y")], FUN=unique)
      
      table$value <- table[,"nutilVariable"]
      
      sum <- c()
      for (i in 1:length(table$value)){
        sum <- c(sum,mean(table$value[[i]]))
      }
      table$value1 <- sum
      
      table$value <- table$value1*100
      table$ZachValue <- table$value
      
      if (input$log_2 == T) {
        table$value <- table$value + 0.00001
        table$value <- log10(table$value)
        # table[is.na(table$value),17] <- 0
      }
      table$displayValue <- table$value
      newTable <- table
      newTable <- newTable[,! colnames(newTable) %in% c("nutilVariable")]
      #Logit scale?
      #Log scale maybe
    } else {
      table <- aggregate(fullData[,c(displayX,displayY,"nutilVariable",ZBasics)], by =fullData[,c("x","y")], FUN=unique)
      
      table$value <- table[,"nutilVariable"]
      
      sum <- c()
      for (i in 1:length(table$value)){
        sum <- c(sum,mean(table$value[[i]]))
      }
      table$value1 <- sum
      table$value <- table$value1
      table$ZachValue <- table$value
      
      if (input$log_2 == T) {
        table$value <- table$value + 0.00001
        table$value <- log10(table$value)
      }
      table$displayValue <- table$value
      newTable <- table
      newTable <- newTable[,! colnames(newTable) %in% c("nutilVariable")]
    }
    valueTable <<- newTable
    print("done making table")
    
    #Remove these regions
    theSearch <<- input$toRemove_2
    remove <- c()
    i <- 1
    for (i in 1:length(valueTable$forRemove)){
      id <- valueTable$forRemove[i]
      row <- tree[tree$X10 == id,-10]
      searched <- theSearch %in% row
      if (sum(searched)>0) {
        remove <- c(remove,"remove")
      }
      else {
        remove <- c(remove,"keep")
      }
    }
    valueTable <- valueTable[remove == "keep",]
    
    check3 <<- valueTable
    
    print("Done getting values for table and Check 3")
    
    #ZACHS Portion
    ZTable <- valueTable[,c(ZBasics,"ZachValue","displayValue")]
    theMax <<- input$max_value_2
    theMin <<- input$min_value_2
    toChangeMax <- ZTable$displayValue > theMax
    toChangeMin <- ZTable$displayValue < theMin
    toChangeMax[is.na(toChangeMax)] <- FALSE
    toChangeMin[is.na(toChangeMin)] <- FALSE
    ZTable[toChangeMax,"displayValue"] <- theMax
    ZTable[toChangeMin,"displayValue"] <- theMin
    
    ZTable <- ZTable %>% mutate_all(as.character)
    
    ZTable[is.na(ZTable)] <- "None"
    ZTable$max <- theMax
    ZTable$min <- theMin
    #add column to ZTable for min_value
    ZTable <<- apply(ZTable,2,as.character)
    ZTable <<- as.data.frame(ZTable)
    print(class(ZTable$mouse))
    
    output$mbh_2 <- downloadHandler(
      filename = function() {
        paste0("MBHTable", ".csv")
      },
      content = function(file) {
        ZTable <- apply(ZTable,2,as.character)
        write.csv(ZTable, file)
        print("Done Downloading the MBH Table")
      }
    )
    print("done thingy")
    
    # library(tidyverse)
    # 
    uniqueX <- unique(valueTable$x)
    uniqueY <- unique(valueTable$y)
    
    forData <- valueTable[,c("x","y","value")]
    attempt <- forData %>%
      pivot_wider(names_from=c(y), values_from=value)
    attempt <- data.frame(attempt)
    rownames(attempt) <- attempt$x
    data <- attempt[,-1]
    #organize attempt rows by rownames
    data <- data[order(rownames(data)), ]
    print("done Creating Data")
    
    #create y annotation using displayY
    annoY <- c()
    for (i in uniqueY) {
      row <- valueTable[valueTable$y == i,c(displayY)]
      if (length(displayY) == 1) {
        annoY <- rbind(annoY,row[1])
      }
      else {
        annoY <- rbind(annoY,row[1,])
      }
    }
    annoY <- data.frame(annoY)
    colnames(annoY) <- displayY
    rownames(annoY) <- uniqueY
    annoY <<- annoY
    
    annoX <- c()
    for (i in uniqueX) {
      row <- valueTable[valueTable$x == i,c(displayX)]
      if (length(displayX) == 1) {
        annoX <- rbind(annoX,row[1])
      }
      else {
        annoX <- rbind(annoX,row[1,])
      }
    }
    annoX <- data.frame(annoX)
    colnames(annoX) <- displayX
    rownames(annoX) <- uniqueX
    annoX <<- annoX
    
    check4 <<- data
    data <- check4
    
    if (input$trim_2 == TRUE) {
      moreThan <- 0.5 #if more than this percent is missing, remove
      #remove data if in less than half the brains
      if (length(data[1,]) < length(data[,1])) {
        testing <- rowSums(is.na(data))
        rowLength <- length(data[1,])
        rowThings <- names(testing)
        remove <- c()
        for (item in rowThings){
          value <- testing[item]
          if (value > rowLength*moreThan) {
            remove <- c(remove,item)
          }
        }
        data <- data[!rownames(data) %in% remove,]
        saved <- colnames(annoX)
        annoX <- annoX[!rownames(annoX) %in% remove,]
        annoX <- data.frame(annoX)
        colnames(annoX) <- saved
        rownames(annoX) <- uniqueX[!uniqueX %in% remove]
        
        
        testing <- colSums(is.na(data))
        colLength <- length(data[,1])
        colThings <- names(testing)
        remove <- c()
        for (item in colThings){
          value <- testing[item]
          if (value > colLength*moreThan) {
            remove <- c(remove,item)
          }
        }
        data <- data[,!colnames(data) %in% remove]
        saved <- colnames(annoY)
        annoY <- annoY[!rownames(annoY) %in% remove,]
        annoY <- data.frame(annoY)
        colnames(annoY) <- saved
        rownames(annoY) <- uniqueY[!uniqueY %in% remove]
      }
      else {
        testing <- colSums(is.na(data))
        colLength <- length(data[,1])
        colThings <- names(testing)
        remove <- c()
        for (item in colThings){
          value <- testing[item]
          if (value > colLength*moreThan) {
            remove <- c(remove,T)
          }
          else {
            remove <- c(remove, F)
          }
        }
        data <- data[,remove == F, drop = F]
        annoY <- annoY[remove == F,,drop = F]
        
        testing <- rowSums(is.na(data))
        rowLength <- length(data[1,])
        rowThings <- names(testing)
        remove <- c()
        for (item in rowThings){
          value <- testing[item]
          if (value > rowLength*moreThan) {
            remove <- c(remove,T)
          }
          else {
            remove <- c(remove, F)
          }
        }
        data <- data[remove == F,,drop = F]
        annoX <- annoX[remove ==F,,drop = F]
        
      }
    }
    print("done creating annotation")
    
    #Create save files to mess with
    write.csv(annoY,"annoY.csv")
    write.csv(data,"data.csv")
    write.csv(annoX,"annoX.csv")
    annoY1 <<- annoY
    annoX1 <<- annoX
    dataTest <<- data
    data1 <<- data
    
    output$data_2 <- downloadHandler(
      filename = function() {
        paste0("data", ".csv")
      },
      content = function(file) {
        write.csv(data, file)
        print("Done Downloading Data")
      }
    )
    
    output$annoX <- downloadHandler(
      filename = function() {
        paste0("annoX", ".csv")
      },
      content = function(file) {
        write.csv(annoX, file)
        print("Done Downloading Anno X")
      }
    )
    
    output$annoY_2 <- downloadHandler(
      filename = function() {
        paste0("annoY", ".csv")
      },
      content = function(file) {
        write.csv(annoY, file)
        print("Done Downloading Anno Y")
      }
    )
    
    print("done creating downloads")
    
    annoX <- annoX1
    correct <- c()
    for (i in 1:length(annoX[,1])) {
      good = F
      for (j in 1:length(annoX[1,])) {
        for (item in chosenOptions) {
          if (item == annoX[i,j]){
            good = T
          }
        }
      }
      correct <- c(correct,good)
    }
    annoX <- annoX[correct == T,,drop = F]
    data1 <- data1[correct == T,,drop = F]
    
    print("done segmenting")
    
    #Create the variables for the color palette
    breaks <- 20
    newMin <<- input$min_value_2
    newMax <<- input$max_value_2
    difference <- newMin-newMax
    breakNum <- abs(difference/(breaks+1))
    seqBreaks <<- seq(newMin, newMax, by=breakNum)
    data1[data1 > newMax] <- newMax
    data1[data1 < newMin] <- newMin
    
    print("colors")
    if (exists("dauColors")& exists("parColors") & exists("specColors")) {
      if ("daughter" %in% colnames(annoX)) {
        if ("parent" %in% colnames(annoX)){
          if ("specials" %in% colnames(annoX)) {
            parColors <- parColors[unique(annoX$parent)]
            specColors <- specColors[unique(annoX$specials)]
            dauColors <- dauColors[unique(annoX$daughter)]
            
            xAnnoColors <- list(parent = parColors,specials = specColors,daughter = dauColors)
          }
          else {
            parColors <- parColors[unique(annoX$parent)]
            dauColors <- dauColors[unique(annoX$daughter)]
            
            xAnnoColors <- list(parent = parColors, daughter = dauColors)
          }
        }
        else {
          dauColors <- dauColors[unique(annoX$daughter)]
          
          xAnnoColors <- list(daughter = dauColors)
        }
      }
      else if ("parent" %in% colnames(annoX)) {
        if ("specials" %in% colnames(annoX)) {
          parColors <- parColors[unique(annoX$parent)]
          specColors <- specColors[unique(annoX$specials)]
          
          xAnnoColors <- list(parent = parColors,specials = specColors)
        }
        else {
          parColors <- parColors[unique(annoX$parent)]
          
          xAnnoColors <- list(parent = parColors)
        }
      }
      else if ("specials" %in% colnames(annoX)){
        specColors <- specColors[unique(annoX$specials)]
        
        xAnnoColors <- list(specials = specColors)
      }
      else {
        xAnnoColors <- list()
      }
      
      #Colors for AnnoY!
      if ("daughter" %in% colnames(annoY)) {
        if ("parent" %in% colnames(annoY)){
          if ("specials" %in% colnames(annoY)) {
            parColors <- parColors[unique(annoY$parent)]
            specColors <- specColors[unique(annoY$specials)]
            dauColors <- dauColors[unique(annoY$daughter)]
            
            yAnnoColors <- list(parent = parColors,specials = specColors, daughter = dauColors)
          }
          else {
            parColors <- parColors[unique(annoY$parent)]
            dauColors <- dauColors[unique(annoY$daughter)]
            
            yAnnoColors <- list(parent = parColors, daughter = dauColors)
          }
        }
        else {
          dauColors <- dauColors[unique(annoY$daughter)] 
          
          yAnnoColors <- list(daughter = dauColors)
        }
      }
      else if ("parent" %in% colnames(annoY)) {
        if ("specials" %in% colnames(annoY)) {
          parColors <- parColors[unique(annoY$parent)]
          specColors <- specColors[unique(annoY$specials)]
          
          yAnnoColors <- list(parent = parColors,specials = specColors)
        }
        else {
          parColors <- parColors[unique(annoY$parent)]
          
          yAnnoColors <- list(parent = parColors)
        }
      }
      else if ("specials" %in% colnames(annoY)){
        specColors <- specColors[unique(annoY$specials)]
        
        yAnnoColors <- list(specials = specColors)
      }
      else {
        yAnnoColors <- list()
      }
    }
    else if (exists("parColors")){
      if ("daughter" %in% colnames(annoX)) {
        if ("parent" %in% colnames(annoX)){
          if ("specials" %in% colnames(annoX)) {
            parColors <- parColors[unique(annoX$parent)]
            
            xAnnoColors <- list(parent = parColors)
          }
          else {
            parColors <- parColors[unique(annoX$parent)]
            
            xAnnoColors <- list(parent = parColors)
          }
        }
        else {
          xAnnoColors <- list()
        }
      }
      else if ("parent" %in% colnames(annoX)) {
        if ("specials" %in% colnames(annoX)) {
          parColors <- parColors[unique(annoX$parent)]
          
          xAnnoColors <- list(parent = parColors)
        }
        else {
          parColors <- parColors[unique(annoX$parent)]
          
          xAnnoColors <- list(parent = parColors)
        }
      }
      else if ("specials" %in% colnames(annoX)){
        xAnnoColors <- list()
      }
      else {
        xAnnoColors <- list()
      }
      
      #Colors for AnnoY!
      if ("daughter" %in% colnames(annoY)) {
        if ("parent" %in% colnames(annoY)){
          if ("specials" %in% colnames(annoY)) {
            parColors <- parColors[unique(annoY$parent)]
            
            yAnnoColors <- list(parent = parColors)
          }
          else {
            parColors <- parColors[unique(annoY$parent)]
            
            yAnnoColors <- list(parent = parColors)
          }
        }
        else {
          yAnnoColors <- list()
        }
      }
      else if ("parent" %in% colnames(annoY)) {
        if ("specials" %in% colnames(annoY)) {
          parColors <- parColors[unique(annoY$parent)]
          
          yAnnoColors <- list(parent = parColors)
        }
        else {
          parColors <- parColors[unique(annoY$parent)]
          
          yAnnoColors <- list(parent = parColors)
        }
      }
      else if ("specials" %in% colnames(annoY)){
        yAnnoColors <- list()
      }
      else {
        yAnnoColors <- list()
      }
    }
    else if (exists("specColors")) {
      if ("daughter" %in% colnames(annoX)) {
        if ("parent" %in% colnames(annoX)){
          if ("specials" %in% colnames(annoX)) {
            specColors <- specColors[unique(annoX$specials)]
            
            xAnnoColors <- list(specials = specColors)
          }
          else {
            xAnnoColors <- list()
          }
        }
        else {
          xAnnoColors <- list()
        }
      }
      else if ("parent" %in% colnames(annoX)) {
        if ("specials" %in% colnames(annoX)) {
          specColors <- specColors[unique(annoX$specials)]
          
          xAnnoColors <- list(specials = specColors)
        }
        else {
          xAnnoColors <- list()
        }
      }
      else if ("specials" %in% colnames(annoX)){
        specColors <- specColors[unique(annoX$specials)]
        
        xAnnoColors <- list(specials = specColors)
      }
      else {
        xAnnoColors <- list()
      }
      
      #Colors for AnnoY!
      if ("daughter" %in% colnames(annoY)) {
        if ("parent" %in% colnames(annoY)){
          if ("specials" %in% colnames(annoY)) {
            specColors <- specColors[unique(annoY$specials)]
            
            yAnnoColors <- list(specials = specColors)
          }
          else {
            xAnnoColors <- list()
          }
        }
        else {
          yAnnoColors <- list()
        }
      }
      else if ("parent" %in% colnames(annoY)) {
        if ("specials" %in% colnames(annoY)) {
          specColors <- specColors[unique(annoY$specials)]
          
          yAnnoColors <- list(specials = specColors)
        }
        else {
          xAnnoColors <- list()
        }
      }
      else if ("specials" %in% colnames(annoY)){
        specColors <- specColors[unique(annoY$specials)]
        
        yAnnoColors <- list(specials = specColors)
      }
      else {
        yAnnoColors <- list()
      }
    }
    else {
      xAnnoColors <- list()
      yAnnoColors <- list()
    }
    
    annoColors <<- append(xAnnoColors,yAnnoColors)
    print(annoColors)
    
    print("start sorting")
    annoYtest <<- annoY
    annoXtest <<- annoX
    data1test <<- data1
    # copy <- annoYtest
    rownames(annoY) <-  gsub("/", ".", rownames(annoY), fixed=TRUE)
    rownames(annoY) <-  gsub("-", ".", rownames(annoY), fixed=TRUE)
    rownames(annoY) <-  gsub(" ", ".", rownames(annoY), fixed=TRUE)
    colnames(data1) <-  gsub("/", ".", colnames(data1), fixed=TRUE)
    colnames(data1) <-  gsub("-", ".", colnames(data1), fixed=TRUE)
    colnames(data1) <-  gsub(" ", ".", colnames(data1), fixed=TRUE)
    # data1 <- data1test
    # annoX <- annoXtest
    # annoY <- annoYtest
    #Sorting by Side then ABA
    if ("side" %in% colnames(annoY) | "side" %in% colnames(annoX)) {
      print("yes side")
      if ("parent" %in% colnames(annoY)) {
        testing <- match(annoY$parent,tree$X1)
        testing1 <- c(1:length(annoY$parent))
        side <- annoY$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("parent" %in% colnames(annoX)) {
        testing <- match(annoX$parent,tree$X1)
        testing1 <- c(1:length(annoX$parent))
        side <- annoX$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
      else if ("specials" %in% colnames(annoY)) {
        testing <- match(annoY$specials,tree$X1)
        testing1 <- c(1:length(annoY$specials))
        side <- annoY$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("specials" %in% colnames(annoX)) {
        testing <- match(annoX$specials,tree$X1)
        testing1 <- c(1:length(annoX$specials))
        side <- annoX$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
      else if ("daughter" %in% colnames(annoY)) {
        testing <- match(annoY$daughter,tree$X1)
        testing1 <- c(1:length(annoY$daughter))
        side <- annoY$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("daughter" %in% colnames(annoX)) {
        testing <- match(annoX$daughter,tree$X1)
        testing1 <- c(1:length(annoX$daughter))
        side <- annoX$side
        test <- data.frame(testing,testing1,side)
        test <- test[order(test$side,test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
    }
    else {
      print("no side")
      if ("parent" %in% colnames(annoY)) {
        testing <- match(annoY$parent,tree$X1)
        testing1 <- c(1:length(annoY$parent))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("parent" %in% colnames(annoX)) {
        testing <- match(annoX$parent,tree$X1)
        testing1 <- c(1:length(annoX$parent))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
      else if ("specials" %in% colnames(annoY)) {
        testing <- match(annoY$specials,tree$X1)
        testing1 <- c(1:length(annoY$specials))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("specials" %in% colnames(annoX)) {
        testing <- match(annoX$specials,tree$X1)
        testing1 <- c(1:length(annoX$specials))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
      else if ("daughter" %in% colnames(annoY)) {
        testing <- match(annoY$daughter,tree$X1)
        testing1 <- c(1:length(annoY$daughter))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoY <- annoY[test$testing1,]
        data1 <- data1[,test$testing1]
      }
      else if ("daughter" %in% colnames(annoX)) {
        testing <- match(annoX$daughter,tree$X1)
        testing1 <- c(1:length(annoX$daughter))
        test <- data.frame(testing,testing1)
        test <- test[order(test$testing),]
        annoX <- annoX[test$testing1,]
        data1 <- data1[test$testing1,]
      }
    }
    print("done sorting")
    
    dataKeep <<- data1
    annoXKeep <<- annoX
    annoYKeep <<- annoY
    
    #labels_col and labels_row
    print(xNames)
    print(colnames(annoX))
    annoX <- annoXKeep
    annoY <- annoYKeep
    
    #sort annox and y 
    
    #sort annoX and annoY based on the organization of the data
    keyAnnoX <- data.frame(DataRows = (rownames(data1)),sortid = c(1:length(rownames(data1))))
    keyAnnoY <- data.frame(DataRows = (colnames(data1)),sortid = c(1:length(colnames(data1))))
    
    annoX <- data.frame(annoX,DataRows = (rownames(annoX)))
    annoY <- data.frame(annoY,DataRows = (rownames(annoY)))
    
    
    #merge with colname and row name, then sort
    annoX <- merge(annoX,keyAnnoX,by = "DataRows")
    annoY <- merge(annoY,keyAnnoY,by = "DataRows")
    rownames(annoX) <- annoX$DataRows
    rownames(annoY) <- annoY$DataRows
    
    annoX <- annoX[order(annoX$sortid), ]
    annoX <- annoX[,!colnames(annoX) %in% c("sortid","DataRows"), drop = FALSE]
    annoY <- annoY[order(annoY$sortid), ]
    annoY <- annoY[,!colnames(annoY) %in% c("sortid","DataRows"), drop = FALSE]
    
    
    
    if (xNames %in% colnames(annoX)) {
      rowName <- unlist(annoX[,xNames])
    } else if (xNames == "None") {
      rowName <- vector( "character" , length(unlist(rownames(annoX))))
    } else {
      rowName <- unlist(rownames(annoX))
    }
    
    if (yNames %in% colnames(annoY)) {
      colName <- unlist(annoY[,yNames])
    } else if (yNames == "None") {
      colName <- vector( "character" , length(unlist(rownames(annoY))))
    } else {
      colName <- unlist(rownames(annoY))
    }
    
    
    print("heatmap colors")
    breaks <<- breaks
    #default colors
    if (input$inverted_2 == T){
      heatmapColors <- viridis::inferno(breaks,direction = 1)
    } else {
      heatmapColors <- viridis::inferno(breaks,direction = -1)
    }
    #if two or more custom colors are selected, default colors are used
    if (input$color2_2 == T & input$color3_2 == T | input$viridis_2 == T & input$color3_2 == T | input$viridis_2 == T & input$color2_2 == T) {
      output$minMax_2 <- renderText({
        print("Select only one of the custom color options please. The heatmap output will be in the default colors for now.")
      })
    }
    else if (input$viridis_2 == T) {
      choice <- input$viridisScale_2
      if (input$inverted_2 == T){
        heatmapColors <- viridis::viridis(breaks,direction = 1,option = choice)
      } else {
        heatmapColors <- viridis::viridis(breaks,direction = -1,option = choice)
      }
      
      output$minMax_2 <- renderText({
        print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
      })
    }
    else if (input$color2_2 == T) {
      range <- colorRampPalette(c(input$col1_2,input$col2_2))
      heatmapColors <- range(length(heatmapColors)) 
      
      output$minMax_2 <- renderText({
        print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
      })
    }
    else if (input$color3_2 == T) {
      range <- colorRampPalette(c(input$colo1_2,input$colo2_2,input$colo3_2))
      heatmapColors <- range(length(heatmapColors))   
      
      output$minMax_2 <- renderText({
        print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
      })
    }
    
    heatmapColorsForZach <<- heatmapColors
    #update colors download
    output$colors_2 <- downloadHandler(
      filename = function() {
        paste("heatmapColors.rds")
      },
      content = function(file) {
        saveRDS(heatmapColorsForZach, file)
      }
    )
    
    laAnnoX <<- annoX
    laAnnoY <<- annoY
    annoX <- annoX %>%
      mutate(across(everything(), as.character))
    annoY <- annoY %>%
      mutate(across(everything(), as.character))
    
    print("start creating thePalette")
    thePalette <<- readRDS("RequiredFiles/colorPalette.rds") #made using distinctthePalette
    NamesCols <- c()
    uniqueVals <- list()
    matchingColors <- list()
    location <- 1
    for (i in 1:length(annoX[1,])) {
      NamesCols <- c(NamesCols,colnames(annoX)[i])
      
      len <- length(uniqueVals)
      uniques <- unique(annoX[,i])
      uniqueVals[[len+1]] <- uniques
      
      colors <- c()
      for (item in uniques) {
        colo <- thePalette[location]
        colors <- c(colors,colo)
        location <- location + 1
      }
      
      len <- length(matchingColors)
      matchingColors[[len+1]] <- colors
    }
    
    for (i in 1:length(annoY[1,])) {
      NamesCols <- c(NamesCols,colnames(annoY)[i])
      
      len <- length(uniqueVals)
      uniques <- unique(annoY[,i])
      uniqueVals[[len+1]] <- uniques
      
      colors <- c()
      for (item in uniques) {
        colo <- thePalette[location]
        colors <- c(colors,colo)
        location <- location + 1
      }
      
      len <- length(matchingColors)
      matchingColors[[len+1]] <- colors
    }
    
    blankList <- list()
    for (i in 1:length(uniqueVals)) {
      namePart <- uniqueVals[[i]]
      valuePart <- matchingColors[[i]]
      len <- length(blankList)
      blankList[[len+1]] <- setNames(as.vector(valuePart),namePart)
    }
    names(blankList) <- NamesCols
    
    fullColors <- blankList
    names(fullColors)
    if ("parent" %in% names(fullColors)) {
      fullColors$parent <- annoColors$parent
    }
    if ("specials" %in% names(fullColors)) {
      fullColors$specials <- annoColors$specials
    }
    if("daughter" %in% names(fullColors)) {
      fullColors$daughter <- annoColors$daughter
    }
    print("finished creating thePalette")
    
    fullColors <<- fullColors
    saveAnnoX <<- annoX
    saveAnnoY <<- annoY
    # annoX <- saveAnnoX
    # annoY <- saveAnnoY
    
    colName <<- colName
    rowName <<- rowName
    heatmapColors <<- heatmapColors
    seqBreaks <<- seqBreaks
    saveData <<- data1
    # data1 <- saveData
    annoXWithout <- annoX[,!colnames(annoX) %in% c("daughter","parent","specials"), drop = FALSE]
    annoYWithout <- annoY[,!colnames(annoY) %in% c("daughter","parent","specials"), drop = FALSE]
    
    
    set.seed(100)
    
    print("Here")
    
    output$minMax_2 <- renderText({
      print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
    })
    if (length(annoYWithout) != 0 & length(annoXWithout) != 0) {
      heatmap2 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                            labels_col = colName,labels_row = rowName, color = heatmapColors, border_color=NA,
                            annotation_row = annoXWithout,annotation_col = annoYWithout, annotation_colors = fullColors)
    }
    else if (length(annoYWithout) != 0) {
      heatmap2 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                            labels_col = colName,labels_row = rowName, color = heatmapColors, border_color=NA,
                            annotation_col = annoYWithout, annotation_colors = fullColors)
    }
    else if (length(annoXWithout) != 0) {
      heatmap2 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                            labels_col = colName,labels_row = rowName, color = heatmapColors, border_color=NA,
                            annotation_row = annoXWithout, annotation_colors = fullColors)
    }
    else {
      heatmap2 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                            labels_col = colName,labels_row = rowName, color = heatmapColors, border_color=NA,
                            annotation_colors = fullColors)
    }
    
    
    print("Testing")
    
    
    heatmap1 <<- pheatmap(data1,fontsize_row = 5,fontsize_col = 5,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F,cellwidth = 5 ,cellheight = 5, 
                          labels_col = colName,labels_row = rowName, color = heatmapColors,border_color=NA,
                          annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)
    
    heatmap3 <- pheatmap(data1,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F, 
                         labels_col = colName,labels_row = rowName, color = heatmapColors,border_color=NA,
                         annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)
    # saveRDS(heatmap1,"heatmapSave.rds")
    # 
    # theHeatmap <- readRDS("heatmapSave.rds")
    print("start creating merge")
    grid.newpage()
    grid.draw(heatmap2)
    grid.force()
    see <- grid.ls()
    testing <- see$gPath
    partial <- testing[!str_detect(testing, "layout::annotation_legend") ]
    check1 <- unlist(strsplit(partial, split='::', fixed=TRUE))
    check2 <- check1[!str_detect(check1, "layout") ]
    
    for (item in check2) {
      tryCatch(
        expr = {
          grid.remove(item)
        },
        error = function(e){
          message('Caught an error!')
        },
        warning = function(w){
          message('Caught a warning!')
        },
        finally = {
          message('All done, quitting.')
        }
      )
    }
    theLegend <- grid.grab()
    
    grid.newpage()
    grid.draw(heatmap1)
    grid.force()
    see <- grid.ls()
    partial <- testing[str_detect(testing, "layout::annotation_legend") ]
    check1 <- unlist(strsplit(partial, split='::', fixed=TRUE))
    check2 <- check1[!str_detect(check1, "layout") ]
    
    for (item in check2) {
      tryCatch(
        expr = {
          grid.remove(item)
        },
        error = function(e){
          message('Caught an error!')
        },
        warning = function(w){
          message('Caught a warning!')
        },
        finally = {
          message('All done, quitting.')
        }
      )
    }
    theGraph <- grid.grab()
    
    
    grid.newpage()
    grid.draw(theLegend)
    grid.draw(theGraph)
    FullPlot <<- grid.grab()
    
    saveRDS(FullPlot,"fullPlot.rds")
    
    
    heatmap3 <- pheatmap(data1,cluster_rows = F, main = "Both", breaks = seqBreaks,cluster_cols = F, 
                         labels_col = colName,labels_row = rowName, color = heatmapColors,border_color=NA,
                         annotation_row = annoX,annotation_col = annoY, annotation_colors = fullColors)
    grid.newpage()
    grid.draw(heatmap3)
    grid.force()
    toUse <- grid.grab()
    look1 <- grid.get("layout")
    vpLayout <- look1[["vp"]][[2]][["layout"]]
    vpLayout$ncol <- as.integer(5)
    vpLayout$widths <- vpLayout$widths[-c(6)]
    vpLayout$respect.mat <- vpLayout$respect.mat[,-c(6)]
    grid.show.layout(vpLayout)
    grid.newpage()
    pushViewport(viewport(layout = vpLayout, name = "testing" ))
    
    look <- getGrob(toUse, "layout")
    theGrobs <- look[["grobs"]]
    while(length(grep("legend",theGrobs)) >0) {
      theGrobs[[grep("legend",theGrobs)[1]]] <- NULL
    }
    theGrobs[[grep("col_names",theGrobs)]] <- NULL
    theGrobs[[grep("row_names",theGrobs)]] <- NULL
    
    for (i in 1:length(theGrobs)) {
      grid.draw(theGrobs[[i]])
    }
    
    noExtras <- grid.grab()
    
    print("done creating merge")
    
    
    
    print(paste("Min: ",min(data,na.rm = TRUE),", Max: ",max(data,na.rm = TRUE), sep = ""))
    
    output$normPlot_2 <- renderPlot({
      grid.draw(FullPlot)
    })
    
    save_heatmap_pdf <- function(x, filename, width=100, height=50) {
      stopifnot(!missing(x))
      stopifnot(!missing(filename))
      pdf(filename, width=width, height=height)
      grid::grid.newpage()
      grid::grid.draw(x)
      dev.off()
    }
    # save_pheatmap_pdf(heatmap1, "testing.pdf")
    
    
    output$heatmap_2 <- downloadHandler(
      filename = function() {
        paste0("heatmap", ".pdf")
      },
      content = function(file) {
        save_heatmap_pdf(FullPlot, file)
        print("Done Downloading Heatmap")
      }
    )
    
    output$justPlot_2 <- downloadHandler(
      filename = function() {
        paste0("justPlot", ".pdf")
      },
      content = function(file) {
        save_heatmap_pdf(noExtras, file, width = 6.5, height = 3)
        print("Done Downloading Heatmap")
      }
    )
    
    # save(list=c("checkData","checkData1","chosenOptions","nutilVariable","groupBy","xNames","yNames","valueTable","check3","annoY1","annoX1","data1","annoColors","origData","origAnnoY","origAnnoX","dataKeep","annoXKeep","annoYKeep","x","y","displayX","displayY","check4"), file="finished.RData")
    
    output$print_2 <- renderText({
      print(paste0(Sys.time()," Done Heatmap, Check 3, Anno Y, Anno X, and Data"))
    })
    
  })
  
  
  
  ##### ANATOMICAL VIEW #####
  # observeEvent(input$SVG,{
  #   # file <- input$SVG
  #   print("started loading svg")
  #   SVG <- readRDS("svgData.rds")#file = file$datapath)
  # View(SVG[SVG$id == 564 & SVG$keyName == 46,])
  #   
  #   SVG <- SVG[SVG$keyName %in% c(23,46,67,82,88,96),]
  #   SVG$id <- as.numeric(SVG$id)
  #   colnames(SVG)[2] <- "Region ID"
  #   SVG <<- SVG
  #   chosenOptions <<- input$optionsX
  #   nutilVariable <<- input$sumWith
  #   groupBy <<- input$groupBy
  #   annoVariable <<- input$annoVariable
  #   print("Done loading SVG")
  #   
  #   view(SVG)
  #   output$print <- renderText({
  #     print("Done Loading SVG")
  #   })
  # })
  # 
  # #use the table from before to create heatmap. look at untitled.r for working anatomical
  # observeEvent(input$anatomical,{
  #   #Create x and y cols
  #   if (length(x) > 1) {
  #     forAnno$x <- apply(forAnno[ ,x] , 1 , paste , collapse = "_" )
  #   }
  #   else {
  #     forAnno$x <- forAnno[ ,x]
  #   }
  #   
  #   if (length(y) > 1) {
  #     forAnno$y <- apply(forAnno[ ,y] , 1 , paste , collapse = "_" )
  #   }
  #   else {
  #     forAnno$y <- forAnno[ ,y]
  #   }
  #   print("created new cols")
  #   
  #   savecols <- c()
  #   for (i in 1:length(colnames(forAnno))) {
  #     currentCol <- forAnno[,i]
  #     if (any(currentCol %in% annoVariable) == TRUE) {
  #       savecols <- c(savecols,i)
  #     }
  #   }
  #   savecols <- as.numeric(savecols)
  #   savecols <- colnames(forAnno)[savecols]
  #   #table based on x and y with displays being included using unique
  #   table <- aggregate(forAnno[,c(nutilVariable,groupBy,"Region ID",savecols)], by = forAnno[,groupBy], FUN=unique)
  #   print("tabled")
  #   
  #   table$toSum <- table[,nutilVariable]
  #   
  #   sum <- c()
  #   for (i in 1:length(table$toSum)){
  #     sum <- c(sum,sum(table$toSum[[i]]))
  #   }
  #   table$sum <- sum
  #   tableKeep <- table[,c(groupBy,"sum")]
  #   
  #   table <- aggregate(forAnno[,c(nutilVariable,groupBy,"Region ID",savecols)], by =forAnno[,c("x","y")], FUN=unique)
  #   
  #   newTable <- table %>% left_join( tableKeep, 
  #                                    by=groupBy)
  #   newTable$toSum <- newTable[,nutilVariable]
  
  # if (input$percent == TRUE) {
  #   newTable$value <- newTable$toSum*100
  #   if (input$log == T) {
  #     newTable$value <- newTable$value + 0.00001
  #     newTable$value <- log10(newTable$value)
  #   }
  #   #Logit scale?
  #   #Log scale maybe
  # }
  # else {
  #   newTable$value <- newTable$toSum/newTable$sum*100
  #   if (input$log == T) {
  #     newTable$value <- newTable$value + 0.00001
  #     newTable$value <- log10(newTable$value)
  #   }
  #   #Linear mostly
  # }
  #   
  #   annoTable <<- newTable
  #   theTable <- annoTable
  #   colnames(theTable)[1:2] <- c("X","Y")
  #   write.csv(theTable,"check4.csv")
  #   
  #   toKeepRegions <- theTable %>% filter_all(any_vars(. %in% annoVariable))
  #   toKeepRegions <- unique(toKeepRegions$`Region ID`)
  #   
  #   newMin <- input$colorScale[1]
  #   newMax <- input$colorScale[2]
  #   
  #   for (vari in annoVariable) {
  #     #only data for current variable
  #     segmented <- theTable %>% filter_all(any_vars(. %in% vari))
  #     segmented$`Region ID` <- as.numeric(segmented$`Region ID`)
  #     if ("daughter" %in% groupBy){
  #       colnames(segmented)[colnames(segmented)=="daughter"] <- "X1"
  #     }
  #     else if ("parent" %in% groupBy){
  #       colnames(segmented)[colnames(segmented)=="parent"] <- "X1"
  #     }
  #     segmented1 <- left_join(segmented,tree[,c("X1","X10")], by = "X1")
  #     table <- aggregate(segmented1[,c("value")], by = segmented1[,c("X1","X10","side")], FUN=mean)
  #     colnames(table)[4] <- "value"
  #     
  #     #join with svg
  #     colnames(table)[colnames(table)=="X10"] <- "Region ID"
  #     table$`Region ID` <- as.numeric(table$`Region ID`)
  #     colnames(table)[colnames(table)=="side"] <- "hemi"
  #     testing <- left_join(SVG, table, by=c("Region ID","hemi"))
  #     testing1 <- testing[testing$`Region ID` %in% toKeepRegions,]
  #     
  #     #create anatomical heat map
  #     p <- ggplot(testing1, aes(x = x, y = y)) +
  #       geom_polygon(aes(fill = value, group = unique), #fill = value,
  #                    color = "black",
  #                    size = .25) +
  #       scale_y_reverse() + facet_wrap( ~ keyName, ncol = 3) +
  #       theme_classic(12) +
  #       scale_fill_gradientn(colours=rev(viridis::inferno(20)),limits=c(newMin,newMax),na.value = "grey",n.breaks=20)+
  #       coord_fixed() +
  #       ggtitle(vari) +
  #       theme(
  #         plot.title = element_text(hjust = 0.5,face = "bold"),
  #         axis.line = element_blank(),
  #         axis.text.x = element_blank(),
  #         axis.text.y = element_blank(),
  #         axis.ticks = element_blank(),
  #         axis.title.x = element_blank(),
  #         axis.title.y = element_blank(),
  #         panel.background = element_blank(),
  #         panel.border = element_blank(),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank(),
  #         plot.background = element_blank(),
  #         legend.position = "bottom",
  #         legend.title = element_blank(),
  #         legend.key.width = unit(3.5, 'cm'),
  #         strip.background = element_blank()
  #       )
  #     ggsave(paste(vari,".pdf",sep=""), p, width = 10, height = 10)
  #   }
  #   
  #   library(qpdf)
  #   files <- list.files(pattern = "\\.pdf$")
  #   output$anatomical <- downloadHandler(
  #     filename = function() {
  #       paste0("anatomical", ".pdf")
  #     },
  #     content = function(file) {
  #       qpdf::pdf_combine(input = files,
  #                         output = file)
  #       print("Done Downloading Anatomical")
  #     }
  #   )
  #   
  #   output$normPlot <- renderPlot({
  #     p
  #   })
  #   
  #   output$print <- renderText({
  #     print("Done Saving Plots")
  #   })
  # })
}


shinyApp(ui = ui, server = server)