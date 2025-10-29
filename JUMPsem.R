#JUMPsem.R

# server-JUMPsem.R
library(JUMPsem)
options(scipen=999)

#Retrive the user upload raw data
jumpsemRun <- reactiveValues(jumpsemRunValue = FALSE)

#load the user selected data type
dataType <- reactive(input$jumpsemdataType)


#If user wants to load example data

#Hide other tabs when clicking on Example data
observe({
  if(input$jumpsemdataSource == "Sample Data"){
    shinyjs::show(selector = ".rowhide")
    variables$jumpsemRaw <- data.frame()
    jumpsemRun$jumpsemRunValue <- FALSE
    
  }else{
    shinyjs::hide(selector = ".rowhide")
    if(input$jumpsemSampleData == "pspSample"){
      variables$jumpsemRaw <- input_psp_example_T
    }else if(input$jumpsemSampleData == "ubiSample"){
      variables$jumpsemRaw <- input_ubi_example_T
    }else{
      data <- input_ace_example_T
      temp <- data[,4:ncol(data)]
      temp <- temp %>% mutate_if(is.character,as.numeric)
      variables$jumpsemRaw <- cbind(data[,1:3],temp)
    }
    jumpsemRun$jumpsemRunValue <- FALSE
  } 
})




#process the jumpsem analysis directly
observeEvent(input$jumpsemDataSampleRun,{
  
  #load sample data
  #run Sample result
  
  progressSweetAlert(
    session = session,
    id = "jumpsemSampleProgress",
    title = "Read in raw data",
    display_pct = TRUE,
    value = 0
  )
  updateProgressBar(
    session = session,
    id = "jumpsemSampleProgress",
    title = "Run JUMPsem analysis",
    value = 30
  )
  
  if(input$jumpsemSampleData == "pspSample"){
    result <- JUMPsem(input = variables$jumpsemRaw,
                      datatype = "psp",
                      organism = "mouse",
                      enzyme.organism = c("human", "mouse", "rat"),
                      input.log2.norm = TRUE,
                      whole.log2.trans = TRUE,
                      motif = motif_example,
                      whole.proteome = wholeProteome_example)
  }else if(input$jumpsemSampleData == "ubiSample"){
    result <- JUMPsem(input = variables$jumpsemRaw,
                   datatype = "ubi",
                   organism = "human",
                   input.log2.norm = T)
  }else{
    result <- JUMPsem(input = variables$jumpsemRaw,
                   datatype = "ace",
                   organism = "human",
                   input.log2.norm = FALSE)
  }
  
  updateProgressBar(
    session = session,
    id = "jumpsemSampleProgress",
    title = "Process Results",
    value = 50
  )
  
  #save results in variables
  activity <- result$Activity
  affinity <- result$Affinity
  eval <- result$Evaluations
  
  updateProgressBar(
    session = session,
    id = "jumpsemSampleProgress",
    title = "Save Results",
    value = 60
  )
  
  #store results
  variables$Activity <- activity
  variables$Evaluations <- eval
  variables$Affinity <- affinity
  
  
  updateProgressBar(
    session = session,
    id = "jumpsemSampleProgress",
    title = "Format Activity Raw Table",
    value = 70
  )
  
  
  # Render result table on the right top ----
  
  # Add a download button to each table
  output$download_raw <- downloadHandler(
    filename = "Activity.csv",
    content = function(file) {
      write.csv(activity, file)
    }
  )
  
  # Construct data tables
  output$Activity <- DT::renderDataTable({
    if (nrow(variables$Activity) == 0) {
      DT::datatable(variables$Activity)
    }else{
      data <- variables$Activity
      #data <- round(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'lfrtip',
          deferRender = TRUE,
          scrollY = 400,
          scrollX = TRUE,
          scroller = TRUE,
          
          pageLength = 5,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          columnDefs = list(
            list(visible = TRUE, targets = -1)
          )
        )
        
      )
    }
  })
  
  updateProgressBar(
    session = session,
    id = "jumpsemSampleProgress",
    title = "Affinity",
    value = 100
  )
  
  #affinity
  output$download_affinity <- downloadHandler(
    filename = "Affinity.csv",
    content = function(file) {
      write.csv(affinity, file)
    }
  )
  output$Affinity <- DT::renderDataTable({
    if (nrow(variables$Affinity) == 0) {
      DT::datatable(variables$Affinity)
    }else{
      data <- variables$Affinity
      #data <- round(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'lfrtip',
          deferRender = TRUE,
          scrollY = 400,
          scrollX = TRUE,
          scroller = TRUE,
          
          pageLength = 5,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          columnDefs = list(
            list(visible = TRUE, targets = -1)
          )
        )
        
      )
    }
  })
  
  
  output$download_eval <- downloadHandler(
    filename = "Evaluations.csv",
    content = function(file) {
      write.csv(eval, file)
    }
  )
  output$Evaluations <- DT::renderDataTable({
    if (nrow(variables$Evaluations) == 0) {
      DT::datatable(variables$Evaluations)
    }else{
      data <- variables$Evaluations
      #data <- round(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'lfrtip',
          deferRender = TRUE,
          scrollY = 400,
          scrollX = TRUE,
          scroller = TRUE,
          
          pageLength = 5,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          columnDefs = list(
            list(visible = TRUE, targets = -1)
          )
        )
        
      )
    }
  })
  
  
  jumpsemRun$jumpsemRunValue <- input$jumpsemDataSampleRun
  
  closeSweetAlert(session = session)
  sendSweetAlert(
    session = session,
    title = "DONE",
    text = "JUMPsem analysis done.",
    type = "success"
  )
})


#upload the substrate data
observeEvent(input$uploadjumpsemSubstrateData,{
  #show notification of uploading
  showNotification("Start uploading file...", type = "message")
  
  tryCatch({
    #read in uploaded substrate raw data
    subrawdata <-
      data.frame(fread(input$uploadjumpsemSubstrateData$datapath))
    
    #check if sample values are numeric
    if(input$jumpsemdataType == "ace"){
      temp <- subrawdata[,4:ncol(subrawdata)]
      temp <- temp %>% mutate_if(is.character,as.numeric)
      subrawdata <- cbind(subrawdata[,1:3],temp)
    }else{
      temp <- subrawdata[,5:ncol(subrawdata)]
      temp <- temp %>% mutate_if(is.character,as.numeric)
      subrawdata <- cbind(subrawdata[,1:4],temp)
    }
    
    #omit NA values
    #subrawdata<- na.omit(subrawdata)
    
    #write.csv(subrawdata,"jumpsemraw.csv")
    jumpsemRun$jumpsemRunValue <- FALSE
    
    variables$jumpsemRaw <- subrawdata
    showNotification("Received uploaded file.", type = "message")
  },
  error = function(e) {
    sendSweetAlert(
      session = session,
      title = "Input data error!",
      text = as.character(message(e)),
      type = "error"
    )
    return()
  },
  warning = function(w) {
    sendSweetAlert(
      session = session,
      title = "Input data warning!",
      text = "Some error is in your dataset, it maybe cause some problem we cannot expected.",
      type = "warning"
    )
    return()
  }
  
  )
  
})
# jumpsemdataInput <- reactive({
#   variables$jumpsemRaw
# })


#upload normalization data
observeEvent(input$uploadWholeProteomicsData,{
  #show notification of uploading
  showNotification("Start uploading file...", type = "message")
  
  tryCatch(
    {
      #read in uploaded normalization data
      jumpsemnorm <- data.frame(fread(input$uploadWholeProteomicsData$datapath))
      
      #omit NA values
      jumpsemnorm <- na.omit(jumpsemnorm)
      
      #check data
      #write.csv(jumpsemnorm,"test/jumpsemnorm.csv")
      jumpsemRun$jumpsemRunValue <- FALSE
      
      #store in variables
      variables$jumpsemnorm <- jumpsemnorm
      showNotification("Received uploaded file.", type = "message")
      
    },
    error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Input data error!",
        text = as.character(message(e)),
        type = "error"
      )
      return()
    },
    warning = function(w) {
      sendSweetAlert(
        session = session,
        title = "Input data warning!",
        text = "Some error is in your dataset, it maybe cause some problem we cannot expected.",
        type = "warning"
      )
      return()
    }
  )
})



#--------------------Table Part----------------------

# Render a table of raw substrate data, adding color ----
output$rawTable <- DT::renderDataTable({
  df <- variables$jumpsemRaw 
  
  brks <-
    quantile(df %>% select_if(is.numeric),
             probs = seq(.05, .95, .05),
             na.rm = TRUE
    )
  
  colInd <- 5:ncol(df)
  df[colInd] <- round(df[colInd], digits = 2)
  DT::datatable(
    df,
    extensions = c("Scroller","RowReorder"),
    option = list(
      rowReorder = TRUE,
      deferRender = TRUE,
      autoWidth = TRUE,
      scrollY = 400,
      scroller = TRUE,
      scrollX = TRUE,
      pageLength = 5,
      searchHighlight = TRUE,
      orderClasses = TRUE
    )
  ) %>%
    formatStyle(names(df %>% select_if(is.numeric)), backgroundColor = styleInterval(brks, head(Blues(40), n = length(brks) + 1)))
})

# Render datatable in UI of row substrate data ----

output$jumpsemRawTable <- renderUI({
  if (nrow(variables$jumpsemRaw) == 0) {
    tags$p("No data to show. Click",tags$code("Upload"), "your own dataset.")
  } else {
    DT::dataTableOutput("rawTable")
  }
})


#----------Group info(optional)------------
observeEvent(input$uploadjumpsemGroup,{
  
  tryCatch(
    {
      #read in uploaded group information
      jumpsemgroup <- data.frame(fread(input$uploadjumpsemGroup$datapath))
      
      jumpsemRun$jumpsemRunValue <- FALSE
      
      #store in variables
      variables$jumpsemgroup <- jumpsemgroup
      showNotification("Received uploaded file.", type = "message")
      
    },
    error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Input data error!",
        text = as.character(message(e)),
        type = "error"
      )
      return()
    },
    warning = function(w) {
      sendSweetAlert(
        session = session,
        title = "Input data warning!",
        text = "Some error is in your dataset, it maybe cause some problem we cannot expected.",
        type = "warning"
      )
      return()
    }
  )
  
})



#-----------Result----------
#Activity Result
#Import jumpsem function

#if run jumpsem button is clicked, check parameters.
observeEvent(input$runjumpsem,{
  #obtain parameters
  organism <- input$jumpsemorg
  #coroff <- input$jumpsemcoroff
  kmooff <- input$jumpsemkmooff
  dtype <- input$jumpsemdataType
  inputLog2 <- input$jumpsemlog2 == "Yes"
  inputwholeLog2 <- input$jumpsemWholelog2 == "Yes"
  motif <- if (!is.null(input$uploadmotif))
    data.frame(fread(input$uploadmotif$datapath))
  else
    NULL
  enzyme_org <- input$enzyme_org
  checkwhole <- ifelse(nrow(variables$jumpsemnorm) == 0,yes = "None", no = "Yes")
  
  sel <- names(orgchoice)[orgchoice %in% enzyme_org]
  label_text <- if (length(sel)) paste(sel, collapse = ", ") else "None"
  
  #show modal when button is clicked
  #show warning if user select certain parameters
  showModal(modalDialog(
    tags$h2('Please check your parameters',style="text-align:center"),
    tags$p(tags$b("Data Type"),": ", names(dt_choice)[dt_choice == dtype]),
    tags$p(tags$b("Substrate Species"),": ", organism),
    tags$p(tags$b("Enzyme Species"),": ", label_text),
    tags$p(tags$b("Whole Proteome"),": ", checkwhole),
    #tags$p(tags$b("Correlation Cutoff"),": ", coroff),
    tags$p(tags$b("KMO Cutoff"),": ", kmooff),
    tags$p(tags$b("Need substrate data log 2 transformation?"), input$jumpsemlog2),
    tags$p(tags$b("Need whole proteome data log 2 transformation?"), input$jumpsemWholelog2),
    span("Click ",tags$b("Cancel "), "to modify your choice or ", tags$b("Submit "), "to run jumpsem analysis."),
    
    easyClose = T,
    fade = T,
    footer = tagList(
      modalButton("Cancel"),
      actionButton("submit", "Submit")
    )
  ))
  
})


observeEvent(input$submit,{
  removeModal()
  progressSweetAlert(
    session = session,
    id = "jumpsemProgress",
    title = "Read in raw data",
    display_pct = TRUE,
    value = 0
  )
  
  #obtain substrate data
  rawdata <- variables$jumpsemRaw
  
  updateProgressBar(
    session = session,
    id = "jumpsemProgress",
    title = "Check whole proteomics data",
    value = 20
  )
  #obtain whole proteomics data if possible
  if(nrow(variables$jumpsemnorm) != 0){
    wholePro <- variables$jumpsemnorm
  }else{
    wholePro <- NULL
  }
  
  if (nrow(rawdata)==0) {
    sendSweetAlert(
      session = session,
      title = "ERROR",
      text = "Please input count data table!",
      type = "error"
    )
    return()
  }
  updateProgressBar(
    session = session,
    id = "jumpsemProgress",
    title = "Check substrate data",
    value = 20
  )
  updateProgressBar(
    session = session,
    id = "jumpsemProgress",
    title = "Read in parameters",
    value = 40
  )
  
  
  
  #obtain parameters
  organism <- input$jumpsemorg
  #coroff <- input$jumpsemcoroff
  kmooff <- input$jumpsemkmooff
  dtype <- input$jumpsemdataType
  inputLog2 <- input$jumpsemlog2 == "Yes"
  inputwholeLog2 <- input$jumpsemWholelog2 == "Yes"
  motif <- if (!is.null(input$uploadmotif))
    data.frame(fread(input$uploadmotif$datapath))
  else
    NULL
  enzyme_org <- input$enzyme_org
  #enzymeSpe <- input$enzymeSpe

  
  updateProgressBar(
    session = session,
    id = "jumpsemProgress",
    title = "Run jumpsem Analysis",
    value = 50
  )
  
  #run jumpsem analysis
  result <- JUMPsem(input = rawdata,
                 datatype = dtype,
                 organism = organism,
                 whole.proteome = wholePro,
                 kmo.off = kmooff,
                 #enzyme.organism = enzymeSpe,
                 input.log2.norm = inputLog2,
                 whole.log2.trans = inputwholeLog2,
                 motif = motif,
                 enzyme.organism = enzyme_org)
  
  
  updateProgressBar(
    session = session,
    id = "jumpsemProgress",
    title = "Get Result",
    value = 60
  )
  
  #save results in variables
  activity <- result$Activity
  affinity <- result$Affinity
  eval <- result$Evaluations
  
  
  
  updateProgressBar(
    session = session,
    id = "jumpsemProgress",
    title = "Store result data",
    value = 70
  )
  #store results
  variables$Activity <- activity
  variables$Evaluations <- eval
  variables$Affinity <- affinity
  
  
  updateProgressBar(
    session = session,
    id = "jumpsemProgress",
    title = "Render result table",
    value = 80
  )
  
  # Render result table on the right top ----
  
  output$download_raw <- downloadHandler(
    filename = "Activity.csv",
    content = function(file) {
      write.csv(Activity, file)
    }
  )
  output$Activity <- DT::renderDataTable({
    if (nrow(variables$Activity) == 0) {
      DT::datatable(variables$Activity)
    }else{
      data <- variables$Activity
      #data <- round(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'lfrtip',
          deferRender = TRUE,
          scrollY = 400,
          scrollX = TRUE,
          scroller = TRUE,
          
          pageLength = 5,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          columnDefs = list(
            list(visible = TRUE, targets = -1)
          )
        )
        
      )
    }
  })
  
  #Evaluations download
  output$download_eval <- downloadHandler(
    filename = "Evaluations.csv",
    content = function(file) {
      write.csv(eval, file)
    }
  )
  output$Evaluations <- DT::renderDataTable({
    if (nrow(variables$Evaluations) == 0) {
      DT::datatable(variables$Evaluations)
    }else{
      data <- variables$Evaluations
      #data <- round(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'lfrtip',
          deferRender = TRUE,
          scrollY = 400,
          scrollX = TRUE,
          scroller = TRUE,
          
          pageLength = 5,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          columnDefs = list(
            list(visible = TRUE, targets = -1)
          )
        )
        
      )
    }
  })
  

  
  #affinity
  output$download_affinity <- downloadHandler(
    filename = "Activity_affinity.csv",
    content = function(file) {
      write.csv(affinity, file)
    }
  )
  output$Affinity <- DT::renderDataTable({
    if (nrow(variables$Affinity) == 0) {
      DT::datatable(variables$Affinity)
    }else{
      data <- variables$Affinity
      #data <- round(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'lfrtip',
          # buttons =
          #   list(
          #     'copy',
          #     'print',
          #     list(
          #       extend = 'collection',
          #       buttons = c('csv', 'excel', 'pdf'),
          #       text = 'Download'
          #     )
          #   ),
          deferRender = TRUE,
          scrollY = 400,
          scrollX = TRUE,
          scroller = TRUE,
          
          pageLength = 5,
          searchHighlight = TRUE,
          orderClasses = TRUE,
          columnDefs = list(
            list(visible = TRUE, targets = -1)
          )
        )
        
      )
    }
  })
  
  updateProgressBar(
    session = session,
    id = "jumpsemProgress",
    title = "Render result table",
    value = 100
  )
  
  closeSweetAlert(session = session)
  sendSweetAlert(
    session = session,
    title = "DONE",
    text = "jumpsem analysis done.",
    type = "success"
  )
  jumpsemRun$jumpsemRunValue <- input$runjumpsem
  
})


#render output tables under each tab

output$ActivityRawTable <- renderUI({
  if(jumpsemRun$jumpsemRunValue){
    tagList(
      fluidRow(column(
        12, 
        downloadButton("download_raw", "Download Result Table"),
        DT::dataTableOutput('Activity') %>% withSpinner()
      )))} else {
        helpText("Click [Run jumpsem Analysis] to obtain Result Table.")
      }
})

output$evaluationsTable <- renderUI({
  if(jumpsemRun$jumpsemRunValue){
    tagList(
      fluidRow(column(
        12, 
        downloadButton("download_eval", "Download Result Table"),
        DT::dataTableOutput('Evaluations') %>% withSpinner()
      )))} else {
        helpText("Click [Run jumpsem Analysis] to obtain Result Table.")
      }
})

output$Affnity <- renderUI({
  if(jumpsemRun$jumpsemRunValue){
    tagList(
      fluidRow(column(
        12, 
        downloadButton("download_affinity", "Download Result Table"),
        DT::dataTableOutput('Affinity') %>% withSpinner()
      )))} else {
        helpText("Click [Run jumpsem Analysis] to obtain Result Table.")
      }
})





#--------------------Heatmap--------------------

#raw data
output$ActivityRawHM <- renderUI({
  if(jumpsemRun$jumpsemRunValue){
    plotlyOutput("rawHeatmap", height = 800)%>% withSpinner()
  }
  else{
    helpText("No data to plot. Run jumpsem analysis first.")
  }
})

output$rawHeatmap <- renderPlotly({
  data <- variables$Activity
  data <- data[-1]
  rown <- nrow(data)
  if(rown <= 100){
    selectn <- rown
  }else{
    selectn <- 100
  }
  
  data <- data[1:selectn,]

  
  data <- as.matrix(data)
  p <- heatmaply(
    data,
    xlab = "Sample",
    ylab = "Protein ID",
    Rowv = T,
    Colv = F,
    scale = "row",
    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
      low = "blue", 
      high = "red", 
      midpoint = 0, 
      limits = c(-3, 3)
    ),
    main = "Activity"
  )%>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "Activity"
      ))
  
  variables$ActivityRawHM <- p
  p
})



