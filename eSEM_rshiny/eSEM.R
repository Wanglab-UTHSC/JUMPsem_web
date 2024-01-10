#eSEM.R

# server-KSEM.R
library(eSEM)
options(scipen=999)

#Retrive the user upload raw data
eSEMRun <- reactiveValues(eSEMRunValue = FALSE)

#load the user selected data type
dataType <- reactive(input$eSEMdataType)


#If user wants to load example data

#Hide other tabs when clicking on Example data
observe({
  if(input$eSEMdataSource == "Sample Data"){
    shinyjs::show(selector = ".rowhide")
    variables$eSEMRaw <- data.frame()
    eSEMRun$eSEMRunValue <- FALSE
    
  }else{
    shinyjs::hide(selector = ".rowhide")
    if(input$eSEMSampleData == "pspSample"){
      variables$eSEMRaw <- input_psp_example_T
    }else if(input$eSEMSampleData == "ubiSample"){
      variables$eSEMRaw <- input_ubi_example_T
    }else{
      data <- input_ace_example_T
      temp <- data[,4:ncol(data)]
      temp <- temp %>% mutate_if(is.character,as.numeric)
      variables$eSEMRaw <- cbind(data[,1:3],temp)
    }
    eSEMRun$eSEMRunValue <- FALSE
  } 
})




#process the eSEM analysis directly
observeEvent(input$eSEMDataSampleRun,{
  
  #load sample data
  #run Sample result
  
  progressSweetAlert(
    session = session,
    id = "eSEMSampleProgress",
    title = "Read in raw data",
    display_pct = TRUE,
    value = 0
  )
  updateProgressBar(
    session = session,
    id = "eSEMSampleProgress",
    title = "Run eSEM analysis",
    value = 30
  )
  
  if(input$eSEMSampleData == "pspSample"){
    result <- JUMPsem(input = input_psp_example_T,
                   datatype = "psp",
                   organism = "mouse",
                   enzyme.organism = c("human", "mouse", "rat"),
                   input.log2.norm = TRUE,
                   whole.log2.trans = TRUE,
                   motif = motif_example,
                   whole.proteome = wholeProteome_example)
  }else if(input$eSEMSampleData == "ubiSample"){
    result <- JUMPsem(input = input_ubi_example_T,
                   datatype = "ubi",
                   organism = "human",
                   input.log2.norm = T)
  }else{
    result <- JUMPsem(input = input_ace_example_T,
                   datatype = "ace",
                   organism = "human",
                   input.log2.norm = FALSE)
  }
  
  updateProgressBar(
    session = session,
    id = "eSEMSampleProgress",
    title = "Process Results",
    value = 50
  )
  
  #save results in variables
  activity_raw <- result$Activity_Raw[,-1]
  activity_meancenter <- result$Activity_MeanCenter[,-1]
  activity_zscore <- result$Activity_Zscore[,-1]
  affinity <- result$Affinity
  
  updateProgressBar(
    session = session,
    id = "eSEMSampleProgress",
    title = "Save Results",
    value = 60
  )
  
  #store results
  variables$Activity_raw <- activity_raw
  variables$Activity_MeanCenter <- activity_meancenter
  variables$Activity_Zscore <- activity_zscore
  variables$Affinity <- affinity
  
  
  updateProgressBar(
    session = session,
    id = "eSEMSampleProgress",
    title = "Format Activity Raw Table",
    value = 70
  )
  
  
  # Render result table on the right top ----
  
  # Add a download button to each table
  output$download_raw <- downloadHandler(
    filename = "Activity_raw.csv",
    content = function(file) {
      write.csv(activity_raw, file)
    }
  )
  
  # Construct data tables
  output$Activity_raw <- DT::renderDataTable({
    if (nrow(variables$Activity_raw) == 0) {
      DT::datatable(variables$Activity_raw)
    }else{
      data <- variables$Activity_raw
      data <- round(data, digits = 2)
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
    id = "eSEMSampleProgress",
    title = "Activity Meancenter",
    value = 80
  )
  
  
  #activity_meancenter
  output$download_meancenter <- downloadHandler(
    filename = "Activity_meancenter.csv",
    content = function(file) {
      write.csv(activity_meancenter, file)
    }
  )
  output$Activity_MeanCenter <- DT::renderDataTable({
    if (nrow(variables$Activity_MeanCenter) == 0) {
      DT::datatable(variables$Activity_MeanCenter)
    }else{
      data <- variables$Activity_MeanCenter
      data <- round(data, digits = 2)
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
    id = "eSEMSampleProgress",
    title = "Activity Zscore",
    value = 90
  )
  #activity_zscore
  output$download_zscore <- downloadHandler(
    filename = "Activity_zscore.csv",
    content = function(file) {
      write.csv(activity_zscore, file)
    }
  )
  output$Activity_Zscore <- DT::renderDataTable({
    if (nrow(variables$Activity_Zscore) == 0) {
      DT::datatable(variables$Activity_Zscore)
    }else{
      data <- variables$Activity_Zscore
      data <- round(data, digits = 2)
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
    id = "eSEMSampleProgress",
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
  
  eSEMRun$eSEMRunValue <- input$eSEMDataSampleRun
  
  closeSweetAlert(session = session)
  sendSweetAlert(
    session = session,
    title = "DONE",
    text = "eSEM analysis done.",
    type = "success"
  )
})


#upload the substrate data
observeEvent(input$uploadeSEMSubstrateData,{
  #show notification of uploading
  showNotification("Start uploading file...", type = "message")
  
  tryCatch({
    #read in uploaded substrate raw data
    subrawdata <-
      data.frame(fread(input$uploadeSEMSubstrateData$datapath))
    
    #check if sample values are numeric
    if(input$eSEMdataType == "ace"){
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
    
    #write.csv(subrawdata,"eSEMraw.csv")
    eSEMRun$eSEMRunValue <- FALSE
    
    variables$eSEMRaw <- subrawdata
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
# eSEMdataInput <- reactive({
#   variables$eSEMRaw
# })


#upload normalization data
observeEvent(input$uploadWholeProteomicsData,{
  #show notification of uploading
  showNotification("Start uploading file...", type = "message")
  
  tryCatch(
    {
      #read in uploaded normalization data
      eSEMnorm <- data.frame(fread(input$uploadWholeProteomicsData$datapath))
      
      #omit NA values
      eSEMnorm <- na.omit(eSEMnorm)
      
      #check data
      #write.csv(eSEMnorm,"test/eSEMnorm.csv")
      eSEMRun$eSEMRunValue <- FALSE
      
      #store in variables
      variables$eSEMnorm <- eSEMnorm
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
  df <- variables$eSEMRaw 
  
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

output$eSEMRawTable <- renderUI({
  if (nrow(variables$eSEMRaw) == 0) {
    tags$p("No data to show. Click",tags$code("Upload"), "your own dataset.")
  } else {
    DT::dataTableOutput("rawTable")
  }
})


#----------Group info(optional)------------
observeEvent(input$uploadeSEMGroup,{
  # if (nrow(variables$eSEMRaw) == 0) {
  #   sendSweetAlert(
  #     session = session,
  #     title = "ERROR",
  #     text = "Please input subsrate data!",
  #     type = "error"
  #   )
  #   return()
  # }
  
  tryCatch(
    {
      #read in uploaded group information
      eSEMgroup <- data.frame(fread(input$uploadeSEMGroup$datapath))
      
      eSEMRun$eSEMRunValue <- FALSE
      
      #store in variables
      variables$eSEMgroup <- eSEMgroup
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
#Import eSEM function

#if run eSEM button is clicked, check parameters.
observeEvent(input$runeSEM,{
  #obtain parameters
  organism <- input$eSEMorg
  coroff <- input$eSEMcoroff
  kmooff <- input$eSEMkmooff
  dtype <- input$eSEMdataType
  inputLog2 <- input$eSEMlog2 == "Yes"
  inputwholeLog2 <- input$eSEMWholelog2 == "Yes"
  checkwhole <- ifelse(nrow(variables$eSEMnorm) == 0,yes = "None", no = "Yes")
  
  
  #show modal when button is clicked
  #show warning if user select certain parameters
  showModal(modalDialog(
    tags$h2('Please check your parameters',style="text-align:center"),
    tags$p(tags$b("Data Type"),": ", names(dt_choice)[dt_choice == dtype]),
    tags$p(tags$b("Substrate Species"),": ", names(orgchoice)[orgchoice == organism]),
    #tags$p(tags$b("Enzyme Species"),": ", names(orgchoice)[orgchoice == organism]),
    tags$p(tags$b("Whole Proteome"),": ", checkwhole),
    tags$p(tags$b("Correlation Cutoff"),": ", coroff),
    tags$p(tags$b("KMO Cutoff"),": ", kmooff),
    tags$p(tags$b("Need substrate data log 2 transformation?"), input$eSEMlog2),
    tags$p(tags$b("Need whole proteome data log 2 transformation?"), input$eSEMWholelog2),
    span("Click ",tags$b("Cancel "), "to modify your choice or ", tags$b("Submit "), "to run eSEM analysis."),
    
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
    id = "eSEMProgress",
    title = "Read in raw data",
    display_pct = TRUE,
    value = 0
  )
  
  #obtain substrate data
  rawdata <- variables$eSEMRaw
  
  updateProgressBar(
    session = session,
    id = "eSEMProgress",
    title = "Check whole proteomics data",
    value = 20
  )
  #obtain whole proteomics data if possible
  if(nrow(variables$eSEMnorm) != 0){
    wholePro <- variables$eSEMnorm
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
    id = "eSEMProgress",
    title = "Check substrate data",
    value = 20
  )
  updateProgressBar(
    session = session,
    id = "eSEMProgress",
    title = "Read in parameters",
    value = 40
  )
  
  
  
  #obtain parameters
  organism <- input$eSEMorg
  coroff <- input$eSEMcoroff
  kmooff <- input$eSEMkmooff
  dtype <- input$eSEMdataType
  inputLog2 <- input$eSEMlog2 == "Yes"
  inputwholeLog2 <- input$eSEMWholelog2 == "Yes"
  #enzymeSpe <- input$enzymeSpe

  
  updateProgressBar(
    session = session,
    id = "eSEMProgress",
    title = "Run eSEM Analysis",
    value = 50
  )
  
  #run eSEM analysis
  result <- JUMPsem(input = rawdata,
                 datatype = dtype,
                 organism = organism,
                 whole.proteome = wholePro,
                 cor.off = coroff,
                 kmo.off = kmooff,
                 #enzyme.organism = enzymeSpe,
                 input.log2.norm = inputLog2,
                 whole.log2.trans = inputwholeLog2)
  
  
  updateProgressBar(
    session = session,
    id = "eSEMProgress",
    title = "Get Result",
    value = 60
  )
  
  #save results in variables
  activity_raw <- result$Activity_Raw[,-1]
  activity_meancenter <- result$Activity_MeanCenter[,-1]
  activity_zscore <- result$Activity_Zscore[,-1]
  affinity <- result$Affinity
  
  
  
  updateProgressBar(
    session = session,
    id = "eSEMProgress",
    title = "Store result data",
    value = 70
  )
  #store results
  variables$Activity_raw <- activity_raw
  variables$Activity_MeanCenter <- activity_meancenter
  variables$Activity_Zscore <- activity_zscore
  variables$Affinity <- affinity
  
  
  updateProgressBar(
    session = session,
    id = "eSEMProgress",
    title = "Render result table",
    value = 80
  )
  
  # Render result table on the right top ----
  
  output$download_raw <- downloadHandler(
    filename = "Activity_raw.csv",
    content = function(file) {
      write.csv(activity_raw, file)
    }
  )
  output$Activity_raw <- DT::renderDataTable({
    if (nrow(variables$Activity_raw) == 0) {
      DT::datatable(variables$Activity_raw)
    }else{
      data <- variables$Activity_raw
      data <- round(data, digits = 2)
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
  
  #activity_meancenter
  output$download_meancenter <- downloadHandler(
    filename = "Activity_meancenter.csv",
    content = function(file) {
      write.csv(activity_meancenter, file)
    }
  )
  output$Activity_MeanCenter <- DT::renderDataTable({
    if (nrow(variables$Activity_MeanCenter) == 0) {
      DT::datatable(variables$Activity_MeanCenter)
    }else{
      data <- variables$Activity_MeanCenter
      data <- round(data, digits = 2)
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
  
  #activity_zscore
  output$download_zscore <- downloadHandler(
    filename = "Activity_zscore.csv",
    content = function(file) {
      write.csv(activity_zscore, file)
    }
  )
  output$Activity_Zscore <- DT::renderDataTable({
    if (nrow(variables$Activity_Zscore) == 0) {
      DT::datatable(variables$Activity_Zscore)
    }else{
      data <- variables$Activity_Zscore
      data <- round(data, digits = 2)
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
    id = "eSEMProgress",
    title = "Render result table",
    value = 100
  )
  
  closeSweetAlert(session = session)
  sendSweetAlert(
    session = session,
    title = "DONE",
    text = "eSEM analysis done.",
    type = "success"
  )
  eSEMRun$eSEMRunValue <- input$runeSEM
  
})


#render output tables under each tab

output$ActivityRawTable <- renderUI({
  if(eSEMRun$eSEMRunValue){
    tagList(
      fluidRow(column(
        12, 
        downloadButton("download_raw", "Download Result Table"),
        DT::dataTableOutput('Activity_raw') %>% withSpinner()
      )))} else {
        helpText("Click [Run eSEM Analysis] to obtain Result Table.")
      }
})

output$MeanCenterTable <- renderUI({
  if(eSEMRun$eSEMRunValue){
    tagList(
      fluidRow(column(
        12, 
        downloadButton("download_meancenter", "Download Result Table"),
        DT::dataTableOutput('Activity_MeanCenter') %>% withSpinner()
      )))} else {
        helpText("Click [Run eSEM Analysis] to obtain Result Table.")
      }
})

output$zscoreTable <- renderUI({
  if(eSEMRun$eSEMRunValue){
    tagList(
      fluidRow(column(
        12, 
        downloadButton("download_zscore", "Download Result Table"),
        DT::dataTableOutput('Activity_Zscore') %>% withSpinner()
      )))} else {
        helpText("Click [Run eSEM Analysis] to obtain Result Table.")
      }
})

output$Affnity <- renderUI({
  if(eSEMRun$eSEMRunValue){
    tagList(
      fluidRow(column(
        12, 
        downloadButton("download_affinity", "Download Result Table"),
        DT::dataTableOutput('Affinity') %>% withSpinner()
      )))} else {
        helpText("Click [Run eSEM Analysis] to obtain Result Table.")
      }
})





#--------------------Heatmap--------------------

#raw data
output$ActivityRawHM <- renderUI({
  if(eSEMRun$eSEMRunValue){
    plotlyOutput("rawHeatmap", height = 800)%>% withSpinner()
  }
  else{
    helpText("No data to plot. Run eSEM analysis first.")
  }
})

output$rawHeatmap <- renderPlotly({
  data <- variables$Activity_raw
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
    main = "Activity Raw Data"
  )%>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "Activity Raw Data"
      ))
  
  variables$ActivityRawHM <- p
  p
})


# Mean Center
output$MeanCenterHM <- renderUI({
  if(eSEMRun$eSEMRunValue){
    plotlyOutput("MeanCenterHeatmap", height = 800)%>% withSpinner()
  }
  else{
    helpText("No data to plot. Run eSEM analysis first.")
  }
})

output$MeanCenterHeatmap <- renderPlotly({
  data <- variables$Activity_MeanCenter
  rown <- nrow(data)
  if(rown <= 100){
    selectn <- rown
  }else{
    selectn <- 100
  }
  
  data <- data[1:selectn,]
  data <- data[!rowSums(data >5),]
  
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
      limits = c(-1, 1)
    ),
    main = "Activity Mean Center Data"
  )%>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "Activity Mean Center"
      ))
  
  variables$ActivityMeanCenterHM <- p
  p
})

#z-score
output$zscoreHM <- renderUI({
  if(eSEMRun$eSEMRunValue){
    plotlyOutput("ZScoreHeatmap", height = 800)%>% withSpinner()
  }
  else{
    helpText("No data to plot. Run eSEM analysis first.")
  }
})

output$ZScoreHeatmap <- renderPlotly({
  data <- variables$Activity_Zscore
  rown <- nrow(data)
  if(rown <= 100){
    selectn <- rown
  }else{
    selectn <- 100
  }
  
  data <- data[1:selectn,]
  data <- data[!rowSums(data >5),]
  
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
      limits = c(-1, 1)
    ),
    main = "Activity Z-Score Data"
  )%>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "Activity Z Score"
      ))
  
  variables$ActivityZScoreHM <- p
  p
})


