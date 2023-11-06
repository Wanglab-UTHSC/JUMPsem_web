#eSEM.R

# server-KSEM.R
library(eSEM)
options(scipen=999)

#Retrive the user upload raw data
eSEMRun <- reactiveValues(eSEMRunValue = FALSE)

#load the user selected data type
dataType <- reactive(input$eSEMdataType)


#If user wants to load example data
observeEvent(input$eSEMSampleData,{
  if(input$eSEMSampleData == "pspSample"){
    variables$eSEMRaw <- input_psp_example
  }else if(input$eSEMSampleData == "ubiSample"){
    variables$eSEMRaw <- input_ubi_example
  }else{
    data <- data.frame(fread("sample_data/input_ace_example.csv"))
    temp <- data[,4:ncol(data)]
    temp <- temp %>% mutate_if(is.character,as.numeric)
    variables$eSEMRaw <- cbind(data[,1:3],temp)
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
    result <- eSEM(input = input_psp_example,
                   datatype = "psp",
                   organism = "mouse",
                   input.log2.norm = TRUE,
                   whole.log2.trans = TRUE,
                   whole.proteome = wholeProteome_example)
  }else if(input$eSEMSampleData == "ubiSample"){
    result <- eSEM(input = input_ubi_example,
                   datatype = "ubi",
                   organism = "human",
                   input.log2.norm = T)
  }else{
    result <- eSEM(input = input_ace_example,
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
  output$Activity_raw <- DT::renderDataTable({
    if (nrow(variables$Activity_raw) == 0) {
      DT::datatable(variables$Activity_raw)
    }else{
      data <- variables$Activity_raw
      data <- format(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
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
  output$Activity_MeanCenter <- DT::renderDataTable({
    if (nrow(variables$Activity_MeanCenter) == 0) {
      DT::datatable(variables$Activity_MeanCenter)
    }else{
      data <- variables$Activity_MeanCenter
      data <- format(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
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
  output$Activity_Zscore <- DT::renderDataTable({
    if (nrow(variables$Activity_Zscore) == 0) {
      DT::datatable(variables$Activity_Zscore)
    }else{
      data <- variables$Activity_Zscore
      data <- format(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
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
  output$Affinity <- DT::renderDataTable({
    if (nrow(variables$Affinity) == 0) {
      DT::datatable(variables$Affinity)
    }else{
      data <- variables$Affinity
      data <- format(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
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
    v$eSEMRunValue <- FALSE
    
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
      v$eSEMRunValue <- FALSE
      
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
  
  # if(input$eSEMdataType == "ace"){
  #   temp <- df[,4:ncol(df)]
  #   temp <- format(temp,digits = 2)
  #   df <- cbind(variables$eSEMRaw[,1:3],temp)
  # }else{
  #   temp <- df[,5:ncol(df)]
  #   temp <- format(temp,digits = 2)
  #   df <- cbind(variables$eSEMRaw[,1:4],temp)
  # }

  
  
  brks <-
    quantile(df %>% select_if(is.numeric),
             probs = seq(.05, .95, .05),
             na.rm = TRUE
    )
  
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
      
      v$eSEMRunValue <- FALSE
      
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

#if run eSEM button is clicked, run eSEM analysis.

observeEvent(input$runeSEM,{
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
  #ks = input$eSEMks
  coroff <- input$eSEMcoroff
  kmooff <- input$eSEMkmooff
  dtype <- input$eSEMdataType
  inputLog2 <- input$eSEMlog2 == "Yes"
  inputwholeLog2 <- input$eSEMWholelog2 == "Yes"
  
  
  
  updateProgressBar(
    session = session,
    id = "eSEMProgress",
    title = "Run eSEM Analysis",
    value = 50
  )
  
  #run eSEM analysis
  result <- eSEM(input = rawdata,
                 datatype = dtype,
                 organism = organism,
                 whole.proteome = wholePro,
                 cor.off = coroff,
                 kmo.off = kmooff,
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
  
  
  
  
  write.csv(activity_raw,"result/Activity_Raw.csv")
  write.csv(activity_meancenter,"result/Activity_MeanCenter.csv")
  write.csv(activity_zscore,"result/Activity_Zscore.csv")
  write.csv(affinty,"result/Affinity.csv")
  
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
  output$Activity_raw <- DT::renderDataTable({
    if (nrow(variables$Activity_raw) == 0) {
      DT::datatable(variables$Activity_raw)
    }else{
      data <- variables$Activity_raw
      data <- format(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
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
  output$Activity_MeanCenter <- DT::renderDataTable({
    if (nrow(variables$Activity_MeanCenter) == 0) {
      DT::datatable(variables$Activity_MeanCenter)
    }else{
      data <- variables$Activity_MeanCenter
      data <- format(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
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
  output$Activity_Zscore <- DT::renderDataTable({
    if (nrow(variables$Activity_Zscore) == 0) {
      DT::datatable(variables$Activity_Zscore)
    }else{
      data <- variables$Activity_Zscore
      data <- format(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
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
  output$Affinity <- DT::renderDataTable({
    if (nrow(variables$Affinity) == 0) {
      DT::datatable(variables$Affinity)
    }else{
      data <- variables$Affinity
      data <- format(data, digits = 2)
      DT::datatable(
        data,
        filter = "bottom",
        selection = 'single',
        extensions = c("Scroller", "Buttons"),
        option = list(
          dom = 'Bfrtip',
          buttons =
            list(
              'copy',
              'print',
              list(
                extend = 'collection',
                buttons = c('csv', 'excel', 'pdf'),
                text = 'Download'
              )
            ),
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
  #data <- data[!rowSums(data >5),]
  #data = t(scale(t(data), center = T, scale = T))
  
  data <- as.matrix(data)
  p <- heatmaply(
    data,
    xlab = "Sample",
    ylab = "Protein ID",
    Rowv = T,
    Colv = F,
    scale = "row",
    k_row = 2,
    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
      low = "blue", 
      high = "red", 
      midpoint = 0, 
      limits = c(-3, 3)
    ),
    main = "Activity Raw Data(Top 100)"
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
    k_row = 2,
    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
      low = "blue", 
      high = "red", 
      midpoint = 0, 
      limits = c(-1, 1)
    ),
    main = "Activity Mean Center Data(Top 100)"
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
    k_row = 2,
    scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
      low = "blue", 
      high = "red", 
      midpoint = 0, 
      limits = c(-5, 5)
    ),
    main = "Activity Z-Score Data(Top 100)"
  )%>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "Activity Z Score"
      ))
  
  variables$ActivityZScoreHM <- p
  p
})


