# ui-eSEM.R

library(shiny)
library(shinythemes)
library(fresh)

my_theme = create_theme(
  adminlte_color(
    light_blue = "#4898a8"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#E8E8E8",
    box_bg = "#FFF", 
    info_box_bg = "#FFF"
  )
)

dashboardPage(
  dashboardHeader(title = "eSEM Analysis",
                  dropdownMenu(
                    type = "notifications", 
                    icon = icon("info-circle"),
                    badgeStatus = NULL,
                    headerText = "Help Documents",
                    
                    notificationItem("Documentation", icon = icon("file"),
                                     status = "success",
                                     href = "https://rawcdn.githack.com/Wanglab-UTHSC/eSEM_web/4e2226edde8770e5dfe77e24e09e11fcce56281a/English_Welcome.html"
                                     ))
                  ),
  dashboardSidebar(
    disable = T,
    sidebarMenu(
      id = "sider",
      menuItem(text = "eSEM",
               tabName = "esem",
               icon = icon("dashboard")
      )
    )

  ),
  dashboardBody(
    use_theme(my_theme),
    tabItem(
      tabName = "esem",
      fluidPage(
        theme = "simplex",
        fluidRow(column(
          3,
          tabBox(
            title = "",
            id = "eSEMdataSource",
            width = NULL,
            tabPanel(
              tagList(icon("book"), "Example"),
              selectInput(
                "eSEMSampleData",
                "Select Sample Data",
                choices = c(
                  "Phosphorylation" = "pspSample",
                  "Ubiquination" = "ubiSample",
                  "Acetylation" = "aceSample"
                )),
              tags$p("Example eSEM raw data for illustration, containing three data types to show."),
              do.call(actionBttn, c(
                list(
                  inputId = "eSEMDataSampleRun",
                  label = "Run Example Data",
                  icon = icon("play")
                ),
                actionBttnParams
              ))
            ),
            tabPanel(
              tagList(icon("folder-open"), "Sample"),
              tags$p("Upload your own sample"),
              radioButtons(
                "eSEMdataType",
                "Choose your data type",
                choices = c("Phosphorylation" = "psp",
                            "Ubiquitination" = "ubi",
                            "Acetylation" = "ace")
              ),
              fileInput(
                "uploadeSEMSubstrateData",
                "Upload Substrate Data",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                buttonLabel = "Upload...",
                placeholder = "No file has been uploaded."
              ),
              fileInput(
                "uploadWholeProteomicsData",
                "Upload Normalization Data(optional)",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                buttonLabel = "Upload...",
                placeholder = "No file has been uploaded."
              ),
              helpText("If you want to normalize the raw data, please upload normalization data file."),
              footer = helpText("Text file in .txt/.csv format."),
            )
          ),
          box(
            title = tagList(icon("info-circle"), "Group Information(optional)"),
            solidHeader = TRUE,
            width = NULL,
            status = "primary",
            fileInput(
              "uploadeSEMGroup",
              "Upload Group Information",
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv"),
              buttonLabel = "Upload...",
              placeholder = "No file has been uploaded."
            )
          ),
          box(
            title = tagList(icon("tags"), "Parameters"),
            solidHeader = TRUE,
            width = NULL,
            status = 'primary',
            selectInput(inputId = "eSEMorg",label = "Substrate Species",
                        choices = c("Human" = "human",
                                    "Mouse" = "mouse",
                                    "Rat" = "rat")),
            do.call(actionBttn, c(
              list(
                inputId = "runeSEM",
                label = "run eSEM Analysis",
                icon = icon("play")),
              actionBttnParams
            )
            )
          ),
          box(
            title = tagList(icon("option-vertical",lib = "glyphicon"), "Advanced Parameters"),
            solidHeader = TRUE,
            status = 'primary',
            width = NULL,
            collapsible = T,
            collapsed = T,
            numericInput(inputId = "eSEMcoroff", label = "Correlation Cutoff",
                         value = 0.95,
                         min = 0,
                         max = 1,
                         step = 0.01,
                         width = NULL),
            bsTooltip(
              id = "eSEMcoroff",
              title = "Set up correlation cutoff value 0-1 to remove high collinear variables.",
              placement = "right"
            ),
            numericInput(inputId = "eSEMkmooff", label = "KMO Cutoff",
                         value = 0,
                         min = 0,
                         max = 1,
                         step = 0.01,
                         width = NULL),
            bsTooltip(
              id = "eSEMkmooff",
              title = "Set up KMO cutoff value 0-1. Default is 0.",
              placement = "right"
            ),
            radioButtons(
              inputId = "eSEMlog2",
              label = "Need substrate data log 2 normalization?",
              choices = c("Yes",
                          "No"),
              selected = "No"
            ),
            radioButtons(
              inputId = "eSEMWholelog2",
              label = "Need whole proteome data log 2 normalization?",
              choices = c("Yes",
                          "No"),
              selected = "No"
            ),
            bsTooltip(
              id = "eSEMlog2",
              title = "Need program to do log2 transforming of the input
file or not.",
              placement = "right"
            )
            
          )
        ),
        column(
          9,
          box(
            title = tagList(icon("table"), "Read Expression Table"),
            solidHeader = TRUE,
            width = NULL,
            status = "primary",
            uiOutput("eSEMRawTable")
          ),
          tabBox(
            title = "",
            width = NULL,
            tabPanel(
              title = tagList(icon("chart-column"), "Activity"),
              tabsetPanel(
                tabPanel(
                  title = tagList(icon("square-poll-vertical"), "Raw result"),
                  uiOutput("ActivityRawTable"),
                  uiOutput("ActivityRawHM")
                ),
                tabPanel(
                  title = tagList(icon("square-poll-vertical"), "Mean Center"),
                  uiOutput("MeanCenterTable"),
                  uiOutput("MeanCenterHM")
                ),
                tabPanel(
                  title = tagList(icon("square-poll-vertical"), "Z score"),
                  uiOutput("zscoreTable"),
                  uiOutput("zscoreHM")
                )
              )
            ),
            tabPanel(
              title = tagList(icon("chart-column"), "Affinity"),
              uiOutput("Affnity")
            )
          )
        )
        )
      )
    )
    
    
    
  )

)

