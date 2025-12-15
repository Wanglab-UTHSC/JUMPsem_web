# ui-jumpsem.R

library(shiny)
library(shinyjs)

dashboardPage(
  dashboardHeader(title = "JUMPsem",
                  tags$li(class = "dropdown", actionLink(inputId='helpdoc',
                                                           icon = icon("circle-info"),
                                                           #style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                                           label="Documentation/Guide", value = "Open popup",
                                                           class = "my_class",
                                                           onclick ="window.open('https://github.com/Wanglab-UTHSC/JUMPsem_web','_blank')"))
                  ),
  dashboardSidebar(
    disable = T,
    sidebarMenu(
      id = "sider",
      menuItem(text = "JUMPsem",
               tabName = "jumpsem",
               icon = icon("dashboard")
      )
    )

  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      includeCSS("theme.css"),
      tags$style(HTML("
        .my_class {
          font-weight: bold;
          color: #fff;
        }
        .main-header .my_class {
          background-color: transparent !important;
          border: none !important;
          color: #fff !important;
          box-shadow: none !important;
          padding: 15px 10px;
        }
        .main-header .my_class:hover,
        .main-header .my_class:focus,
        .main-header .my_class:active {
          background-color: #008ccf !important;
          color: #fff !important;
          transform: none !important;
        }
      "))
    ),
    tabItem(
      tabName = "jumpsem",
      fluidPage(
        fluidRow(
          column(
          3,
          fluidRow(
            tabBox(
              title = "",
              id = "jumpsemdataSource",
              width = NULL,
              tabPanel(
                title = "Sample Data",
                icon = icon("folder-open"),
                tags$p("Upload your own sample"),
                radioButtons(
                  "jumpsemdataType",
                  "Choose your data type",
                  choices = c("Phosphorylation" = "psp",
                              "Ubiquitination" = "ubi",
                              "Acetylation" = "ace")
                ),
                fileInput(
                  "uploadjumpsemSubstrateData",
                  "Upload Substrate Data",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"),
                  buttonLabel = "Upload...",
                  placeholder = "No file has been uploaded."
                ),
                fileInput(
                  "uploadWholeProteomicsData",
                  "Upload Whole Proteome Data(optional)",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"),
                  buttonLabel = "Upload...",
                  placeholder = "No file has been uploaded."
                ),
                helpText("If you want to normalize the raw data, please upload normalization data file."),
                footer = helpText("Text file in .txt/.csv format."),
                #shinyDirButton('folder', 'Select a folder', 'Please select a folder for output result tables', FALSE)
              ),
              tabPanel(
                "Example",
                icon = icon("book"),
                selectInput(
                  "jumpsemSampleData",
                  "Select Sample Data",
                  choices = c(
                    "Phosphorylation(mouse)" = "pspSample",
                    "Ubiquination(human)" = "ubiSample",
                    "Acetylation(human)" = "aceSample"
                  )),
                tags$p("Example JUMPsem raw data for illustration, containing three data types to show."),
                do.call(actionBttn, c(
                  list(
                    inputId = "jumpsemDataSampleRun",
                    label = "Run Example Data",
                    icon = icon("play")
                  ),
                  actionBttnParams
                ))
              )
            )
          ),
          fluidRow(
            class = "rowhide",
            box(
              title = tagList(icon("info-circle"), "Group Information(optional)"),
              solidHeader = TRUE,
              width = NULL,
              status = "primary",
              fileInput(
                "uploadjumpsemGroup",
                "Upload Group Information",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                buttonLabel = "Upload...",
                placeholder = "No file has been uploaded."
              )
          )
          ),
          fluidRow(
            class = "rowhide",
            box(
              title = tagList(icon("tags"), "Parameters"),
              solidHeader = TRUE,
              width = NULL,
              status = 'primary',
              selectInput(inputId = "jumpsemorg",label = "Substrate Species",
                          choices = c("Human" = "human",
                                      "Mouse" = "mouse",
                                      "Rat" = "rat")),
              selectInput(inputId = "enzyme_org",label = "Enzyme Species(only phospho)",
                          choices = c("Human" = "human",
                                      "Mouse" = "mouse",
                                      "Rat" = "rat"),
                          multiple = T),
              do.call(actionBttn, c(
                list(
                  inputId = "runjumpsem",
                  label = "Run JUMPsem Analysis",
                  icon = icon("play")),
                actionBttnParams
              )
              )
            )
          ),
          fluidRow(
            class = "rowhide",
            box(
              title = tagList(icon("option-vertical",lib = "glyphicon"), "Advanced Parameters"),
              solidHeader = TRUE,
              status = 'primary',
              width = NULL,
              collapsible = T,
              collapsed = T,
              # selectInput(inputId = "enzymeSpe",label = "Enzyme Organism",
              #             choices = c("Human" = "human",
              #                         "Mouse" = "mouse",
              #                         "Rat" = "rat"),
              #             multiple = T
              #             ),
              # bsTooltip(id = "enzymeSpe",title = "Multiple choices available. Default is set the same as substrate organism"),
              fileInput(
                "uploadmotif",
                "Upload motif data",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),
                buttonLabel = "Upload...",
                placeholder = "No file has been uploaded."
              ),
              # bsTooltip(
              #   id = "jumpsemcoroff",
              #   title = "Set up correlation cutoff value 0-1 to remove high collinear variables.",
              #   placement = "right"
              # ),
              numericInput(inputId = "jumpsemkmooff", label = "KMO Cutoff",
                           value = 0,
                           min = 0,
                           max = 1,
                           step = 0.01,
                           width = NULL),
              # bsTooltip(
              #   id = "jumpsemkmooff",
              #   title = "Set up KMO cutoff value 0-1. Default is 0.",
              #   placement = "right"
              # ),
              radioButtons(
                inputId = "jumpsemlog2",
                label = "Need substrate data log 2 transformation?",
                choices = c("Yes",
                            "No"),
                selected = "No"
              ),
              radioButtons(
                inputId = "jumpsemWholelog2",
                label = "Need whole proteome data log 2 transformation?",
                choices = c("Yes",
                            "No"),
                selected = "No"
              )
#               bsTooltip(
#                 id = "jumpsemlog2",
#                 title = "Need program to do log2 transforming of the input
# file or not."
#               )
              
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
            uiOutput("jumpsemRawTable")
          ),
          tabBox(
            title = "",
            width = NULL,
            tabPanel(
              title = tagList(icon("square-poll-vertical"), "Activity"),
              uiOutput("ActivityRawTable"),
              uiOutput("ActivityRawHM")
            ),
            tabPanel(
              title = tagList(icon("square-poll-vertical"), "Evaluations"),
              uiOutput("evaluationsTable")
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
