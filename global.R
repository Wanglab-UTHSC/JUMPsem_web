
#global.R


#install missing ones
if (!require("pacman")) install.packages("pacman")
pacman::p_load("devtools","shiny","dplyr","shinycssloaders","ggplot2","heatmaply","shinyBS","shinyjs","scales","data.table","RColorBrewer","markdown","utils","shinyWidgets",
               "tidyr","shinydashboard","plotly","gplots","DT","tidyverse","lavaan","EFAtools","dplyr")
# pacman::p_load_current_gh("Wanglab-UTHSC/JUMPsem")


orgchoice <- c("Human" = "human",
               "Mouse" = "mouse",
               "Rat" = "rat")
dt_choice <- c("Phosphorylation" = "psp",
              "Ubiquitination" = "ubi",
              "Acetylation" = "ace")

actionBttnParams <- list(
  size = "sm",
  color = "primary",
  style = "fill",
  block = TRUE
)
