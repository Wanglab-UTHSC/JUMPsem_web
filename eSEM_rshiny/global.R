#list of packages required
list.of.packages <- c("pacman","shiny","dplyr","eSEM","shinycssloaders","ggplot2","heatmaply","shinyBS","rgeos","shinyjs","scales","data.table","RColorBrewer","markdown","utils","shinyWidgets",
                      "tidyr","shinydashboard","plotly","gplots")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)


library(shiny)
#global.R
library(eSEM)
library(shinycssloaders)
library(shinyBS)
library(heatmaply)
library(data.table)
library(RColorBrewer)
library(markdown)
library(utils)
library(shinyWidgets)
library(tidyr)
library(shinydashboard)
library(plotly)
library(dplyr)
library(gplots)
library(ggplot2)
library(DT)

actionBttnParams <- list(
  size = "sm",
  color = "primary",
  style = "fill",
  block = TRUE
)