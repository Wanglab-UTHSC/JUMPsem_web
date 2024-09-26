
# options(repos = BiocManager::repositories())
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install()

source(file = "global.R",
       local = TRUE,
       encoding = "UTF-8")

shinyServer(function(input,output,session){
  options(shiny.maxRequestSize = 500 * (1024 ^ 2))
  
  
  
  variables = reactiveValues(
    #KSEM
    jumpsemRaw = data.frame(),
    jumpsemnorm = data.frame(),
  )
  
  source(file = "JUMPsem.R",
         local = TRUE,
         encoding = "UTF-8")


})







