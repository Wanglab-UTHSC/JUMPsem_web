
# options(repos = BiocManager::repositories())
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install()

source(file = "global.R",
       local = TRUE,
       encoding = "UTF-8")

shinyServer(function(input,output,session){
  options(shiny.maxRequestSize = 500 * (1024 ^ 2))

  # Remove generated text outputs when the Shiny session stops
  onStop(clean_generated_files)
  
  
  
  variables = reactiveValues(
    #KSEM
    jumpsemRaw = data.frame(),
    jumpsemnorm = data.frame(),
  )
  
  source(file = "JUMPsem.R",
         local = TRUE,
         encoding = "UTF-8")


})






