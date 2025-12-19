library(shiny)

.libPaths( c( "./lib" , .libPaths() ) )
#install.packages("tidyverse", lib="./lib",repos="https://cran.rstudio.com/")
#options(shiny.fullstacktrace=TRUE)

clean_generated_files <- function() {
  files <- c(
    Sys.glob("Activity*.txt"),
    Sys.glob("Affinity*.txt"),
    Sys.glob("Eval_table*.txt")
  )
  if (length(files)) {
    unlink(files, force = TRUE)
  }
}

# Clear any leftovers from prior runs on startup
clean_generated_files()

source(file = "server.R",
       local = TRUE,
       encoding = "UTF-8")

captureStackTraces <- function(expr) {
  withCallingHandlers(
    expr,
    error = function(e) {
      if (is.null(attr(e, "stack.trace", exact = TRUE))) {
        attr(e, "stack.trace") <- sys.calls()
        stop(e)
      }
    }
  )
}

#options(shiny.fullstacktrace=TRUE)
#options(shiny.trace = TRUE)
runApp(launch.browser=0, port=3979)
