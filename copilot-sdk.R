library(jsonlite)
source("logger.R")
source("ShinyModule.R")

inputFileName = "input_lowres_geese_74X.rds"
outputFileName = "output.rds"

if(file.exists("configuration.json")) {
  args <- read_json("configuration.json")
} else {
  args <- list()
}

#################################################################
########################### Arguments ###########################
# The data parameter will be added automatically if input data is available
# The name of the field in the vector must be exactly the same as in the shiny module signature
# Example:
# shinyModule <- function(input, output, session, username, password)
# The parameter must look like:
#    args[["username"]] = "any-username"
#    args[["password"]] = "any-password"

# Add your arguments of your r function here
args[["grid"]] <- 50000
args[["meth"]] <- "sf" #"rast", "fast" or "sf"

#################################################################
#################################################################

storeConfiguration <- function(configuration) {
  write_json(configuration, "configuration.json", auto_unbox = TRUE)
  cat("Stored configuration of shinyModule to configuration.json")
}

ui <- fluidPage(
  #mainPanel( #PLEASE DELETE!
    do.call(shinyModuleUserInterface, c("shinyModule", "shinyModule", args)),
    dataTableOutput("table"), #Is neccessary for storing result
    
    if(exists("shinyModuleConfiguration")) {
      actionButton("storeConfiguration", "Store current configuration")
    }
  )
#)

server <- function(input, output, session) {
  inputData <- NULL
  if(!is.null(inputFileName) && inputFileName != "" && file.exists(inputFileName)) {
    cat("Loading file from", inputFileName, "\n")
    inputData <- readRDS(file = inputFileName)
  } else {
    cat("Skip loading: no input File", "\n")
  }
  
  shinyModuleArgs <- c(shinyModule, "shinyModule", args)
  if (!is.null(inputData)) {
    shinyModuleArgs[["data"]] <- inputData
  }
  
  result <- tryCatch({
        do.call(callModule, shinyModuleArgs)
    },
    error = function(e) {
      print(paste("ERROR: ", e))
      stop(e) # re-throw the exception
    }
  )
  
  observeEvent(input$storeConfiguration, {
    cat("Start reading configuration from shinyModule")
    storeConfiguration(shinyModuleConfiguration("shinyModule", input))
  })
  
  output$table <- renderDataTable({
    if(!is.null(outputFileName) && outputFileName != "" && !is.null(result())) {
      cat("Storing file to", outputFileName, "\n")
      saveRDS(result(), file = outputFileName)
    } else {
      cat("Skip store result: no output File or result is missing", "\n")
    }
  })
}

shinyApp(ui, server)