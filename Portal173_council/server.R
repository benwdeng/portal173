###################################################################################################
#
# Portal 173 - Server file
#
###################################################################################################

# Global must be called in server, but does not need to also be called in ui.R 
# You can run the app by clicking 'Run app' or running `runApp('app')` 

source("global.R")

server <- function(input, output, session){

  # Grab data
  dataStorage <- reactiveValues(
    fact_matter = reactiveFileReader(intervalMillis = 1000, session = NULL, readFunc = readRDS, filePath = path_file_fact_matter),
    fact_forms = reactiveFileReader(intervalMillis = 1000, session = NULL, readFunc = readRDS, filePath = path_file_fact_forms),
    fact_documents = reactiveFileReader(intervalMillis = 1000, session = NULL, readFunc = readRDS, filePath = path_file_fact_documents),
    fact_council = reactiveFileReader(intervalMillis = 1000, session = NULL, readFunc = readRDS, filePath = path_file_fact_council)
  )
  
  # Server for the tabs
  mod_overview_server(id = "overview_tab", values = dataStorage)
  mod_portal173_server(id = "portal173_tab", values = dataStorage)
  
  
  
  
}
