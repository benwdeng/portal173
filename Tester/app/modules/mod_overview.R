###################################################################################################
#
# Portal 173 - About Us module
#
###################################################################################################

mod_overview_ui <- function(id){
  # Defining namespace
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      h1("Overview"),
      align = "center"
    ),
    br(),
    uiOutput(ns("uploadUI")),
    tags$button(
      id = "download_button_js",
      style = "color: white; width: 100%;",
      type = "button",
      class = "btn btn-primary btn-lg",
      "download"
    ),
    bsPopover(
      id = "download_button_js", "hello", placement = "bottom", trigger = "hover",
      options = NULL
    ),
    # fileInput("file1", "Choose CSV File",
    #     multiple = FALSE
    # ),
    tags$input(
      id = "file1",
      style = "color: white; width: 100%;",
      type = "file",
      class = "btn btn-primary btn-lg"
    ),
    bsTooltip(
      id = "file1", "hello", placement = "bottom", trigger = "hover",
      options = NULL
    ),
    tags$button(
      id = "filelist_button_js",
      style = "color: white; width: 100%;",
      type = "button",
      class = "btn btn-primary btn-lg",
      "file list"
    ),
    bsTooltip(
      id = "filelist_button_js", 
      title = "Here is some text with your instructions"
    ),
    tags$iframe(
      id = "my_iframe",
      style="display:none;"
    ),
    actionButton(
      inputId = ns("showList"), 
      label = "Show List"
    )
  )
  
}

# Module Server -----------------------------------------------------------
mod_overview_server <- function(id){
  
  moduleServer(
    id,
    function(input, output, session){
      output$uploadUI <- renderUI({
        actionButton(session$ns("upload"), "Upload Image")
      })
      
      observeEvent(input$upload, {
        browser()
        showLog()
        logjs("App started")
        # js$uploadImage("C:/GIT/portal173/TOPH/app/www/documents/MA0003-Certificate Title Example name 3.jpg")
        shinyalert(text = paste0(input$testingstorage), type = "success")
        #s$upload_file("C:/GIT/portal173/TOPH/app/www/documents/MA0003-Certificate Title Example name 3.jpg", "up")
      })
      
      
      # download a file
      # output$downloadUI <- renderUI({
      #   actionButton(session$ns("download"), "Download Image")
      # })
      
      observeEvent(input$download_button_js, {
        browser()
        shinyalert(text = "downloading", type = "success")
      })
      
      observeEvent(input$showList, {
        browser()
        click("filelist_button_js", asis = TRUE)
        shinyalert(text = "Show List", type = "success")
      })
      
      observeEvent(input$listOfFiles, { shinyalert(text = paste(input$listOfFiles), type = "success") })
      
    }
  )
      
}