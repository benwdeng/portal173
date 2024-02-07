###################################################################################################
#
# Portal 173 - Overview: Create new matter
#
###################################################################################################

change_matter_status_ui <- function(ns){
  fluidRow(
    bsModal(
      id = ns("change_matter_status_pop_up"),          # Name of the modal 
      title = "Change matter status",                  # Title of the modal
      trigger = "change_matter_status",                   # The button that triggers the modal
      
      fluidPage(
        column(width = 8, offset = 2,
        uiOutput(ns("change_matter_status_details_ui")),
        uiOutput(ns("change_matter_status_ui")),
        uiOutput(ns("change_matter_status_submit_log_ui"))
        )
      )
    )
  )  
}


change_matter_status_server <- function(session, input, output, change_matter_status, values){
  observeEvent(change_matter_status(), {
    s <- input$all_matters_rows_selected
    if(!is.null(s)){
      toggleModal(session, modalId = "change_matter_status_pop_up", toggle = "toggle")
    } else {
      shinyalert(paste0("No matters selected"), type = "warning")
    }
  })
  
  output$change_matter_status_details_ui <- renderUI({
    s <- input$all_matters_rows_selected
    fluidRow(
      h6(
        paste0("Changing status for matter ID ", filtereddataOverview()[s, matter_id]), 
        align = "right"
      )
    )
  })
  
  #### START - Information to show ----------------------------------
  output$change_matter_status_ui <- renderUI({
    s <- input$all_matters_rows_selected
    # flog.warn(s)
    fluidRow(
      selectizeInput(
        inputId = session$ns("change_matter_status"),
        label = "Status: *",
        choices = status_choices,
        selected = filtereddataOverview()[s, status_of_matter],
        multiple = TRUE,
        options = list(maxItems = 1,
                       placeholder = 'Select the matter status' # Placeholder text
                       )
      )
    )
  })
  #### END - Information to show ----------------------------------
  
  output$change_matter_status_submit_log_ui <- renderUI({
    fluidRow(
      actionButton(
        inputId = session$ns("change_matter_status_submit"), 
        label = "Change matter status"
      )
    )
  })
  
  observeEvent(
    input$change_matter_status_submit, 
    ignoreInit = TRUE, 
    {
      s <- input$all_matters_rows_selected
      # flog.warn(s)
      status_entry <- filtereddataOverview()[
        s, 
        c("status_of_matter", "last_modified_date") := list(
          input$change_matter_status, Sys.time()
        )
      ]
    
      # Combine with table and save in path
      adjusted_fact_matter <- rbind(values$fact_matter()[unique_id != status_entry[, unique_id]], status_entry)
      saveRDS(object = adjusted_fact_matter, file = path_file_fact_matter)
      
      # Close the modal -------------------------------------------------
      toggleModal(session, modalId = "change_matter_status_pop_up", toggle = "close")
      shinyalert("Matter status changed!", type = "success")
    }
  )
}