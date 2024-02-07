approve_matter_ui <- function(ns){
  fluidRow(
    bsModal(
      id = ns("approve_matter"),          # Name of the modal 
      title = "Approve matter",                  # Title of the modal
      trigger = "approve_matter_button",                   # The button that triggers the modal
      
      fluidPage(
        column(width = 8, offset = 2,
               uiOutput(ns("confirm_approved_ui")),
               uiOutput(ns("confirm_approve_matter_button_ui"))
        )
      )
    )
  )  
}

approve_matter_server <- function(session, input, output, confirm_approve_matter, values){
  
  observeEvent(confirm_approve_matter(),ignoreInit = T, {
      toggleModal(session, modalId = "approve_matter", toggle = "toggle")
  })

  
  output$confirm_approved_ui <- renderUI({
    s <- input$all_matters_rows_selected
    fluidPage(
      fluidRow(
        paste0("Are you sure you want to approve ",filtereddataOverview()[s, matter_id]," for address ",filtereddataOverview()[s, address],"?"),
        textInput(inputId = session$ns("approve_council_comment"),label = "Write any comments here:",value = "", width = "100%"),
        align = "center"
      )
    )
    
  })
  
  output$confirm_approve_matter_button_ui <- renderUI({
    fluidRow(
      actionButton(inputId = session$ns("confirm_approve_matter_button"), "Confirm")
    )
  })
  
  #Modal pop up when council user clicks approve
  observeEvent(session$ns(input$confirm_approve_matter_button),ignoreInit = TRUE,{
    s <- input$all_matters_rows_selected
    if(input$confirm_approve_matter_button > 0){ #to prevent triggering on render
      
      #Edit fact matter table to update status -------------------------------------------------
      status_entry <- filtereddataOverview()[
        s,
        c("status_of_matter", "last_modified_date") := list(
          "6 council approved", Sys.time()
        )
      ][,-c("LGA_NAME_2011","POSTCODE"),with = FALSE]
      
      adjusted_fact_matter <- rbind(values$fact_matter()[unique_id != status_entry[, unique_id]], status_entry)
      #saveRDS(object = adjusted_fact_matter, file = path_file_fact_matter)
      
      #Edit fact council table to update decision-------------------------------------------------
      approval_entry <- data.table(
        matter_id = filtereddataOverview()[s,matter_id],
        council_decision = "approved",
        comment = input$approve_council_comment,
        timestamp = Sys.time())
      
      
      adjusted_fact_council <- rbind(values$fact_council(),approval_entry)
      
      saveRDS(object = adjusted_fact_council, file = path_file_fact_council)
      
      #Close the modal -------------------------------------------------  
      toggleModal(session, modalId = "approve_matter", toggle = "close")
      shinyalert(paste0("Matter ", filtereddataOverview()[s, matter_id]," approved!"), type = "success")
    }

  })
}