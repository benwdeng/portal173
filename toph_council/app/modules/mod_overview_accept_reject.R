###################################################################################################
#
# Portal 173 - Overview: Create new matter
#
###################################################################################################

accept_reject_ui <- function(ns) {
  fluidRow(
    bsModal(
      id = ns("accept_reject_pop_up"),  # Name of the modal 
      title = "Accept / Reject",        # Title of the modal
      trigger = "accept_reject",        # The button that triggers the modal
      size = "large",                   # Size of the modal
      fluidPage(
        uiOutput(ns("accept_reject_details_ui")),
        fluidRow(
          column(width = 6,
                 selectInput(ns("accept_reject_status"), "Decision", choices = c("Accept", "Reject")),
                 textAreaInput(ns("accept_reject_comment"), "Comment (Optional)", "")
          ),
          column(width = 6,
                 fileInput(ns("accept_reject_document"), "Upload Document", multiple = FALSE, accept = c(".pdf", ".doc", ".docx")),
                 actionButton(ns("accept_reject_submit"), "Submit Decision", class = "btn-primary")
          )
        )
      )
    )
  )  
}




accept_reject_server <- function(session, input, output, accept_reject, values) {
  observeEvent(accept_reject(), {
    s <- input$all_matters_tab1_rows_selected
    if (!is.null(s)) {
      toggleModal(session, modalId = "accept_reject_pop_up", toggle = "toggle")
    } else {
      shinyalert("No matters selected", type = "warning")
    }
  })
  
  output$accept_reject_details_ui <- renderUI({
    s <- input$all_matters_tab1_rows_selected
    if (!is.null(s)) {
      fluidRow(
        h6(paste0("Decision for Application ID: ", filtereddataOverview_tab1()[s, matter_id]), align = "right")
      )
    }
  })
  
  observeEvent(input$accept_reject_submit, {
    s <- input$all_matters_tab1_rows_selected
    if (!is.null(s)) {
      # Logic to handle the decision and comment
      decision <- input[[session$ns("accept_reject_status")]]
      comment <- input[[session$ns("accept_reject_comment")]]
      
      # Update the database or application state based on the decision and comment
      # This is where you integrate with your data handling logic
      
      # Example: Print to console (replace with your logic)
      print(paste("Decision:", decision, "- Comment:", comment))
      
      # Close the modal
      toggleModal(session, modalId = "accept_reject_pop_up", toggle = "close")
      
      # Display confirmation
      shinyalert("Decision submitted successfully!", type = "success")
    }
  }, ignoreInit = TRUE)
}
