###################################################################################################
#
# Portal 173 - Overview: Create new matter
#
###################################################################################################

mod_view_draft_ui <- function(ns){
  fluidRow(
    bsModal(
      id = ns("view_draft_pop_up"),          # Name of the modal 
      title = "Viewing document",                  # Title of the modal
      trigger = "view_draft",                   # The button that triggers the modal
      size = "large",
      fluidPage(
        uiOutput(ns("view_draft_area_ui"))
      )
    )
  )  
}


mod_view_draft_server <- function(session, input, output, view_draft, values){
  observeEvent(view_draft(), {
    toggleModal(session, modalId = "view_draft_pop_up", toggle = "toggle")
  })
  
  output$view_draft_area_ui <- renderUI({
    s = input$all_draft_forms_rows_selected
    if(!is.null(s)){
      entry <- values$fact_forms()[s]
      
      # Convert to html
      pandoc_convert(paste0(getwd(), "/", path_output_draft, entry[, form_id], " Section 173 Agreement draft.docx"), to="html", output = paste0(getwd(), "/", path_output_draft, entry[, form_id], " Section 173 Agreement draft.html"))
      
      includeHTML(paste0(getwd(), "/", path_output_draft, entry[, form_id], " Section 173 Agreement draft.html"))
      #shinyalert(paste0("Viewing ", file_to_view), type = "success")
    } else {
      shinyalert(paste0("No forms selected"), type = "warning")
    }
  })
  
}