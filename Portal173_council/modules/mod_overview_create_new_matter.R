###################################################################################################
#
# Portal 173 - Overview: Create new matter
#
###################################################################################################

create_new_matter_ui <- function(ns){
  fluidRow(
    bsModal(
      id = ns("create_new_matter_pop_up"),          # Name of the modal 
      title = "Create new matter",                  # Title of the modal
      trigger = "create_matter",                   # The button that triggers the modal
      
      fluidPage(
        column(width = 8, offset = 2,
        uiOutput(ns("create_new_id_details_ui")),
        uiOutput(ns("create_information_details_ui")),
        uiOutput(ns("create_matter_submit_log_ui"))
        )
      )
    )
  )  
}


create_new_matter_server <- function(session, input, output, create_matter, values){
  dataPostCodeMapping <- as.data.table(read.csv("inputs/australian_postcodes.csv"))
  
  observeEvent(create_matter(), {
    toggleModal(session, modalId = "create_new_matter_pop_up", toggle = "toggle")
  })
  
  output$create_new_id_details_ui <- renderUI({
    fluidRow(
      h6("* denotes required field", align = "right"),
    )
  })
  
  #### START - Information to show ----------------------------------
  output$create_information_details_ui <- renderUI({
    fluidRow(
      textAreaInput(
        inputId = session$ns("create_matter_subject_land_address"), 
        label = "Subject land address: *", 
        placeholder = "Enter the address of the subject land",
        width = "100%"
      ),
      selectizeInput(
        inputId = session$ns("create_matter_address_postcode"),
        label = "Postcode: *",
        choices = NULL,
        selected = "",
        multiple = TRUE,
        width = "100%",
        options = list(maxItems = 1,
                       placeholder = 'Postcode for subject land' # Placeholder text
        )
      ),
      textAreaInput(
        inputId = session$ns("create_matter_client_name"), 
        label = "Client name: *", 
        placeholder = "Enter the client's full name",
        width = "100%"
      ),
      
      textAreaInput(
        inputId = session$ns("create_matter_contact_number"), 
        label = "Client contact number: ", 
        placeholder = "Enter client contact number",
        width = "100%"
      ),
      
      textAreaInput(
        inputId = session$ns("create_matter_email_address"), 
        label = "Client email address ", 
        placeholder = "Enter client email address",
        width = "100%"
      ),
      
      selectizeInput(
        inputId = session$ns("create_matter_status"),
        label = "Status: *",
        choices = status_choices,
        selected = "",
        multiple = TRUE,
        width = "100%",
        options = list(maxItems = 1,
                       placeholder = 'Select the action status' # Placeholder text
                       )
      ),
      selectizeInput(
        inputId = session$ns("create_matter_type"),
        label = "Type of matter: *",
        choices = type_of_agreement,
        selected = "",
        multiple = TRUE,
        width = "100%",
        options = list(maxItems = 1,
                       placeholder = 'Select the type of agreement' # Placeholder text
        )
      ),
      dateInput(
        inputId = session$ns("create_matter_permit_expiry_date"), 
        label = "Permit expiry date:", 
        value = NULL,
        format = "yyyy-mm-dd",
        startview = "month",
        width = "100%"
      )
    )
  })
  #### END - Information to show ----------------------------------
  
  # Updating list of postcodes when subject address exist
  observeEvent(input$create_matter_subject_land_address, {
    if(isTRUE(length(input$create_matter_address_postcode) == 0)){
      updateSelectizeInput(
        session,
        inputId = 'create_matter_address_postcode',
        choices = dataPostCodeMapping[, sprintf("%04d", unique(postcode))],
        selected = "",
        server = TRUE
      )
    }
  })

  
  output$create_matter_submit_log_ui <- renderUI({
    fluidRow(
      actionButton(
        inputId = session$ns("create_matter_submit"), 
        label = "Create matter"
      )
    )
  })
  
  observeEvent(
    input$create_matter_submit, 
    ignoreInit = TRUE, 
    {
      if(nrow(values$fact_matter()) > 0){
        new_entry <- values$fact_matter()[, max(unique_id)] + 1
      } else {
        new_entry <- 1
      }
      
      entry <- data.table(
        unique_id = new_entry,
        matter_id = paste0("MA", sprintf("%04d", new_entry)),
        address = input$create_matter_subject_land_address,
        address_postcode = ifelse(is.null(input$create_matter_address_postcode), as.character(NA), input$create_matter_address_postcode),
        client_name = input$create_matter_client_name,
        client_contact_number = input$create_matter_contact_number,
        client_email = input$create_matter_email_address,
        matter_created_date = Sys.time(),
        matter_type = input$create_matter_type,
        permit_expiry_date = input$create_matter_permit_expiry_date,
        status_of_matter = input$create_matter_status,
        last_modified_date = Sys.time()
      )
      
      # Combine with table and save in path
      adjusted_fact_matter <- rbind(values$fact_matter(), entry)
      saveRDS(object = adjusted_fact_matter, file = path_file_fact_matter)
      
      # Close the modal -------------------------------------------------
      toggleModal(session, modalId = "create_new_matter_pop_up", toggle = "close")
      shinyalert(paste0("Matter ", entry[, matter_id]," created!"), type = "success")
      
      # Clear all fields after creating a new matter
      updateTextAreaInput(session, inputId = "create_matter_subject_land_address", value = "")
      updateSelectizeInput(session, inputId = "create_matter_address_postcode", selected = "") 
      updateTextAreaInput(session, inputId = "create_matter_client_name", value = "")
      updateTextAreaInput(session, inputId = "create_matter_contact_number", value = "")
      updateTextAreaInput(session, inputId = "create_matter_email_address", value = "")
      updateSelectizeInput(session, inputId = "create_matter_status", selected = "") 
      updateSelectizeInput(session, inputId = "create_matter_type", selected = "") 
      updateDateInput(session, inputId = "create_matter_permit_expiry_date", value = NULL)
    }
  )
}