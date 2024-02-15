###################################################################################################
#
# Portal 173 - Overview: Create new matter
#
###################################################################################################

view_edit_matter_ui <- function(ns){
  fluidRow(
    bsModal(
      id = ns("view_edit_matter_pop_up"),          # Name of the modal 
      title = "View/Edit matter",                  # Title of the modal
      trigger = "view_edit_matter",                   # The button that triggers the modal
      
      fluidPage(
        column(
          width = 8, offset = 2,
          fluidRow(
            h6("* denotes required field", align = "right"),
          ),
          uiOutput(ns("view_edit_information_details_ui")),
          uiOutput(ns("view_edit_matter_submit_log_ui"))
        )
      )
    )
  )  
}


view_edit_matter_server <- function(session, input, output, view_edit_matter, values){
  dataPostCodeMapping <- as.data.table(read.csv("inputs/australian_postcodes.csv"))
  dataCouncilInfo <- data.table(read.csv(path_input_council_info, fileEncoding = 'UTF-8-BOM'))
  
  observeEvent(view_edit_matter(), {
    s <- input$all_matters_rows_selected
    if(!is.null(s)){
      toggleModal(session, modalId = "view_edit_matter_pop_up", toggle = "toggle")
    } else {
      shinyalert(paste0("No matters selected"), type = "warning")
    }
  })
  
  #### START - Information to show ----------------------------------
  output$view_edit_information_details_ui <- renderUI({
    s <- input$all_matters_rows_selected
    selected_matter_id_edit <- filtereddataOverview()[s, matter_id]
    fluidRow(
      selectizeInput(
        inputId = session$ns("view_edit_type"),
        label = "Service required: *",
        choices = type_of_agreement,
        selected = filtereddataOverview()[matter_id %in% selected_matter_id_edit, matter_type],
        multiple = TRUE,
        width = "100%",
        options = list(maxItems = 1,
                       placeholder = 'Select the type of agreement' # Placeholder text
        )
      ),
      
      textAreaInput(
        inputId = session$ns("view_edit_client_name"), 
        label = "Contact name: *", 
        placeholder = "Enter the client's full name",
        width = "100%",
        value = filtereddataOverview()[matter_id %in% selected_matter_id_edit, client_name]
      ),
      
      selectizeInput(
        inputId = session$ns("view_edit_client_role"),
        label = "Contact name:",
        choices = client_roles,
        selected = filtereddataOverview()[matter_id %in% selected_matter_id_edit, client_role],
        multiple = TRUE,
        width = "100%",
        options = list(maxItems = 1,
                       placeholder = "Select the contact's role" # Placeholder text
        )
      ),
      
      textAreaInput(
        inputId = session$ns("view_edit_client_postal_address"), 
        label = "Contact postal address: ", 
        placeholder = "Enter contact postal address",
        width = "100%",
        value = filtereddataOverview()[matter_id %in% selected_matter_id_edit, client_postal_address]
      ),
      
      textAreaInput(
        inputId = session$ns("view_edit_contact_number"), 
        label = "Client contact number: ", 
        placeholder = "Enter client contact number",
        width = "100%",
        value = filtereddataOverview()[matter_id %in% selected_matter_id_edit, client_contact_number]
      ),
      
      textAreaInput(
        inputId = session$ns("view_edit_email_address"), 
        label = "Client email address ", 
        placeholder = "Enter client email address",
        width = "100%",
        value = filtereddataOverview()[matter_id %in% selected_matter_id_edit, client_email]
      ),
      
      
      textAreaInput(
        inputId = session$ns("view_edit_subject_land_address"), 
        label = "Subject land address: *", 
        placeholder = "Enter the address of the subject land",
        width = "100%",
        value = filtereddataOverview()[matter_id %in% selected_matter_id_edit, address]
      ),
      
      selectizeInput(
        inputId = session$ns("view_edit_address_postcode"),
        label = "Postcode: *",
        choices = NULL,
        selected = filtereddataOverview()[matter_id %in% selected_matter_id_edit, address_postcode],
        multiple = TRUE,
        width = "100%",
        options = list(maxItems = 1,
                       placeholder = 'Postcode for subject land' # Placeholder text
        )
      ),
      
      selectizeInput(
        inputId = session$ns("view_edit_council_name"), 
        label = "Council: *", 
        choices = dataCouncilInfo[, unique(Council)],
        multiple = FALSE,
        selected = filtereddataOverview()[matter_id %in% selected_matter_id_edit, council_name],
        width = "100%",
        options = list(placeholder = 'Select the relevant council' # Placeholder text
        ),
      ),

      
      selectizeInput(
        inputId = session$ns("view_edit_status"),
        label = "Status: *",
        choices = status_choices,
        selected = filtereddataOverview()[matter_id %in% selected_matter_id_edit, status_of_matter],
        multiple = TRUE,
        width = "100%",
        options = list(maxItems = 1,
                       placeholder = 'Select the action status' # Placeholder text
                       )
      ),

      dateInput(
        inputId = session$ns("view_edit_permit_expiry_date"), 
        label = "Permit expiry date:", 
        value = coalesceNA(filtereddataOverview()[matter_id %in% selected_matter_id_edit, permit_expiry_date], as.Date(x = integer(0))),
        format = "d M yyyy",
        startview = "month",
        width = "100%"
      )
    )
  })
  #### END - Information to show ----------------------------------
  
  # Updating list of postcodes when subject address exist
  observeEvent(input$view_edit_subject_land_address, {
    s <- input$all_matters_rows_selected
    selected_matter_id_edit <- filtereddataOverview()[s, matter_id]
    if(isTRUE(length(input$view_edit_address_postcode) == 0)){
      updateSelectizeInput(
        session,
        inputId = 'view_edit_address_postcode',
        choices = dataPostCodeMapping[, sprintf("%04d", unique(postcode))],
        selected = filtereddataOverview()[matter_id %in% selected_matter_id_edit, address_postcode],
        server = TRUE
      )
    }
  })

  
  output$view_edit_matter_submit_log_ui <- renderUI({
    fluidRow(
      actionButton(
        inputId = session$ns("edit_matter_submit"), 
        label = "Save changes"
      )
    )
  })
  
  observeEvent(
    input$edit_matter_submit, 
    ignoreInit = TRUE, 
    {
      s <- input$all_matters_rows_selected
      selected_matter_id_edit <- filtereddataOverview()[s, matter_id]
      
      entry <- data.table(
        unique_id = filtereddataOverview()[matter_id %in% selected_matter_id_edit, unique_id],
        matter_id = selected_matter_id_edit,
        address = input$view_edit_subject_land_address,
        address_postcode = ifelse(is.null(input$view_edit_address_postcode), as.character(NA), input$view_edit_address_postcode),
        council_name = input$view_edit_council_name,
        client_name = input$view_edit_client_name,
        client_role = input$view_edit_client_role,
        client_postal_address = input$view_edit_client_postal_address,
        client_contact_number = input$view_edit_contact_number,
        client_email = input$view_edit_email_address,
        created_date = filtereddataOverview()[matter_id %in% selected_matter_id_edit, created_date],
        matter_type = coalesceNULL(input$view_edit_type, ""),
        permit_expiry_date = input$view_edit_permit_expiry_date,
        status_of_matter = coalesceNULL(input$view_edit_status, ""),
        last_modified_date = Sys.time(),
        deleted_flag = filtereddataOverview()[matter_id %in% selected_matter_id_edit, deleted_flag],
        firestore_id = filtereddataOverview()[matter_id %in% selected_matter_id_edit, firestore_id]
      )
      
      # Get Firestore ID
      firestore_id_entry <- entry[, firestore_id]
      dataJSON <- convertDTtoJSON(entry)
      
      firestore_response <- sendJSONToFirestore(
        api_call = paste0(db_endpoint_api, db_endpoint_dataOverview, "/", firestore_id_entry),
        access_token = session$userData$current_user()$stsTokenManager$accessToken,
        data = dataJSON,
        replacement = TRUE
      )
      
      # Combine with table and save in path
      adjusted_fact_matter <- rbind(values$fact_matter()[matter_id != selected_matter_id_edit], entry)
      saveRDS(object = adjusted_fact_matter, file = path_file_fact_matter)
      
      # Close the modal -------------------------------------------------
      toggleModal(session, modalId = "view_edit_matter_pop_up", toggle = "close")
      shinyalert(paste0("Matter ", entry[, matter_id]," updated!"), type = "success")
    }
  )
}