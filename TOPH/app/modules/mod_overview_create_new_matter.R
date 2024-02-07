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
  dataCouncilInfo <- data.table(read.csv(path_input_council_info, fileEncoding = 'UTF-8-BOM'))
  
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
      selectizeInput(
        inputId = session$ns("create_matter_type"),
        label = "Service required: *",
        choices = type_of_agreement,
        selected = "",
        multiple = TRUE,
        width = "100%",
        options = list(maxItems = 1,
                       placeholder = 'Select the type of agreement' # Placeholder text
        )
      ),
      textAreaInput(
        inputId = session$ns("create_matter_client_name"), 
        label = "Contact name: *", 
        placeholder = "Enter the client's full name",
        width = "100%"
      ),
      
      selectizeInput(
        inputId = session$ns("create_matter_client_role"),
        label = "Contact role:",
        choices = client_roles,
        selected = "",
        multiple = TRUE,
        width = "100%",
        options = list(maxItems = 1,
                       placeholder = "Select the contact's role" # Placeholder text
        )
      ),
      
      textAreaInput(
        inputId = session$ns("create_matter_client_postal_address"), 
        label = "Contact postal address: ", 
        placeholder = "Enter contact postal address",
        width = "100%"
      ),
      
      textAreaInput(
        inputId = session$ns("create_matter_contact_number"), 
        label = "Contact number: ", 
        placeholder = "Enter contact number",
        width = "100%"
      ),
      
      textAreaInput(
        inputId = session$ns("create_matter_email_address"), 
        label = "Client email address ", 
        placeholder = "Enter client email address",
        width = "100%"
      ),
      
      textAreaInput(
        inputId = session$ns("create_matter_subject_land_address"), 
        label = "Subject land address: * (as on Planning Permit)", 
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
      
      selectizeInput(
        inputId = session$ns("create_matter_council_name"), 
        label = "Council: *", 
        choices = dataCouncilInfo[, unique(Council)],
        multiple = FALSE,
        selected = "",
        width = "100%",
        options = list(placeholder = 'Select the relevant council', # Placeholder text
                       onInitialize = I('function() { this.setValue(""); }')),
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

      dateInput(
        inputId = session$ns("create_matter_permit_expiry_date"), 
        label = "Permit expiry date:", 
        value = NULL,
        format = "d M yyyy",
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
      refreshAllTables(session, session$userData$current_user()$stsTokenManager$accessToken)
      if(nrow(values$fact_matter()) > 0){
        new_entry <- values$fact_matter()[, max(unique_id)] + 1
      } else {
        new_entry <- 1
      }
      
      entry <- data.table(
        unique_id = new_entry,
        matter_id = paste0("MA", sprintf("%04d", new_entry)),
        address = input$create_matter_subject_land_address,
        address_postcode = ifelse(is.null(input$create_matter_address_postcode), "", input$create_matter_address_postcode),
        council_name = input$create_matter_council_name,
        client_name = input$create_matter_client_name,
        client_role = ifelse(is.null(input$create_matter_client_role), "", input$create_matter_client_role),
        client_postal_address = input$create_matter_client_postal_address,
        client_contact_number = input$create_matter_contact_number,
        client_email = input$create_matter_email_address,
        created_date = Sys.time(),
        matter_type = input$create_matter_type,
        permit_expiry_date = input$create_matter_permit_expiry_date,
        status_of_matter = ifelse(is.null(input$create_matter_status), "", input$create_matter_status),
        last_modified_date = Sys.time(),
        deleted_flag = 0
      )
      
      dataJSON <- convertDTtoJSON(entry)
      
      firestore_response <- sendJSONToFirestore(
        api_call = paste0(db_endpoint_api, db_endpoint_dataOverview),
        access_token = session$userData$current_user()$stsTokenManager$accessToken,
        data = dataJSON
      )
      
      # Add firestore_id
      firestore_id_entry <- getFirebaseIDFromPath(fromJSON(content(firestore_response,"text"))$name)
      
      entry[, firestore_id := firestore_id_entry]
      
      # Combine with table and save in path
      adjusted_fact_matter <- rbind(values$fact_matter(), entry)
      saveRDS(object = adjusted_fact_matter, file = path_file_fact_matter)
      
      # Close the modal -------------------------------------------------
      toggleModal(session, modalId = "create_new_matter_pop_up", toggle = "close")
      
      if(firestore_response$status_code == 200){
        shinyalert(paste0("Matter ", entry[, matter_id]," created!"), type = "success")
      } else {
        shinyalert(paste0("Cannot send to server"), type = "warning")
      }
      
      
      # Clear all fields after creating a new matter
      updateTextAreaInput(session, inputId = "create_matter_subject_land_address", value = "")
      updateSelectizeInput(session, inputId = "create_matter_address_postcode", selected = "") 
      updateSelectizeInput(session, inputId = "create_matter_council_name", selected = "") 
      updateSelectizeInput(session, inputId = "create_matter_client_role", selected = "") 
      updateTextAreaInput(session, inputId = "create_matter_client_name", value = "")
      updateTextAreaInput(session, inputId = "create_matter_client_postal_address", value = "")
      updateTextAreaInput(session, inputId = "create_matter_contact_number", value = "")
      updateTextAreaInput(session, inputId = "create_matter_email_address", value = "")
      updateSelectizeInput(session, inputId = "create_matter_status", selected = "") 
      updateSelectizeInput(session, inputId = "create_matter_type", selected = "") 
      updateDateInput(session, inputId = "create_matter_permit_expiry_date", value = NULL)
    }
  )
}