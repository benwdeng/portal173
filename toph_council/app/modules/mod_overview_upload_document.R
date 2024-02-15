###################################################################################################
#
# Portal 173 - Overview: Create new matter
#
###################################################################################################

update_document_ui <- function(ns){
  fluidRow(
    bsModal(
      id = ns("upload_document_pop_up"),          # Name of the modal 
      title = "Upload document",                  # Title of the modal
      trigger = "upload_document",                   # The button that triggers the modal
      
      fluidPage(
        column(
          width = 12, offset = 0,
          uiOutput(ns("document_details_ui")),
          DT::dataTableOutput(ns("table_document_list")),
          uiOutput(ns("document_buttons_ui")),
          br(),
          hr(),
          br(),
          uiOutput(ns("document_upload_ui")),
          uiOutput(ns("document_type_ui")),
          uiOutput(ns("document_upload_buttons_ui"))
        )
      )
    )
  )  
}


update_document_server <- function(session, input, output, upload_document, values, specific_matter_id){
  
  observeEvent(upload_document(), {
    toggleModal(session, modalId = "upload_document_pop_up", toggle = "toggle")
  })
  
  output$document_details_ui <- renderUI({
    fluidRow(
      h3(paste0("Document uploaded for ", specific_matter_id()), align = "left")
    )
  })
  
  output$table_document_list <- renderDataTable({
    sendMatterID <- specific_matter_id()
    session$sendCustomMessage("folderLocation", sendMatterID)
    values$fact_document()[
      matter_id == specific_matter_id() & deleted_flag == "0", # & filename %in% session$userData$relevantFiles()$filename,
      .("File name" = filename, 
        "Document" = document_type,
        "Last modified" = format(last_modified_date, "%Y-%b-%d, %I:%M:%S%p"))
    ]
  }, selection = 'single', options = list(scrollX = TRUE))
  
  output$document_buttons_ui <- renderUI({
    fluidRow(
      tags$button(
        id = "document_list_button_js",
        style = "width: 100%;",
        type = "button",
        class = "btn btn-default action-button shiny-bound-input",
        icon("sync"),
        "Refresh documents list"
      ),
      if(is.null(input$table_document_list_rows_selected)){
        actionButton(
          inputId = session$ns("nodoc_download_button"),
          label = "Download document", 
          width = "100%",
          icon = icon("download")
        )
      } else {
        actionButton(
          inputId = session$ns("download_document"), 
          label = "Download document", 
          width = "100%",
          icon = icon("download")
        )
      },
      actionButton(
        inputId = session$ns("delete_specific_document_button"), 
        label = "Delete document", 
        icon = icon("times-circle"),
        width = "100%"
      )
    )
  })
  
  # Area to delete specific item
  observeEvent(
    input$delete_specific_document_button, 
    {
      s = isolate(input$table_document_list_rows_selected)
      if(!is.null(s)){
        delete_unique_id <- values$fact_document()[
          matter_id == specific_matter_id() & deleted_flag == "0"
        ][s, unique_id]
        
        adjusted_fact_document <- values$fact_document()[unique_id == delete_unique_id, deleted_flag := "1"]
        saveRDS(object = adjusted_fact_document, file = path_file_fact_document)
        
        # Resent to include firestore_id
        status_entry <- adjusted_fact_document[unique_id == delete_unique_id]
        firestore_id_entry <- status_entry[, firestore_id]
        dataJSON <- convertDTtoJSON(status_entry)
        
        firestore_response <- sendJSONToFirestore(
          api_call = paste0(db_endpoint_api, db_endpoint_dataDocument, "/", firestore_id_entry),
          access_token = session$userData$current_user()$stsTokenManager$accessToken,
          data = dataJSON,
          replacement = TRUE
        )
        
        js$deleteFile(status_entry[, filename], status_entry[, matter_id])
        
        shinyalert(paste0("Document deleted"), type = "success")
      } else {
        shinyalert(paste0("No documents selected"), type = "warning")
      }
      
    }
  )
  
  #### START - What happens once user downloads the document ----
  observeEvent(input$nodoc_download_button, {
    shinyalert(paste0("No documents selected"), type = "warning")
  })
  
  observeEvent(input$download_document, {
    entry <- values$fact_document()[
      matter_id == specific_matter_id() & deleted_flag == "0" 
    ][input$table_document_list_rows_selected]
    
    # To fix: Images don't download
    js$downloadFile(entry[, filename], entry[, matter_id])
  })
  #### END - What happens once user downloads the document ----
  
  #### START - What happens once user upload the document ####
  output$document_upload_ui <- renderUI({
    fluidRow(
      h3(paste0("Upload documents for ", specific_matter_id()), align = "left"),
    )
  })
  
  output$document_type_ui <- renderUI({
    fluidRow(
      selectizeInput(
        inputId = session$ns("document_type_input"), 
        label = "What is the document you're uploading",
        choices = c("All", type_of_document),
        multiple = FALSE,
        selected = "All",
        width = "100%"
      )
    )
  })
  
  output$document_upload_buttons_ui <- renderUI({

    fluidRow(
      if(is.null(input$document_type_input) || input$document_type_input == "All"){
        actionButton(
          inputId = session$ns("dummy_upload_button"),
          label = "Upload document", 
          width = "100%",
          icon = icon("upload")
        )
      } else {
        ## Keeping this in case we want to copy formatting
        session$userData$matterIDDocument <- reactiveVal(NULL)
        session$userData$matterIDDocument(list(matter_id = specific_matter_id(),
                                               document_type = input$document_type_input))
        fileInput(
          inputId = session$ns("docUpload"),
          multiple = FALSE,
          label = NULL,
          buttonLabel = list(icon("upload"), "Upload"),
          width = "100%"
        )

        # tags$input(
        #   id = "uploadfile",
        #   style = "width:100%;",
        #   type = "file",
        #   class = "form-group shiny-input-container",
        #   icon = "upload"
        # )
      }
    )
  })
  
  observeEvent(input$dummy_upload_button, {
    shinyalert(paste0("Select document type before uploading"), type = "warning")
  })
  
  observeEvent(input$docUpload, {
    
    if(nrow(values$fact_document()[filename == input$docUpload$name]) > 0){
      # Update the data table
      status_entry <- data.table(
        unique_id = values$fact_document()[filename == input$docUpload$name, unique_id],
        matter_id = specific_matter_id(),
        document_type = input$document_type_input,
        filename = input$docUpload$name,
        date_uploaded = values$fact_document()[filename == input$docUpload$name, date_uploaded],
        last_modified_date = Sys.time(),
        deleted_flag = "0",
        firestore_id = values$fact_document()[filename == input$docUpload$name, firestore_id]
      )
      
    } else {
      # Update the data table
      status_entry <- data.table(
        unique_id = ifelse(nrow(values$fact_document()) > 0, values$fact_document()[, max(as.numeric(unique_id)) + 1], 1),
        matter_id = session$userData$matterIDDocument()$matter_id,
        document_type = input$document_type_input,
        filename = input$docUpload$name,
        date_uploaded = Sys.time(),
        last_modified_date = Sys.time(),
        deleted_flag = "0",
        firestore_id = "0"
      )
      dataJSON <- convertDTtoJSON(status_entry)
      
      firestore_response <- sendJSONToFirestore(
        api_call = paste0(db_endpoint_api, db_endpoint_dataDocument),
        access_token = session$userData$current_user()$stsTokenManager$accessToken,
        data = dataJSON
      )
      
      firestore_id_entry <- getFirebaseIDFromPath(fromJSON(content(firestore_response,"text"))$name)
      status_entry[, firestore_id := firestore_id_entry]
    }
    
    # Resent to include firestore_id
    firestore_id_entry <- status_entry[, firestore_id]
    dataJSON <- convertDTtoJSON(status_entry)
    
    firestore_response <- sendJSONToFirestore(
      api_call = paste0(db_endpoint_api, db_endpoint_dataDocument, "/", firestore_id_entry),
      access_token = session$userData$current_user()$stsTokenManager$accessToken,
      data = dataJSON,
      replacement = TRUE
    )
    
    # Combine with table and save in path
    adjusted_fact_matter <- rbind(
      values$fact_document()[unique_id != status_entry[, unique_id]], 
      status_entry
    )
    saveRDS(object = adjusted_fact_matter, file = path_file_fact_document)
    click("document_list_button_js", asis = TRUE)
    shinyalert(paste0("Document uploaded"), type = "success")
  })
}