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
          uiOutput(ns("document_type_ui")),
          DT::dataTableOutput(ns("table_document_list")),
          br(),
          uiOutput(ns("document_buttons_ui"))
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
      h3(paste0("Document uploads for ", specific_matter_id()), align = "left"),
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
  
  output$document_buttons_ui <- renderUI({
    fluidRow(
      if(is.null(input$table_document_list_rows_selected)){
        actionButton(
          inputId = session$ns("dummy_download_button"),
          label = "Download document", 
          width = "100%",
          icon = icon("download")
        )
      } else {
        downloadButton(
          outputId = session$ns("download_document"), 
          label = "Download document", 
          style = "width:100%",
          icon = icon("download")
        )
      },
      actionButton(
        inputId = session$ns("delete_specific_document_button"), 
        label = "Delete document", 
        icon = icon("times-circle"),
        width = "100%"
      ),
      if(is.null(input$document_type_input) || input$document_type_input == "All"){
        actionButton(
          inputId = session$ns("dummy_upload_button"),
          label = "Upload document", 
          width = "100%",
          icon = icon("upload")
        )
      } else {
        fileInput(
          inputId = session$ns("docUpload"),
          multiple = TRUE,
          label = NULL,
          buttonLabel = list(icon("upload"), "Upload"),
          width = "100%"
        )
      }
    )
  })
  
  observeEvent(input$document_type_input, {
    output$table_document_list <- renderDataTable({
      values$fact_documents()[
        matter_id == specific_matter_id() & (input$document_type_input == "All" | document_type %in% input$document_type_input),
        .("File name" = filename, "Date uploaded" = format(date_uploaded, "%Y-%b-%d, %I:%M:%S%p"))
      ]
    })
  })
  
  #### START - What happens once user upload the document ----
  observeEvent(input$dummy_upload_button, {
    shinyalert(paste0("Select document type before uploading"), type = "warning")
  })
  
  
  observeEvent(input$docUpload, {
    # Copy from temporary location into documents folder
    # TODO Need to check multiple documents
    if(input$document_type_input == "All"){
      shinyalert(paste0("Select a document type to upload before uploading"), type = "warning")
    } else {
      file.copy(
        input$docUpload$datapath,
        paste0("www/documents/", specific_matter_id(), "-", input$docUpload$name),
        overwrite = TRUE, copy.date = TRUE) # Recursive to overwrite previous version
      
      # Update the data table
      status_entry <- data.table(
        unique_id = if(nrow(values$fact_documents()) > 0){values$fact_documents()[, max(unique_id) + 1]} else {1},
        matter_id = specific_matter_id(),
        document_type = input$document_type_input,
        filename = paste0(specific_matter_id(), "-", input$docUpload$name),
        date_uploaded = Sys.time()
      )
      
      # Combine with table and save in path
      adjusted_fact_matter <- rbind(
        values$fact_documents(), 
        status_entry
      )
      saveRDS(object = adjusted_fact_matter, file = path_file_fact_documents)
      shinyalert(paste0("Document uploaded"), type = "success")
    }
    
  })
  #### END - What happens once user upload the document ----
  
  # Area to delete specific item
  observeEvent(
    input$delete_specific_document_button, 
    {
      s = isolate(input$table_document_list_rows_selected)
      if(!is.null(s)){
        delete_unique_id <- values$fact_documents()[
          matter_id == specific_matter_id() & document_type %in% isolate(input$document_type_input)
        ][s, unique_id]
        adjusted_fact_documents <- values$fact_documents()[unique_id != delete_unique_id]
        saveRDS(object = adjusted_fact_documents, file = path_file_fact_documents)
        shinyalert(paste0("Document deleted"), type = "success")
      } else {
        shinyalert(paste0("No documents selected"), type = "warning")
      }
      
    }
  )
  
  #### START - What happens once user downloads the document ----
  observeEvent(input$dummy_download_button, {
    shinyalert(paste0("No documents selected"), type = "warning")
  })
  
  output$download_document <- downloadHandler(
    filename = function(){
      entry <- values$fact_documents()[
        matter_id == specific_matter_id() & 
          (document_type %in% input$document_type_input | input$document_type_input == "All")
      ][input$table_document_list_rows_selected]
      paste0(entry[, filename])
    },
    content = function(file){
      entry <- values$fact_documents()[
        matter_id == specific_matter_id() & 
          (document_type %in% input$document_type_input | input$document_type_input == "All")
      ][input$table_document_list_rows_selected]
      file.copy(paste0("www/documents/", entry[, filename]), file)
    }
  )
  #### END - What happens once user downloads the document ----
}