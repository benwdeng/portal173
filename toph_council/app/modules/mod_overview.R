###################################################################################################
#
# Portal 173 - About Us module
#
###################################################################################################

mod_overview_ui <- function(id) {
  # Defining namespace
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      h1("Overview"),
      align = "center"
    ),
    br(),
    fluidRow(
      valueBoxOutput(ns("stat_total_matter"), width = 3),
      bsTooltip(id = ns("stat_total_matter"), "Total number of matters in the portal", placement = "bottom", trigger = "hover", options = NULL),
      valueBoxOutput(ns("stat_active_matter"), width = 3),
      bsTooltip(id = ns("stat_active_matter"), "Total number of matters actively sitting with the law firm", placement = "bottom", trigger = "hover", options = NULL),
      valueBoxOutput(ns("stat_pending_matter"), width = 3),
      bsTooltip(id = ns("stat_pending_matter"), "Total number of matters waiting for signature or approval", placement = "bottom", trigger = "hover", options = NULL),
      valueBoxOutput(ns("stat_closed_matter"), width = 3),
      bsTooltip(id = ns("stat_closed_matter"), "Total number of matters closed", placement = "bottom", trigger = "hover", options = NULL)
    ),
    br(),
    fluidRow(
      column(
        width = 12,
        h2("Applications"),
        tabsetPanel(
          id = ns("matter_tabs"), # Assign an ID for the tabsetPanel for potential future use
          tabPanel("Active - action required",
                   column(
                     width = 6,
                     fluidRow(
                       column(
                         width = 6,
                         uiOutput(ns("filter_matter_status_ui_tab1")),
                         bsTooltip(id = ns("filter_matter_status_ui_tab1"), "Filter matter based on its status", placement = "right", trigger = "hover", options = NULL),
                         align = "center"
                       ),
                       column(
                         width = 6,
                         uiOutput(ns("filter_service_ui_tab1")),
                         bsTooltip(id = ns("filter_service_ui_tab1"), "Filter matter based on its service", placement = "right", trigger = "hover", options = NULL),
                         align = "center"
                       ),
                       DT::dataTableOutput(ns("all_matters_tab1")),
                       br(),
                       fluidRow(
                         actionButton(inputId = ns("accept_reject_button"), label = "Accept / Reject", icon = icon("check-square")),
                         bsTooltip(
                           id = ns("accept_reject_button"), 
                           "Once you select a matter, you can accept / reject", 
                           placement = "top", trigger = "hover",
                           options = NULL
                         ),
                         align = "center"
                       )
                     )
                   ),
                   column(
                     width = 6,
                     #verbatimTextOutput(ns("specific_form_output"))
                     conditionalPanel(
                       condition = "typeof input.all_matters_tab1_rows_selected !== 'undefined' && input.all_matters_tab1_rows_selected > 0",
                       ns = ns,
                       uiOutput(ns("specific_form_output_tab1"))
                     )
                   )
          ),
          tabPanel("Active - no action required",
                   column(
                     width = 6,
                     fluidRow(
                       column(
                         width = 6,
                         uiOutput(ns("filter_matter_status_ui_tab2")),
                         bsTooltip(id = ns("filter_matter_status_ui_tab2"), "Filter matter based on its status", placement = "right", trigger = "hover", options = NULL),
                         align = "center"
                       ),
                       column(
                         width = 6,
                         uiOutput(ns("filter_service_ui_tab2")),
                         bsTooltip(id = ns("filter_service_ui_tab2"), "Filter matter based on its service", placement = "right", trigger = "hover", options = NULL),
                         align = "center"
                       ),
                       DT::dataTableOutput(ns("all_matters_tab2"))
                     )
                   ),
                   column(
                     width = 6,
                     #verbatimTextOutput(ns("specific_form_output"))
                     conditionalPanel(
                       condition = "typeof input.all_matters_tab2_rows_selected !== 'undefined' && input.all_matters_tab2_rows_selected > 0",
                       ns = ns,
                       uiOutput(ns("specific_form_output_tab2"))
                     )
                   )
          ),
          tabPanel("Closed",
                   column(
                     width = 6,
                     fluidRow(
                       column(
                         width = 6,
                         uiOutput(ns("filter_matter_status_ui_tab3")),
                         bsTooltip(id = ns("filter_matter_status_ui_tab3"), "Filter matter based on its status", placement = "right", trigger = "hover", options = NULL),
                         align = "center"
                       ),
                       column(
                         width = 6,
                         uiOutput(ns("filter_service_ui_tab3")),
                         bsTooltip(id = ns("filter_service_ui_tab3"), "Filter matter based on its service", placement = "right", trigger = "hover", options = NULL),
                         align = "center"
                       ),
                       DT::dataTableOutput(ns("all_matters_tab3"))
                     )
                   ),
                   column(
                     width = 6,
                     #verbatimTextOutput(ns("specific_form_output"))
                     conditionalPanel(
                       condition = "typeof input.all_matters_tab3_rows_selected !== 'undefined' && input.all_matters_tab3_rows_selected > 0",
                       ns = ns,
                       uiOutput(ns("specific_form_output_tab3"))
                     )
                   )
          )
        )
      )
    ),
    # Add modal and other UI elements as before
    create_new_matter_ui(ns = ns),
    change_matter_status_ui(ns = ns),
    view_edit_matter_ui(ns = ns),
    update_document_ui(ns = ns),
    accept_reject_ui(ns = ns)
  )
}







# Module Server -----------------------------------------------------------
mod_overview_server <- function(id, values){
  
  moduleServer(
    id,
    function(input, output, session){
      dataPostCodeMapping <- as.data.table(read.csv("inputs/australian_postcodes.csv"))
      dataCouncilInfo <- data.table(read.csv(path_input_council_info, fileEncoding = 'UTF-8-BOM'))
      
      # Sub-tabs ----------------------------------------------------------------
      
      eval(parse("modules/mod_overview_create_new_matter.R"))
      eval(parse("modules/mod_overview_view_edit_matter.R"))
      eval(parse("modules/mod_overview_change_matter_status.R"))
      eval(parse("modules/mod_overview_accept_reject.R"))
      
      # update value boxes
      output$stat_total_matter <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter())
        valueBox(
          paste0(total_matter_count),
          "total applications", icon = icon("list"),
          color = "blue"
        )
      })
      
      output$stat_active_matter <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter()[status_of_matter %in% c("2 Drafting stage", "3 Signing stage")])
        valueBox(
          paste0(total_matter_count), 
          "active applications - action required", icon = icon("user-circle"),
          color = "purple"
        )
      })
      
      output$stat_pending_matter <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter()[status_of_matter %in% c("3 Pending council", "3 Pending owner",
                                                                                "3 Pending other stakeholders",
                                                                                "4 Lodgement")])
        valueBox(
          paste0(total_matter_count), "active applications - no action required", icon = icon("hourglass-half"),
          color = "orange"
        )
      })
      
      output$stat_closed_matter <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter()[status_of_matter == "5 Closed matter"])
        valueBox(
          paste0(total_matter_count), "closed applications", icon = icon("thumbs-up"),
          color = "green"
        )
      })
      
      #### START - Filter data table ----------------------------------
      output$filter_matter_status_ui_tab1 <- renderUI({
        case_status_list <- status_choices
        
        fluidRow(
          HTML("Matter status to show:"),
          selectInput(
            inputId = session$ns("filter_matter_status"), # session$ns() is necessary to access inputs inside a renderUI
            label = NULL,
            choices = append("All", status_choices),
            selected = "All"
          )
        )
      })
      
      output$filter_matter_status_ui_tab2 <- renderUI({
        case_status_list <- status_choices
        
        fluidRow(
          HTML("Application status to show:"),
          selectInput(
            inputId = session$ns("filter_matter_status"), # session$ns() is necessary to access inputs inside a renderUI
            label = NULL,
            choices = append("All", status_choices),
            selected = "All"
          )
        )
      })

      output$filter_matter_status_ui_tab3 <- renderUI({
        case_status_list <- status_choices
        
        fluidRow(
          HTML("Matter status to show:"),
          selectInput(
            inputId = session$ns("filter_matter_status"), # session$ns() is necessary to access inputs inside a renderUI
            label = NULL,
            choices = append("All", status_choices),
            selected = "All"
          )
        )
      })
      
      output$filter_service_ui_tab1 <- renderUI({
        service_list <- type_of_agreement
        
        fluidRow(
          HTML("Service to show"),
          selectInput(
            inputId = session$ns("filter_service"), # session$ns() is necessary to access inputs inside a renderUI
            label = NULL,
            choices = append("All", service_list),
            selected = "All"
          )
        )
      })
      
      output$filter_service_ui_tab2 <- renderUI({
        service_list <- type_of_agreement
        
        fluidRow(
          HTML("Service to show"),
          selectInput(
            inputId = session$ns("filter_service"), # session$ns() is necessary to access inputs inside a renderUI
            label = NULL,
            choices = append("All", service_list),
            selected = "All"
          )
        )
      })
      
      output$filter_service_ui_tab3 <- renderUI({
        service_list <- type_of_agreement
        
        fluidRow(
          HTML("Service to show"),
          selectInput(
            inputId = session$ns("filter_service"), # session$ns() is necessary to access inputs inside a renderUI
            label = NULL,
            choices = append("All", service_list),
            selected = "All"
          )
        )
      })
      #### END - Filter data table ----------------------------------
      
      # Create a filtered table
      dataOverview_tab1 = reactive({values$fact_matter()})
      
      filtereddataOverview_tab1 = reactive({
        dataOverview_tab1()[
            (council_name == user_council_mapping[[session$userData$user_email()]]) &
            (input$filter_matter_status == "All" | status_of_matter == input$filter_matter_status) &
            (input$filter_service == "All" | matter_type == input$filter_service)  
        ]
      })
      
      dataOverview_tab2 = reactive({values$fact_matter()})
      
      filtereddataOverview_tab2 = reactive({
        dataOverview_tab2()[
            (council_name == user_council_mapping[[session$userData$user_email()]]) &
            (input$filter_matter_status == "All" | status_of_matter == input$filter_matter_status) &
            (input$filter_service == "All" | matter_type == input$filter_service)  
        ]
      })
      
      dataOverview_tab3 = reactive({values$fact_matter()})
      
      filtereddataOverview_tab3 = reactive({
        dataOverview_tab3()[
            (council_name == user_council_mapping[[session$userData$user_email()]]) &
            (input$filter_matter_status == "All" | status_of_matter == input$filter_matter_status) &
            (input$filter_service == "All" | matter_type == input$filter_service)  
        ]
      })
      
      #### START - Show table ----------------------------------
      output$all_matters_tab1 <- renderDataTable({
        
        filtereddataOverview_tab1()[deleted_flag == 0,
                                    .("Application ID" = matter_id,
                                      "Contact Name" = client_name,
                                      "Subject land" = address,
                                      "Permit Expiry Date" = format(permit_expiry_date, "%d %b %Y"),
                                      #"Council" = council_name,
                                      "Service required" = matter_type,
                                      "Last Updated" = format(last_modified_date,"%d %b %Y, %I:%M:%S%p"),
                                      "Status" = status_of_matter)
        ]
      }, selection = 'single', options = list(scrollX = TRUE))
      
      output$all_matters_tab2 <- renderDataTable({
        
        filtereddataOverview_tab2()[deleted_flag == 0,
                                    .("Application ID" = matter_id,
                                      "Contact Name" = client_name,
                                      "Subject land" = address,
                                      "Permit Expiry Date" = format(permit_expiry_date, "%d %b %Y"),
                                      #"Council" = council_name,
                                      "Service required" = matter_type,
                                      "Last Updated" = format(last_modified_date,"%d %b %Y, %I:%M:%S%p"),
                                      "Status" = status_of_matter)
        ]
      }, selection = 'single', options = list(scrollX = TRUE))
      
      output$all_matters_tab3 <- renderDataTable({
        
        filtereddataOverview_tab3()[deleted_flag == 0,
                                    .("Application ID" = matter_id,
                                      "Contact Name" = client_name,
                                      "Subject land" = address,
                                      "Permit Expiry Date" = format(permit_expiry_date, "%d %b %Y"),
                                      #"Council" = council_name,
                                      "Service required" = matter_type,
                                      "Last Updated" = format(last_modified_date,"%d %b %Y, %I:%M:%S%p"),
                                      "Status" = status_of_matter)
        ]
      }, selection = 'single', options = list(scrollX = TRUE))
      #### END - Show table ----------------------------------
      
      # Modal pop up when user clicks button to create a new matter
      create_new_matter_server(session, input, output, reactive({input$create_matter_button}), values)
      view_edit_matter_server(session, input, output, reactive({input$view_edit_matter_button}), values)
      change_matter_status_server(session, input, output, reactive({input$change_matter_status_button}), values)
      accept_reject_server(session, input, output, reactive({input$accept_reject_button}), values)
      
      # Delete matter 
      observeEvent(
        input$delete_matter_button, 
        {
          s <- input$all_matters_rows_selected
          if(!is.null(s)){
            
            status_entry <- filtereddataOverview_tab1()[s]
            status_entry <- status_entry[,
                                         c("deleted_flag", "last_modified_date") := list(
                                           "1", Sys.time()
                                         )
            ]
            
            # Get Firestore ID
            firestore_id_entry <- status_entry[, firestore_id]
            dataJSON <- convertDTtoJSON(status_entry)
            
            firestore_response <- sendJSONToFirestore(
              api_call = paste0(db_endpoint_api, db_endpoint_dataOverview, "/", firestore_id_entry),
              access_token = session$userData$current_user()$stsTokenManager$accessToken,
              data = dataJSON,
              replacement = TRUE
            )
            
            adjusted_fact_matter <- rbind(values$fact_matter()[unique_id != status_entry[, unique_id]], status_entry)
            saveRDS(object = adjusted_fact_matter, file = path_file_fact_matter)
            
            shinyalert(paste0("Matter deleted"), type = "success")
          } else {
            shinyalert(paste0("No matters selected"), type = "warning")
          }
          
        }
      )
      
      #### START - Print selected information on the side  ----------------------------------
      output$specific_form_output_tab1 = renderUI({
        s = input$all_matters_tab1_rows_selected
        
        box(
          width = 12,
          title = paste0("Form information for ", filtereddataOverview_tab1()[s, matter_id]),
          tags$p(
            paste0('Showing details for application ', filtereddataOverview_tab1()[s, matter_id]),
            tags$br(),
            paste0('Form ID: ', filtereddataOverview_tab1()[s, matter_id]),
            tags$br(),
            paste0('Council: ', filtereddataOverview_tab1()[s, council_name]),
            tags$br(),
            paste0('Subject land address: ', filtereddataOverview_tab1()[s, address]),
            tags$br(),
            paste0('Contact: ', filtereddataOverview_tab1()[s, client_name]),
            tags$br(),
            paste0('Contact role: ', filtereddataOverview_tab1()[s, client_role]),
            tags$br(),
            paste0('Contact postal address: ', filtereddataOverview_tab1()[s, client_postal_address]),
            tags$br(),
            paste0('Contact number: ', filtereddataOverview_tab1()[s, client_contact_number]),
            tags$br(),
            paste0('Contact email: ', filtereddataOverview_tab1()[s, client_email]),
            tags$br(),
            paste0('Last updated: ', filtereddataOverview_tab1()[s, format(last_modified_date,"%d %b %Y, %I:%M:%S%p")]),
            tags$br(),
            paste0('Creation time/date: ', filtereddataOverview_tab1()[s, format(created_date,"%d %b %Y, %I:%M:%S%p")]),
            tags$br()
          ),
          tags$strong("Document uploads"),
          tags$p("List of documents uploaded"),
          tags$p(
            paste0(
              'Certificate of Title: ', 
              if(!is.null(s) && 
                 nrow(values$fact_document()[matter_id == filtereddataOverview_tab1()[s, matter_id] & 
                                             document_type == "Certificate/s of Title"]) > 0
              ){
                "Yes"
              } else {
                "No"
              }
            ),
            tags$br(),
            paste0(
              'Planning permits: ', 
              if(!is.null(s) && 
                 nrow(values$fact_document()[matter_id == filtereddataOverview_tab1()[s, matter_id] & 
                                             document_type == "Planning Permits"]) > 0
              ){
                "Yes"
              } else {
                "No"
              }
            ),
            tags$br(),
            paste0(
              'Draft 173 Agreement - for council approval and signature: No'
            ),
            tags$br()
          ),
          actionButton(
            inputId = session$ns("upload_document_button"), 
            label = "Document download", 
            icon = icon("upload")
          ),
          bsTooltip(
            id = session$ns("upload_document_button"), 
            "Takes you to a pop up that will allow you to upload/download documents and check uploaded documents", 
            placement = "right", trigger = "hover",
            options = NULL
          ),
          tags$br(),
          tags$strong("Location"),
          tags$br(),
          leafletOutput(session$ns("mymap_tab1"))
        )
      })
      
      # Modal pop up when user clicks upload document
      update_document_server(
        session, input, output, reactive({input$upload_document_button}), values,
        reactive({filtereddataOverview_tab1()[input$all_matters_tab1_rows_selected, matter_id]})
      )
      
      output$mymap_tab1 <- renderLeaflet({
        s = input$all_matters_tab1_rows_selected
        
        setView(
          addMarkers(   
            addTiles(
              leaflet()
            ),
            data = dataPostCodeMapping[
              postcode %in% filtereddataOverview_tab1()[s, address_postcode], 
              .(long = mean(long), lat = mean(lat))
            ]
          ),
          lng = 144.94, 
          lat = -37.84,
          zoom = 8
        )
      })
      
      output$specific_form_output_tab2 = renderUI({
        s = input$all_matters_tab2_rows_selected
        
        box(
          width = 12,
          title = paste0("Form information for ", filtereddataOverview_tab2()[s, matter_id]),
          tags$p(
            paste0('Showing details for application ', filtereddataOverview_tab2()[s, matter_id]),
            tags$br(),
            paste0('Form ID: ', filtereddataOverview_tab2()[s, matter_id]),
            tags$br(),
            paste0('Council: ', filtereddataOverview_tab2()[s, council_name]),
            tags$br(),
            paste0('Subject land address: ', filtereddataOverview_tab2()[s, address]),
            tags$br(),
            paste0('Contact: ', filtereddataOverview_tab2()[s, client_name]),
            tags$br(),
            paste0('Contact role: ', filtereddataOverview_tab2()[s, client_role]),
            tags$br(),
            paste0('Contact postal address: ', filtereddataOverview_tab2()[s, client_postal_address]),
            tags$br(),
            paste0('Contact number: ', filtereddataOverview_tab2()[s, client_contact_number]),
            tags$br(),
            paste0('Contact email: ', filtereddataOverview_tab2()[s, client_email]),
            tags$br(),
            paste0('Last updated: ', filtereddataOverview_tab2()[s, format(last_modified_date,"%d %b %Y, %I:%M:%S%p")]),
            tags$br(),
            paste0('Creation time/date: ', filtereddataOverview_tab2()[s, format(created_date,"%d %b %Y, %I:%M:%S%p")]),
            tags$br()
          ),
          tags$strong("Document uploads"),
          tags$p("List of documents uploaded"),
          tags$p(
            paste0(
              'Certificate of Title: ', 
              if(!is.null(s) && 
                 nrow(values$fact_document()[matter_id == filtereddataOverview_tab2()[s, matter_id] & 
                                             document_type == "Certificate/s of Title"]) > 0
              ){
                "Yes"
              } else {
                "No"
              }
            ),
            tags$br(),
            paste0(
              'Planning permits: ', 
              if(!is.null(s) && 
                 nrow(values$fact_document()[matter_id == filtereddataOverview_tab2()[s, matter_id] & 
                                             document_type == "Planning Permits"]) > 0
              ){
                "Yes"
              } else {
                "No"
              }
            ),
            tags$br()
          ),
          actionButton(
            inputId = session$ns("upload_document_button_tab1"), 
            label = "Document upload", 
            icon = icon("upload")
          ),
          bsTooltip(
            id = session$ns("upload_document_button_tab1"), 
            "Takes you to a pop up that will allow you to upload/download documents and check uploaded documents", 
            placement = "right", trigger = "hover",
            options = NULL
          ),
          tags$br(),
          tags$strong("Location"),
          tags$br(),
          leafletOutput(session$ns("mymap_2"))
        )
      })
      
      # Modal pop up when user clicks upload document
      update_document_server(
        session, input, output, reactive({input$upload_document_button_tab1}), values,
        reactive({filtereddataOverview_tab2()[input$all_matters_tab2_rows_selected, matter_id]})
      )
      
      output$mymap_2 <- renderLeaflet({
        s = input$all_matters_tab2_rows_selected
        
        setView(
          addMarkers(   
            addTiles(
              leaflet()
            ),
            data = dataPostCodeMapping[
              postcode %in% filtereddataOverview_tab2()[s, address_postcode], 
              .(long = mean(long), lat = mean(lat))
            ]
          ),
          lng = 144.94, 
          lat = -37.84,
          zoom = 8
        )
      })
      
      output$specific_form_output_tab3 = renderUI({
        s = input$all_matters_tab3_rows_selected
        
        box(
          width = 12,
          title = paste0("Form information for ", filtereddataOverview_tab3()[s, matter_id]),
          tags$p(
            paste0('Showing details for application ', filtereddataOverview_tab3()[s, matter_id]),
            tags$br(),
            paste0('Form ID: ', filtereddataOverview_tab3()[s, matter_id]),
            tags$br(),
            paste0('Council: ', filtereddataOverview_tab3()[s, council_name]),
            tags$br(),
            paste0('Subject land address: ', filtereddataOverview_tab3()[s, address]),
            tags$br(),
            paste0('Contact: ', filtereddataOverview_tab3()[s, client_name]),
            tags$br(),
            paste0('Contact role: ', filtereddataOverview_tab3()[s, client_role]),
            tags$br(),
            paste0('Contact postal address: ', filtereddataOverview_tab3()[s, client_postal_address]),
            tags$br(),
            paste0('Contact number: ', filtereddataOverview_tab3()[s, client_contact_number]),
            tags$br(),
            paste0('Contact email: ', filtereddataOverview_tab3()[s, client_email]),
            tags$br(),
            paste0('Last updated: ', filtereddataOverview_tab3()[s, format(last_modified_date,"%d %b %Y, %I:%M:%S%p")]),
            tags$br(),
            paste0('Creation time/date: ', filtereddataOverview_tab3()[s, format(created_date,"%d %b %Y, %I:%M:%S%p")]),
            tags$br()
          ),
          tags$strong("Document uploads"),
          tags$p("List of documents uploaded"),
          tags$p(
            paste0(
              'Certificate of Title: ', 
              if(!is.null(s) && 
                 nrow(values$fact_document()[matter_id == filtereddataOverview_tab3()[s, matter_id] & 
                                             document_type == "Certificate/s of Title"]) > 0
              ){
                "Yes"
              } else {
                "No"
              }
            ),
            tags$br(),
            paste0(
              'Planning permits: ', 
              if(!is.null(s) && 
                 nrow(values$fact_document()[matter_id == filtereddataOverview_tab3()[s, matter_id] & 
                                             document_type == "Planning Permits"]) > 0
              ){
                "Yes"
              } else {
                "No"
              }
            ),
            tags$br()
          ),
          actionButton(
            inputId = session$ns("upload_document_button"), 
            label = "Document upload", 
            icon = icon("upload")
          ),
          bsTooltip(
            id = session$ns("upload_document_button"), 
            "Takes you to a pop up that will allow you to upload/download documents and check uploaded documents", 
            placement = "right", trigger = "hover",
            options = NULL
          ),
          tags$br(),
          tags$strong("Location"),
          tags$br(),
          leafletOutput(session$ns("mymap_3"))
        )
      })
      
      # Modal pop up when user clicks upload document
      update_document_server(
        session, input, output, reactive({input$upload_document_button}), values,
        reactive({filtereddataOverview_tab3()[input$all_matters_tab3_rows_selected, matter_id]})
      )
      
      output$mymap_3 <- renderLeaflet({
        s = input$all_matters_tab3_rows_selected
        
        setView(
          addMarkers(   
            addTiles(
              leaflet()
            ),
            data = dataPostCodeMapping[
              postcode %in% filtereddataOverview_tab3()[s, address_postcode], 
              .(long = mean(long), lat = mean(lat))
            ]
          ),
          lng = 144.94, 
          lat = -37.84,
          zoom = 8
        )
      })
      #### END - Print selected information on the side  ----------------------------------
    }
  )
  
}