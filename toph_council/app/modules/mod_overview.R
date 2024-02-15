###################################################################################################
#
# Portal 173 - About Us module
#
###################################################################################################

mod_overview_ui <- function(id){
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
      bsTooltip(
        id = ns("stat_total_matter"), "Total number of matters in the portal", placement = "bottom", trigger = "hover",
        options = NULL
      ),
      valueBoxOutput(ns("stat_active_matter"), width = 3),
      bsTooltip(
        id = ns("stat_active_matter"), "Total number of matters actively sitting with the law firm", placement = "bottom", trigger = "hover",
        options = NULL
      ),
      valueBoxOutput(ns("stat_pending_matter"), width = 3),
      bsTooltip(
        id = ns("stat_pending_matter"), "Total number of matters waiting for signature or approval", placement = "bottom", trigger = "hover",
        options = NULL
      ),
      valueBoxOutput(ns("stat_closed_matter"), width = 3),
      bsTooltip(
        id = ns("stat_closed_matter"), "Total number of matters closed", placement = "bottom", trigger = "hover",
        options = NULL
      )
    ),
    br(),
    fluidRow(
      column(
        width = 6,
        h2("Matters"),
        fluidRow(
          column(
            width = 6,
            uiOutput(ns("filter_matter_status_ui")),
            bsTooltip(
              id = ns("filter_matter_status_ui"), "Filter matter based on its status", placement = "right", trigger = "hover",
              options = NULL
            ),
            align = "center"
          ),
          column(
            width = 6,
            uiOutput(ns("filter_council_name_ui")),
            bsTooltip(
              id = ns("filter_council_name_ui"), "Filter matter based the the relevant council", placement = "right", trigger = "hover",
              options = NULL
            ),
            align = "center"
          )
        ),
        DT::dataTableOutput(ns("all_matters")),
        br(),
        fluidRow(
          actionButton(inputId = ns("create_matter_button"), label = "Create new matter", icon = icon("pencil-alt")),
          bsTooltip(
            id = ns("create_matter_button"), "Create a new matter", 
            placement = "top", trigger = "hover",
            options = NULL
          ),
          actionButton(inputId = ns("view_edit_matter_button"), label = "View/edit matter", icon = icon("eye")),
          bsTooltip(
            id = ns("view_edit_matter_button"), 
            "Once you select a matter, this directs you to a detailed view where you can edit the information", 
            placement = "top", trigger = "hover",
            options = NULL
          ),
          actionButton(inputId = ns("change_matter_status_button"), label = "Change matter status", icon = icon("check-square")),
          bsTooltip(
            id = ns("change_matter_status_button"), 
            "Once you select a matter, you can change the matter status", 
            placement = "top", trigger = "hover",
            options = NULL
          ),
          actionButton(inputId = ns("delete_matter_button"), label = "Delete matter", icon = icon("times-circle")),
          bsTooltip(
            id = ns("delete_matter_button"), 
            "Deletes the matter from the portal. NOTE, administrators can recover the deleted files", 
            placement = "top", trigger = "hover",
            options = NULL
          ),
          align = "center"
        )
      ),
      column(
        width = 6,
        #verbatimTextOutput(ns("specific_form_output"))
        conditionalPanel(
          condition = "typeof input.all_matters_rows_selected !== 'undefined' && input.all_matters_rows_selected.length > 0",
          ns = ns,
          uiOutput(ns("specific_form_output"))
        )
      )
    ),
    # Add modal
    create_new_matter_ui(ns = ns),
    change_matter_status_ui(ns = ns),
    view_edit_matter_ui(ns = ns),
    update_document_ui(ns = ns)
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
      
      # update value boxes
      output$stat_total_matter <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter())
        valueBox(
          paste0(total_matter_count),
          "total matters", icon = icon("list"),
          color = "blue"
        )
      })
      
      output$stat_active_matter <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter()[status_of_matter %in% c("2 Drafting stage", "3 Signing stage")])
        valueBox(
          paste0(total_matter_count), 
          "active matters", icon = icon("user-circle"),
          color = "purple"
        )
      })
      
      output$stat_pending_matter <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter()[status_of_matter %in% c("3 Pending council", "3 Pending owner",
                                                                                "3 Pending other stakeholders",
                                                                                "4 Lodgement")])
        valueBox(
          paste0(total_matter_count), "pending matters", icon = icon("hourglass-half"),
          color = "orange"
        )
      })
      
      output$stat_closed_matter <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter()[status_of_matter == "5 Closed matter"])
        valueBox(
          paste0(total_matter_count), "closed matters", icon = icon("thumbs-up"),
          color = "green"
        )
      })
      
      #### START - Filter data table ----------------------------------
      output$filter_matter_status_ui <- renderUI({
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
      
      output$filter_council_name_ui <- renderUI({
        council_list <- dataCouncilInfo[, unique(Council)]
        
        fluidRow(
          HTML("Council to show:"),
          selectInput(
            inputId = session$ns("filter_council_name"), # session$ns() is necessary to access inputs inside a renderUI
            label = NULL,
            choices = append("All", council_list),
            selected = "All"
          )
        )
      })
      #### END - Filter data table ----------------------------------
      
      # Create a filtered table
      dataOverview = reactive({values$fact_matter()})
      
      filtereddataOverview = reactive({
        dataOverview()[
          (input$filter_matter_status == "All" | status_of_matter == input$filter_matter_status) &
            (input$filter_council_name == "All" | council_name == input$filter_council_name)  
        ]
      })
      
      #### START - Show table ----------------------------------
      output$all_matters <- renderDataTable({
        
        filtereddataOverview()[deleted_flag == 0,
          .("Matter ID" = matter_id,
            "Contact Name" = client_name,
            "Subject land" = address,
            "Permit Expiry Date" = format(permit_expiry_date, "%d %b %Y"),
            "Council" = council_name,
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

      # Delete matter 
      observeEvent(
        input$delete_matter_button, 
        {
          s <- input$all_matters_rows_selected
          if(!is.null(s)){
            
            status_entry <- filtereddataOverview()[s]
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
      output$specific_form_output = renderUI({
        s = input$all_matters_rows_selected
        
        box(
          width = 12,
          title = paste0("Form information for ", filtereddataOverview()[s, matter_id]),
          tags$p(
            paste0('Showing details for application ', filtereddataOverview()[s, matter_id]),
            tags$br(),
            paste0('Form ID: ', filtereddataOverview()[s, matter_id]),
            tags$br(),
            paste0('Council: ', filtereddataOverview()[s, council_name]),
            tags$br(),
            paste0('Subject land address: ', filtereddataOverview()[s, address]),
            tags$br(),
            paste0('Contact: ', filtereddataOverview()[s, client_name]),
            tags$br(),
            paste0('Contact role: ', filtereddataOverview()[s, client_role]),
            tags$br(),
            paste0('Contact postal address: ', filtereddataOverview()[s, client_postal_address]),
            tags$br(),
            paste0('Contact number: ', filtereddataOverview()[s, client_contact_number]),
            tags$br(),
            paste0('Contact email: ', filtereddataOverview()[s, client_email]),
            tags$br(),
            paste0('Last updated: ', filtereddataOverview()[s, format(last_modified_date,"%d %b %Y, %I:%M:%S%p")]),
            tags$br(),
            paste0('Creation time/date: ', filtereddataOverview()[s, format(created_date,"%d %b %Y, %I:%M:%S%p")]),
            tags$br()
          ),
          tags$strong("Document uploads"),
          tags$p("List of documents uploaded"),
          tags$p(
            paste0(
              'Certificate of Title: ', 
              if(!is.null(s) && 
                 nrow(values$fact_document()[matter_id == filtereddataOverview()[s, matter_id] & 
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
                 nrow(values$fact_document()[matter_id == filtereddataOverview()[s, matter_id] & 
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
          leafletOutput(session$ns("mymap"))
        )
      })
      
      # Modal pop up when user clicks upload document
      update_document_server(
        session, input, output, reactive({input$upload_document_button}), values,
        reactive({filtereddataOverview()[input$all_matters_rows_selected, matter_id]})
      )

      output$mymap <- renderLeaflet({
        s = input$all_matters_rows_selected
        
        setView(
          addMarkers(   
            addTiles(
              leaflet()
            ),
            data = dataPostCodeMapping[
              postcode %in% filtereddataOverview()[s, address_postcode], 
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