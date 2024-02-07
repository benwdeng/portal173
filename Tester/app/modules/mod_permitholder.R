###################################################################################################
#
# Portal 173 - About Us module
#
###################################################################################################

mod_permitholder_ui <- function(id){
  
  # Defining namespace
  ns <- NS(id)
  
  fluidPage(
    tabsetPanel(
      tabPanel(
        value = "ph_explain", title = "1 Explain", 
        fluidPage(
          fluidRow(
            h1("What is a section 173 agreement?"),
            align = "center"
          ),
          tags$p(
            class = "text-center",
            tags$img(
              class = "img-responsive img-rounded center-block", 
              src = "What is a s173 agreement.jpg"
              # style = "max-width: 150px;"
            )
          ),
        )
      ),
      tabPanel(
        value = "ph_process", title = "2 Process", 
        fluidPage(
          fluidRow(
            h1("What is the process?"),
            align = "center"
          ),
          tags$p(
            class = "text-center",
            tags$img(
              class = "img-responsive img-rounded center-block", 
              src = "What is the process.jpg"
              # style = "max-width: 150px;"
            )
          ),
        )
      ),
      tabPanel(
        value = "ph_taC", title = "3 Terms", 
        fluidPage(
          fluidRow(
            h1("Terms and conditions"),
            align = "center"
          ),
          box(
            title = "Standard Terms of Appointment",
            status = "danger",
            tags$p(
              "This is our agreement with you as required under the 
              Legal Profession Uniform Law (Victoria) (Uniform Law)
              telling you:"
            ),
            tags$ul(
              tags$li(
                "an estimate of total legal costs and expenses and the 
                basis on which our fees are calculated; or"
              ),
              tags$li(
                "total legal costs where our fees are a fixed amount 
                and expenses incurred on your behalf are estimated; 
                and "
              ),
              tags$li(
                "your rights in relation to costs."
              ),
            ),
            tags$p(
              tags$strong("The basis on which our fees will be calculated")
            ),
          )
        )
      ),
      tabPanel(
        value = "ph_forms", title = "4 Matter details", 
        fluidPage(
          fluidRow(
            h1("Forms"),
            align = "center"
          ),
          fluidRow(
            column(
              width = 6,
              h2("Matters"),
              DT::dataTableOutput(ns("all_matters")),
              br(),
              fluidRow(
                actionButton(inputId = ns("create_matter_button"), label = "Create new matter", icon = icon("pencil-alt")),
                actionButton(inputId = ns("view_edit_matter_button"), label = "View/edit matter", icon = icon("eye")),
                actionButton(inputId = ns("change_matter_status_button"), label = "Change matter status", icon = icon("check-square")),
                actionButton(inputId = ns("delete_matter_button"), label = "Delete matter", icon = icon("times-circle")),
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
      )
    )
  )
}

# Module Server -----------------------------------------------------------
mod_permitholder_server <- function(id, values, users){
  
  moduleServer(
    id,
    function(input, output, session){
      dataPostCodeMapping <- as.data.table(read.csv("inputs/australian_postcodes.csv"))
      dataCouncilInfo <- data.table(read.csv(path_input_council_info, fileEncoding = 'UTF-8-BOM'))
      
      # Sub-tabs ----------------------------------------------------------------
      
      eval(parse("modules/mod_overview_create_new_matter.R"))
      eval(parse("modules/mod_overview_view_edit_matter.R"))
      eval(parse("modules/mod_overview_change_matter_status.R"))
      
      # Create a filtered table
      dataOverview = reactive({values$fact_matter()})
      
      filtereddataOverview = reactive({
        dataOverview()[
          (values$dim_user()[user %in% users$view_as_user(), role] == "Admin") |
          (matter_id %in% values$dim_user_permission()[user %in% users$view_as_user(), matter_id])
        ]
      })
      
      #### START - Show table ----------------------------------
      output$all_matters <- renderDataTable({
        filtereddataOverview()[,
                               .("Matter ID" = matter_id,
                                 "Contact Name" = client_name,
                                 "Subject land" = address,
                                 "Permit Expiry Date" = permit_expiry_date,
                                 "Council" = council_name,
                                 "Service required" = matter_type,
                                 "Last Updated" = format(last_modified_date,"%Y-%b-%d, %I:%M:%S%p"),
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
          s = input$all_matters_rows_selected
          if(!is.null(s)){
            delete_unique_id <- filtereddataOverview()[s, unique_id]
            adjusted_fact_matter <- values$fact_matter()[unique_id != delete_unique_id]
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
            paste0('Last updated: ', filtereddataOverview()[s, format(last_modified_date,"%Y-%b-%d, %I:%M:%S%p")]),
            tags$br()
          ),
          tags$strong("Document uploads"),
          tags$p("List of documents uploaded"),
          tags$p(
            paste0(
              'Certificate of Title: ', 
              if(!is.null(s) && 
                 nrow(values$fact_documents()[matter_id == filtereddataOverview()[s, matter_id] & 
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
                 nrow(values$fact_documents()[matter_id == filtereddataOverview()[s, matter_id] & 
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