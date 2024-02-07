###################################################################################################
#
# Portal 173 - About Us module
#
###################################################################################################

mod_overview_ui <- function(id){
  # Defining namespace
  ns <- NS(id)
  fluidPage(
    fluidRow(uiOutput(ns("filter_council_ui")),
             align = "left"),
    fluidRow(
      h1("Overview"),
      align = "center"
    ),
    br(),
    fluidRow(
      valueBoxOutput(ns("stat_total_matter"), width = 3),
      valueBoxOutput(ns("stat_active_matter"), width = 3),
      valueBoxOutput(ns("stat_pending_matter"), width = 3),
      valueBoxOutput(ns("stat_closed_matter"), width = 3)
    ),
    br(),
    fluidRow(
      column(
        width = 6,
        h2("Matters"),
        fluidRow(
          uiOutput(ns("filter_matter_status_ui")),
          align = "center"
        ),
        DT::dataTableOutput(ns("all_matters"))
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
    approve_matter_ui(ns = ns),
    reject_matter_ui(ns = ns)
  )
  
}

# Module Server -----------------------------------------------------------
mod_overview_server <- function(id, values){
  
  moduleServer(
    id,
    function(input, output, session){
      dataPostCodeMapping <- as.data.table(read.csv("inputs/australian_postcodes.csv"))
      dataLgaMapping <- as.data.table(read.csv("inputs/australian_postcodes.csv"))
      ns <- NS(id)
      
      # Sub-tabs ----------------------------------------------------------------
      
      # eval(parse("modules/mod_overview_create_new_matter.R"))
      # eval(parse("modules/mod_overview_view_edit_matter.R"))
      # eval(parse("modules/mod_overview_change_matter_status.R"))
      eval(parse("modules/council_mod_overview_approve_matter.R"))
      eval(parse("modules/council_mod_overview_reject_matter.R"))
      
      #Modals
      approve_matter_server(session, input, output, reactive({input$approve_matter_button}), values)
      reject_matter_server(session, input, output, reactive({input$reject_matter_button}), values)
      
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
        total_matter_count <- nrow(values$fact_matter()[status_of_matter != "5 Closed matter"])
        valueBox(
          paste0(total_matter_count), 
          "active matters", icon = icon("user-circle"),
          color = "purple"
        )
      })
      
      output$stat_pending_matter <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter()[grepl("pending", status_of_matter, ignore.case = TRUE)])
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
      
      #### START - Filter data table for council ----------------------------------
      
      output$filter_council_ui <- renderUI({
        
        fluidRow(
          HTML("Select council:"),
          selectInput(
            inputId = session$ns("filter_council"), # session$ns() is necessary to access inputs inside a renderUI
            label = NULL,
            choices = append("All", vic_council_list),
            selected = "All"
          )
        )
      })
      #### END - Filter data table council ----------------------------------
      
      
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
      #### END - Filter data table ----------------------------------
      
      # Create a filtered table
      dataOverview = reactive({merge(values$fact_matter()[,POSTCODE := as.integer(address_postcode)],postcode_to_lga_mapping, by="POSTCODE", all.x = TRUE)})
      
      #dataOverview = reactive({values$fact_matter()})
      
      councilfilterdataOverview = reactive({
        dataOverview()[
          (input$filter_council == "All" | LGA_NAME_2011 == input$filter_council)
        ]
      })
      
      filtereddataOverview = reactive({
        councilfilterdataOverview()[
          (input$filter_matter_status == "All" | status_of_matter == input$filter_matter_status)
        ]
      })
      
      #### START - Show table ----------------------------------
      output$all_matters <- renderDataTable({
        filtereddataOverview()[,
                               .("Matter ID" = matter_id,
                                 "Client Name" = client_name,
                                 "Subject land" = address,
                                 "Permit Expiry Date" = permit_expiry_date,
                                 "Type of form" = matter_type,
                                 "Last Updated" = format(last_modified_date,"%Y-%b-%d, %I:%M:%S%p"),
                                 "Status" = status_of_matter)
        ]
      }, selection = 'single', options = list(scrollX = TRUE))
      
      #### END - Show table ----------------------------------
      
      ###START - Document Table
      document_list <- reactive({as.data.table(list.files(path = "./www/documents",pattern = paste0(".*",as.character(filtereddataOverview()[input$all_matters_rows_selected, matter_id]),".*")))})
      
      output$documentTable <- renderDataTable({document_list()},
                                              selection = 'single',
                                              options = list(dom = 't',
                                                             scrollX = TRUE,
                                                             pageLength = 5,
                                                             rownames = FALSE,
                                                             headerCallback = JS(
                                                               "function(thead, data, start, end, display){",
                                                               "  $(thead).remove();",
                                                               "}")
                                              )
      )
      

      #### START - Print selected information on the side  ----------------------------------
      output$specific_form_output = renderUI({
        s = input$all_matters_rows_selected
        fc <- values$fact_council()
        
        #document_list <- (as.data.table(list.files(path = "./www/documents",pattern = paste0(".*",as.character(filtereddataOverview()[s, matter_id]),".*"))))
        match_matter_id <- filtereddataOverview()[s, matter_id]
        council_comment <- fc[matter_id %in% match_matter_id][fc[matter_id %in% match_matter_id,.(which.max(timestamp)), by = matter_id]$V1][, .(council_decision, comment)]
       
        
        box(
          width = 12,
          title = paste0("Form information for ", filtereddataOverview()[s, matter_id]),
          fluidRow(
            column(6,
                   tags$p(
                     paste0('Showing details for application ', filtereddataOverview()[s, matter_id]),
                     tags$br(),
                     paste0('Form ID: ', filtereddataOverview()[s, matter_id]),
                     tags$br(),
                     paste0('Address: ', filtereddataOverview()[s, address]),
                     tags$br(),
                     paste0('Client: ', filtereddataOverview()[s, client_name]),
                     tags$br(),
                     paste0('Client contact number: ', filtereddataOverview()[s, client_contact_number]),
                     tags$br(),
                     paste0('Client email: ', filtereddataOverview()[s, client_email]),
                     tags$br(),
                     paste0('Last updated: ', filtereddataOverview()[s, format(last_modified_date,"%Y-%b-%d, %I:%M:%S%p")]),
                     tags$br()
                   )
                   ),
            column(6,
                   
                   actionButton(inputId = ns("approve_matter_button"), "Approve", icon = icon("check",style = "color: rgb(0,166,90)"), width = "100%"),
                   tags$br(),
                   actionButton(inputId = ns("reject_matter_button"), "Reject", icon = icon("xmark",style = "color: rgb(166,90,0)"), width = "100%"),
                   tags$hr(),
                   tags$strong("Latest comments:"),
                   box(
                     width = 12,
                     if(length(council_comment[[1]])>0){
                       paste0(council_comment[[1]],": ",council_comment[[2]]) 
                     }
                   )
                   )
          ),
          tags$strong("Uploaded Documents"),
          tags$p(
              actionButton(inputId = ns("view_document_button"),
                           label = "View selected document")
          ),
          tags$p(
            dataTableOutput(ns("documentTable"))
            
            ),
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
          tags$br(),
          tags$strong("Location"),
          tags$br(),
          leafletOutput(session$ns("mymap"))
        )
        
        
      })
      
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
          zoom = 10
        )
      })
      
      #Modal pop up when user clicks view document
      observeEvent(ns(input$view_document_button),{
        
        if(!is.null(input$documentTable_rows_selected)){
          showModal(modalDialog(
            title = document_list()[input$documentTable_rows_selected, V1],
            output$viewDocumentFrame <- renderUI({
              tags$iframe(src = paste0("./documents/",document_list()[input$documentTable_rows_selected, V1]),width = "100%",style='height:100vh;')
            }),
            easyClose = T
          ))
        }
        
      },ignoreInit = T)
      
      
      
      
      #### END - Print selected information on the side  ----------------------------------
    }
  )
  
}

