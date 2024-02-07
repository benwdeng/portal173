mod_portal173_ui <- function(id){
  # Defining namespace
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      h1("Records"),
      align = "center"
    ),
    fluidRow(
      valueBoxOutput(ns("stat_closed_matter_record"), width = 6)
    ),
    fluidRow(
      column(
        width = 6,
        h2("Closed matters"),
        fluidRow(
          uiOutput(ns("filter_matter_address_ui")),
          align = "center" 
        )
        ,
        fluidRow(
          dataTableOutput(ns("closed_matters")),
          align = "center"
        ),
      ),
      column(
        width = 6,
        #verbatimTextOutput(ns("specific_form_output"))
        conditionalPanel(
          condition = "typeof input.closed_matters_rows_selected !== 'undefined' && input.closed_matters_rows_selected.length > 0",
          ns = ns,
          uiOutput(ns("closed_matter_view"))
        )
      )
    ),
    approve_matter_ui(ns = ns),
    reject_matter_ui(ns = ns)
    
    #### END - Form section ----------------------------------
    
  )
}

mod_portal173_server <- function(id, values, users){
  
  moduleServer(
    id,
    function(input, output, session){
      ns <- NS(id)
      dataPostCodeMapping <- as.data.table(read.csv("inputs/australian_postcodes.csv"))
      
      output$stat_closed_matter_record <- renderValueBox({
        total_matter_count <- nrow(values$fact_matter()[status_of_matter == "5 Closed matter"])
        valueBox(
          paste0(total_matter_count), "closed matter", icon = icon("thumbs-up"),
          color = "green"
        )
      })
      
      
      address_list <- reactive({values$fact_matter()[status_of_matter == "5 Closed matter"]$address})
      
      
      output$filter_matter_address_ui <- renderUI({
        fluidRow(
          HTML("Select address to filter matters:"),
          selectInput(
            inputId = ns("filter_matter_address"),
            label = NULL,
            choices = append("All", address_list()),
            selected = "All"
          )
        )
      })

      
      #create closed matter table
      closed_fact_matters <- reactive({values$fact_matter()[status_of_matter == "5 Closed matter"][
        input$filter_matter_address == "All" | address == input$filter_matter_address
      ]})
      
      
      
      #### START - Show table ----------------------------------
      output$closed_matters <- renderDataTable({
        closed_fact_matters()[,
                               .("Matter ID" = matter_id,
                                 "Dealing number" = dealing_number,
                                 "Client Name" = client_name,
                                 "Subject land" = address,
                                 "Permit Expiry Date" = permit_expiry_date,
                                 "Type of form" = matter_type,
                                 "Last Updated" = format(last_modified_date,"%Y-%b-%d, %I:%M:%S%p"),
                                 "Status" = status_of_matter)
        ]
      }, selection = 'single', options = list(scrollX = TRUE,ordering = FALSE))
      
      
      #create conditional panel UI to view matter details and documents
      output$closed_matter_view = renderUI({
        s = input$closed_matters_rows_selected
        fc <- values$fact_council()
        
        match_matter_id <- closed_fact_matters()[s, matter_id]
        council_comment <- fc[matter_id %in% match_matter_id][fc[matter_id %in% match_matter_id,.(which.max(timestamp)), by = matter_id]$V1][, .(council_decision, comment)]
        
        document_list <- reactive({as.data.table(list.files(path = "./www/documents",pattern = paste0(".*",as.character(closed_fact_matters()[input$closed_matters_rows_selected, matter_id]),".*")))})
        
        output$documentTableClosed <- renderDataTable({document_list()},
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
        
        box(
          width = 12,
          title = paste0("Record of ", closed_fact_matters()[s, matter_id]," (DEALING_",closed_fact_matters()[s, matter_id],")"),
          fluidRow(
            column(6,
                   tags$p(
                     paste0('Showing details for matter ', closed_fact_matters()[s, matter_id]),
                     tags$br(),
                     paste0('Form ID: ', closed_fact_matters()[s, matter_id]),
                     tags$br(),
                     paste0('Address: ', closed_fact_matters()[s, address]),
                     tags$br(),
                     paste0('Client: ', closed_fact_matters()[s, client_name]),
                     tags$br(),
                     paste0('Client contact number: ', closed_fact_matters()[s, client_contact_number]),
                     tags$br(),
                     paste0('Client email: ', closed_fact_matters()[s, client_email]),
                     tags$br(),
                     paste0('Last updated: ', closed_fact_matters()[s, format(last_modified_date,"%Y-%b-%d, %I:%M:%S%p")]),
                     tags$br(),
                     paste0('Date lodged: ', closed_fact_matters()[s, format(last_modified_date,"%Y-%b-%d, %I:%M:%S%p")]),
                     tags$br()
                   )
            ),
            column(6,
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
            dataTableOutput(ns("documentTableClosed"))
            
          ),
          tags$p(
            paste0(
              'Certificate of Title: ', 
              if(!is.null(s) && 
                 nrow(values$fact_documents()[matter_id == closed_fact_matters()[s, matter_id] & 
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
                 nrow(values$fact_documents()[matter_id == closed_fact_matters()[s, matter_id] & 
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
        s = input$closed_matters_rows_selected
        
        setView(
          addMarkers(   
            addTiles(
              leaflet()
            ),
            data = dataPostCodeMapping[
              postcode %in% closed_fact_matters()[s, address_postcode], 
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
        
        if(!is.null(input$documentTableClosed_rows_selected)){
          showModal(modalDialog(
            title = document_list()[input$documentTableClosed_rows_selected, V1],
            output$viewDocumentFrame <- renderUI({
              tags$iframe(src = paste0("./documents/",document_list()[input$documentTableClosed_rows_selected, V1]),width = "100%",style='height:100vh;')
            }),
            easyClose = T
          ))
        }
        
      },ignoreInit = T)
    }
  )  
}
