###################################################################################################
#
# Portal 173 - Portal with the forms
#
###################################################################################################


mod_portal173_ui <- function(id){
  # Defining namespace
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      h1("Draft forms"),
      align = "center"
    ),
    
    fluidRow(
      align = "center",
      actionButton(
        inputId = ns("load_create_form"),
        label = "Create new form", 
        width = "20%"
      ),
      bsTooltip(
        id = ns("load_create_form"), 
        "Create a new form. WARNING - clears all forms, ensure you save your work", 
        placement = "left", trigger = "hover",
        options = NULL
      ),
      actionButton(
        inputId = ns("load_existing_form"),
        label = "View saved forms", 
        width = "20%"
      ),
      bsTooltip(
        id = ns("load_existing_form"), 
        "Get a list of all saved forms", 
        placement = "right", trigger = "hover",
        options = NULL
      )
    ),
    hr(),
    #### START - Saved forms ----------------------------------
    # uiOutput(ns("past_forms_box")),
    box(
      id = "past_forms_box",
      title = "Previously saved forms",
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      collapsed = TRUE,
      width = 12,
      fluidRow(
        column(
          width = 9,
          DT::dataTableOutput(ns("all_draft_forms"))
        ),
        column(
          width = 3,
          actionButton(
            inputId = ns("load_draft_form"),
            label = "Load saved form",
            width = "100%",
            icon = icon("spinner"),
            style = "font-size: 1vw"
          ),
          actionButton(
            inputId = ns("delete_draft_form"),
            label = "Delete saved form",
            width = "100%",
            icon = icon("times-circle"),
            style = "font-size: 1vw"
          ),
          downloadButton(
            outputId = ns("download_173_draft_agreement"), 
            label = "Download 173 draft", 
            style = "width:100%; font-size: 1vw; visibility: hidden;",
            icon = icon("download")
          ),
          downloadButton(
            outputId = ns("download_183_draft_agreement"), 
            label = "Download 183 application", 
            style = "width:100%; font-size: 1vw; visibility: hidden;",
            icon = icon("download")
          ),
          downloadButton(
            outputId = ns("download_181_draft_agreement"), 
            label = "Download 181 application", 
            style = "width:100%; font-size: 1vw; visibility: hidden;",
            icon = icon("download")
          )
        )
      )
    ),
    #### END - Saved forms ----------------------------------
    hr(),
    #### START - Form section ----------------------------------
    #uiOutput(ns("new_forms_box")),
    box(
      id = "new_forms_box",
      title = "Form",
      solidHeader = TRUE,
      status = "primary",
      collapsible = TRUE,
      collapsed = FALSE,
      width = 12,
      fluidRow(
        column(
          width = 6, offset = 2, 
          h2("Information for form"), br(),
          h6("* denotes required field", align = "right"),
          h4("Information about the matter"),
          uiOutput(ns("questions_key")),
          uiOutput(ns("questions_owner1")),
          uiOutput(ns("questions_owner2")),
          uiOutput(ns("questions_owner3")),
          uiOutput(ns("questions_owner4")),
          uiOutput(ns("questions_owner5")),
          hr(),
          h4("Certificate of Title"),
          uiOutput(ns("questions_cert_of_title_flag")),
          uiOutput(ns("questions_cert_of_title_info")),
          uiOutput(ns("info_cert_of_title_link")),
          hr(),
          h4("Development permit"),
          uiOutput(ns("questions_development_permit")),
          hr(),
          h4("Subdivision permit"),
          uiOutput(ns("questions_subdivision_permit")),
          hr(),
          h4("Other information"),
          uiOutput(ns("questions_mortgage_flag")),
          uiOutput(ns("questions_mortgage_name")),
          uiOutput(ns("questions_caveator_flag")),
          uiOutput(ns("questions_caveator_name"))
        ),
        column(
          width = 6, offset = 2, align = "center",
          uiOutput(ns("submit_matter_ui"))
        )
      )   
    ),
    #### END - Form section ----------------------------------
    
    mod_view_draft_ui(ns = ns)
  )
}

mod_portal173_server <- function(id, values, users){
  
  moduleServer(
    id,
    function(input, output, session){
      dataCouncilInfo <- data.table(read.csv(path_input_council_info, fileEncoding = 'UTF-8-BOM'))
      session$userData$downloadFile <- reactiveVal(NULL)
      
      # Sub-tabs ----------------------------------------------------------------
      
      eval(parse("modules/mod_portal173_view_draft.R"))
      
      
      #### START - Buttons to expand and collapse boxes ----------------------------------
      observeEvent(input$load_create_form, {
        js$collapseBox("past_forms_box")
        js$expandBox("new_forms_box")
        updateSelectizeInput(session, inputId = "matter_id_input", selected = "") 
        updateSelectizeInput(session, inputId = "matter_type_input", selected = "") 
        updateSelectizeInput(session, inputId = "council_name_input", selected = "") 
        updateTextAreaInput(session, inputId = "subject_address_input", value = "")
        updateSelectizeInput(session, inputId = "number_of_owners_input", selected = "") 
        updateRadioButtons(session, inputId = "owner1_company_flag_input", selected = character(0)) 
        updateRadioButtons(session, inputId = "owner2_company_flag_input", selected = character(0)) 
        updateRadioButtons(session, inputId = "owner3_company_flag_input", selected = character(0)) 
        updateRadioButtons(session, inputId = "owner4_company_flag_input", selected = character(0)) 
        updateRadioButtons(session, inputId = "owner5_company_flag_input", selected = character(0)) 
        updateTextAreaInput(session, inputId = "owner1_name_input", value = "")
        updateTextAreaInput(session, inputId = "owner1_address_input", value = "")
        updateTextAreaInput(session, inputId = "owner2_name_input", value = "")
        updateTextAreaInput(session, inputId = "owner2_address_input", value = "")
        updateTextAreaInput(session, inputId = "owner3_name_input", value = "")
        updateTextAreaInput(session, inputId = "owner3_address_input", value = "")
        updateTextAreaInput(session, inputId = "owner4_name_input", value = "")
        updateTextAreaInput(session, inputId = "owner4_address_input", value = "")
        updateTextAreaInput(session, inputId = "owner5_name_input", value = "")
        updateTextAreaInput(session, inputId = "owner5_address_input", value = "")
        
        updateTextAreaInput(session, inputId = "owner1_ACN_input", value = "")
        updateTextAreaInput(session, inputId = "owner2_ACN_input", value = "")
        updateTextAreaInput(session, inputId = "owner3_ACN_input", value = "")
        updateTextAreaInput(session, inputId = "owner4_ACN_input", value = "")
        updateTextAreaInput(session, inputId = "owner5_ACN_input", value = "")
        
        updateRadioButtons(session, inputId = "owner1_company_director_no_input", selected = character(0))
        updateRadioButtons(session, inputId = "owner2_company_director_no_input", selected = character(0))
        updateRadioButtons(session, inputId = "owner3_company_director_no_input", selected = character(0))
        updateRadioButtons(session, inputId = "owner4_company_director_no_input", selected = character(0))
        updateRadioButtons(session, inputId = "owner5_company_director_no_input", selected = character(0))
        
        updateRadioButtons(session, inputId = "cert_of_title_flag_input", selected = character(0))
        updateDateInput(session, inputId = "cert_of_title_date_issued_input", value = NULL)
        updateTextAreaInput(session, inputId = "cert_of_title_volume_input", value = "")
        updateTextAreaInput(session, inputId = "cert_of_title_folio_input", value = "")
        updateTextAreaInput(session, inputId = "develop_permit_number_input", value = "")
        updateDateInput(session, inputId = "develop_permit_issue_date_input", value = NULL)
        updateTextAreaInput(session, inputId = "develop_permit_condition_number_input", value = "")
        updateTextAreaInput(session, inputId = "subdivision_permit_number_input", value = "")
        updateDateInput(session, inputId = "subdivision_permit_issue_date_input", value = NULL)
        updateTextAreaInput(session, inputId = "subdivision_permit_condition_number_input", value = "")
        updateRadioButtons(session, inputId = "mortgage_flag_input", selected = character(0))
        updateTextAreaInput(session, inputId = "mortgage_name_input", value = "")
        updateRadioButtons(session, inputId = "caveator_flag_input", selected = character(0))
        updateTextAreaInput(session, inputId = "caveator_name_input", value = "")
        
        shinyjs::hide(id = "questions_mortgage_name")
        shinyjs::hide(id = "caveator_name_input")
      })
      
      observeEvent(input$load_existing_form, {
        js$collapseBox("new_forms_box")
        js$expandBox("past_forms_box")
      })
      #### END - Buttons to expand and collapse boxes ----------------------------------
      
      #### START - Show table ----------------------------------
      output$all_draft_forms <- renderDataTable({
        
        values$fact_forms()[
          # Remove permissions for now
          # (values$dim_user()[user %in% users$view_as_user(), role] == "Admin") |
            # (matter_id %in% values$dim_user_permission()[user %in% users$view_as_user(), matter_id])
          ,
          .("Matter ID" = matter_id,
            "Subject land" = subject_land_address,
            "Council" = council_name,
            "Draft 173 created" = draft_created,
            "181/183 created" = draft_183_created,
            "Last Updated" = format(last_modified_date,"%d %b %Y, %I:%M:%S%p")
          )

        ]
      }, selection = 'single', options = list(scrollX = TRUE))
      #### END - Show table ----------------------------------
      
      #### START - Show view draft button  ----------------------------------
      output$view_draft_ui <- renderUI({
        s = input$all_draft_forms_rows_selected
        if(!is.null(s)){
          entry <- values$fact_forms()[s]
          if(nrow(entry) > 0 && entry[, draft_created] == "Yes"){
            actionButton(
              inputId = session$ns("view_draft_agreement_button"), 
              label = "View draft", 
              width = "100%",
              icon = icon("eye"),
              style = "font-size: 1vw"
            )
          }
        }
      })
      #### END - Show view draft button  ----------------------------------
      
      #### START -Information to show relevant tabs in the form ---- 
      output$questions_key <- renderUI({
        fluidPage(
          # Form ID
          fluidRow(
            selectizeInput(
              inputId = session$ns("matter_id_input"), 
              label = "Matter ID: *", 
              choices = values$fact_matter()[, unique(matter_id)],
              multiple = FALSE,
              selected = "",
              width = "100%",
              options = list(placeholder = 'Select the relevant matter id', # Placeholder text
                             onInitialize = I('function() { this.setValue(""); }')),
            )
          ),
          
          fluidRow(
            selectizeInput(
              inputId = session$ns("matter_type_input"), 
              label = "Type of matter: ", 
              choices = type_of_agreement,
              multiple = FALSE,
              selected = "",
              width = "100%",
              options = list(placeholder = 'Select the type of matter', # Placeholder text
                             onInitialize = I('function() { this.setValue(""); }')),
            )
          ),
          
          fluidRow(
            selectizeInput(
              inputId = session$ns("council_name_input"), 
              label = "Council: *", 
              choices = dataCouncilInfo[, unique(Council)],
              multiple = FALSE,
              selected = "",
              width = "100%",
              options = list(placeholder = 'Select the relevant council', # Placeholder text
                             onInitialize = I('function() { this.setValue(""); }')),
            )
          ),
          fluidRow(
            textAreaInput(
              inputId = session$ns("subject_address_input"), 
              label = "Subject land address: *", 
              placeholder = "Enter the address of the subject land",
              width = "100%"
            )
          ),
          fluidRow(
            selectizeInput(
              inputId = session$ns("number_of_owners_input"), 
              label = "Number of owners: *",
              choices = 1:5,
              multiple = FALSE,
              selected = "",
              width = "100%",
              options = list(placeholder = 'Select the number of owners', # Placeholder text
                             onInitialize = I('function() { this.setValue(""); }')),
            )
          )
        )
      })
      
      output$questions_owner1 <- renderUI({
        fluidPage(
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner1_company_flag_input"), 
                label = "Is owner 1 a company or natural person", 
                choices = c("Company", "Natural person"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner1_name_input"), 
                label = "Full name of owner 1 *", 
                placeholder = "Enter the full name of owner 1",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner1_ACN_input"), 
                label = "Company ACN", 
                placeholder = "Enter company ACN",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner1_company_director_no_input"), 
                label = "How many company directors are there?", 
                choices = c("1 company director", "2 or more company directors"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner1_address_input"), 
                label = "Address of owner 1 *", 
                placeholder = "Enter the address of owner 1",
                width = "100%"
              )
            )
          )
        )
      })
      
      observeEvent(input$number_of_owners_input, {
        if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 1){
          shinyjs::show(id = "owner1_company_flag_input")
          shinyjs::show(id = "owner1_name_input")
          shinyjs::show(id = "owner1_address_input")
        } else {
          shinyjs::hide(id = "owner1_company_flag_input")
          shinyjs::hide(id = "owner1_name_input")
          shinyjs::hide(id = "owner1_address_input")
          shinyjs::hide(id = "owner1_ACN_input")
          shinyjs::hide(id = "owner1_company_director_no_input")
        }
      })
      
      observeEvent(input$owner1_company_flag_input, {
        if(isTRUE(length(input$owner1_company_flag_input) > 0) && input$owner1_company_flag_input == "Company"){
          shinyjs::show(id = "owner1_ACN_input")
          shinyjs::show(id = "owner1_company_director_no_input")
        } else {
          shinyjs::hide(id = "owner1_ACN_input")
          shinyjs::hide(id = "owner1_company_director_no_input")
        }
      })
      
      output$questions_owner2 <- renderUI({
        fluidPage(
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner2_company_flag_input"), 
                label = "Is owner 2 a company or natural person", 
                choices = c("Company", "Natural person"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner2_name_input"), 
                label = "Full name of owner 2 *", 
                placeholder = "Enter the full name of owner 2",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner2_ACN_input"), 
                label = "Company ACN", 
                placeholder = "Enter company ACN",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner2_company_director_no_input"), 
                label = "How many company directors are there?", 
                choices = c("1 company director", "2 or more company directors"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner2_address_input"), 
                label = "Address of owner 2 *", 
                placeholder = "Enter the address of owner 2",
                width = "100%"
              )
            )
          )
        )
      })
      
      observeEvent(input$number_of_owners_input, {
        if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 2){
          shinyjs::show(id = "owner2_company_flag_input")
          shinyjs::show(id = "owner2_name_input")
          shinyjs::show(id = "owner2_address_input")
        } else {
          shinyjs::hide(id = "owner2_company_flag_input")
          shinyjs::hide(id = "owner2_name_input")
          shinyjs::hide(id = "owner2_address_input")
          shinyjs::hide(id = "owner2_ACN_input")
          shinyjs::hide(id = "owner2_company_director_no_input")
        }
      })
      
      observeEvent(input$owner2_company_flag_input, {
        if(isTRUE(length(input$owner2_company_flag_input) > 0) && input$owner2_company_flag_input == "Company"){
          shinyjs::show(id = "owner2_ACN_input")
          shinyjs::show(id = "owner2_company_director_no_input")
        } else {
          shinyjs::hide(id = "owner2_ACN_input")
          shinyjs::hide(id = "owner2_company_director_no_input")
        }
      })
      
      output$questions_owner3 <- renderUI({
        fluidPage(
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner3_company_flag_input"), 
                label = "Is owner 3 a company or natural person", 
                choices = c("Company", "Natural person"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner3_name_input"), 
                label = "Full name of owner 3 *", 
                placeholder = "Enter the full name of owner 3",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner3_ACN_input"), 
                label = "Company ACN", 
                placeholder = "Enter company ACN",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner3_company_director_no_input"), 
                label = "How many company directors are there?", 
                choices = c("1 company director", "2 or more company directors"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner3_address_input"), 
                label = "Address of owner 3 *", 
                placeholder = "Enter the address of owner 3",
                width = "100%"
              )
            )
          )
        )
      })
      
      observeEvent(input$number_of_owners_input, {
        if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 3){
          shinyjs::show(id = "owner3_company_flag_input")
          shinyjs::show(id = "owner3_name_input")
          shinyjs::show(id = "owner3_address_input")
        } else {
          shinyjs::hide(id = "owner3_company_flag_input")
          shinyjs::hide(id = "owner3_name_input")
          shinyjs::hide(id = "owner3_address_input")
          shinyjs::hide(id = "owner3_ACN_input")
          shinyjs::hide(id = "owner3_company_director_no_input")
        }
      })
      
      observeEvent(input$owner3_company_flag_input, {
        if(isTRUE(length(input$owner3_company_flag_input) > 0) && input$owner3_company_flag_input == "Company"){
          shinyjs::show(id = "owner3_ACN_input")
          shinyjs::show(id = "owner3_company_director_no_input")
        } else {
          shinyjs::hide(id = "owner3_ACN_input")
          shinyjs::hide(id = "owner3_company_director_no_input")
        }
      })
      
      output$questions_owner4 <- renderUI({
        fluidPage(
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner4_company_flag_input"), 
                label = "Is owner 4 a company or natural person", 
                choices = c("Company", "Natural person"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner4_name_input"), 
                label = "Full name of owner 4 *", 
                placeholder = "Enter the full name of owner 4",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner4_ACN_input"), 
                label = "Company ACN", 
                placeholder = "Enter company ACN",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner4_company_director_no_input"), 
                label = "How many company directors are there?", 
                choices = c("1 company director", "2 or more company directors"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner4_address_input"), 
                label = "Address of owner 4 *", 
                placeholder = "Enter the address of owner 4",
                width = "100%"
              )
            )
          )
        )
      })
      
      observeEvent(input$number_of_owners_input, {
        if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 4){
          shinyjs::show(id = "owner4_company_flag_input")
          shinyjs::show(id = "owner4_name_input")
          shinyjs::show(id = "owner4_address_input")
        } else {
          shinyjs::hide(id = "owner4_company_flag_input")
          shinyjs::hide(id = "owner4_name_input")
          shinyjs::hide(id = "owner4_address_input")
          shinyjs::hide(id = "owner4_ACN_input")
          shinyjs::hide(id = "owner4_company_director_no_input")
        }
      })
      
      observeEvent(input$owner4_company_flag_input, {
        if(isTRUE(length(input$owner4_company_flag_input) > 0) && input$owner4_company_flag_input == "Company"){
          shinyjs::show(id = "owner4_ACN_input")
          shinyjs::show(id = "owner4_company_director_no_input")
        } else {
          shinyjs::hide(id = "owner4_ACN_input")
          shinyjs::hide(id = "owner4_company_director_no_input")
        }
      })
      
      output$questions_owner5 <- renderUI({
        fluidPage(
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner5_company_flag_input"), 
                label = "Is owner 5 a company or natural person", 
                choices = c("Company", "Natural person"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner5_name_input"), 
                label = "Full name of owner 5 *", 
                placeholder = "Enter the full name of owner 5",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner5_ACN_input"), 
                label = "Company ACN", 
                placeholder = "Enter company ACN",
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              radioButtons(
                inputId = session$ns("owner5_company_director_no_input"), 
                label = "How many company directors are there?", 
                choices = c("1 company director", "2 or more company directors"),
                selected = character(0),
                width = "100%"
              )
            )
          ),
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("owner5_address_input"), 
                label = "Address of owner 5 *", 
                placeholder = "Enter the address of owner 5",
                width = "100%"
              )
            )
          )
        )
      })
      
      observeEvent(input$number_of_owners_input, {
        if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 5){
          shinyjs::show(id = "owner5_company_flag_input")
          shinyjs::show(id = "owner5_name_input")
          shinyjs::show(id = "owner5_address_input")
        } else {
          shinyjs::hide(id = "owner5_company_flag_input")
          shinyjs::hide(id = "owner5_name_input")
          shinyjs::hide(id = "owner5_address_input")
          shinyjs::hide(id = "owner5_ACN_input")
          shinyjs::hide(id = "owner5_company_director_no_input")
        }
      })
      
      observeEvent(input$owner5_company_flag_input, {
        if(isTRUE(length(input$owner5_company_flag_input) > 0) && input$owner5_company_flag_input == "Company"){
          shinyjs::show(id = "owner5_ACN_input")
          shinyjs::show(id = "owner5_company_director_no_input")
        } else {
          shinyjs::hide(id = "owner5_ACN_input")
          shinyjs::hide(id = "owner5_company_director_no_input")
        }
      })
      
      observeEvent(input$number_of_owners_input, {
        if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 5){
          shinyjs::show(id = "owner5_name_input")
          shinyjs::show(id = "owner5_address_input")
        } else {
          shinyjs::hide(id = "owner5_name_input")
          shinyjs::hide(id = "owner5_address_input")
        }
      })
      
      # Information about the Certificate of Title
      output$questions_cert_of_title_flag <- renderUI({
        fluidPage(
          fluidRow(
            radioButtons(
              inputId = session$ns("cert_of_title_flag_input"), 
              label = "Do you have a copy of the Certificate of Title that was issued last than three months ago?", 
              choices = c("Yes", "No"),
              selected = character(0),
              width = "100%"
            )
          )
        )
      })
      
      output$questions_cert_of_title_info <- renderUI({
        fluidPage(
          fluidRow(
            hidden(
              dateInput(
                inputId = session$ns("cert_of_title_date_issued_input"), 
                label = "Certificate of Title issue date:", 
                value = NULL,
                format = "d M yyyy",
                startview = "month",
                width = "100%"
              )
            ),
            hidden(
              textAreaInput(
                inputId = session$ns("cert_of_title_volume_input"), 
                label = "Volume number", 
                placeholder = "Enter volume number on Certificate of Title",
                width = "100%"
              )
            ),
            hidden(
              textAreaInput(
                inputId = session$ns("cert_of_title_folio_input"), 
                label = "Folio number", 
                placeholder = "Enter folio number on Certificate of Title",
                width = "100%"
              )
            )
          )
        )
      })
      
      output$info_cert_of_title_link <- renderUI({
        if(isTRUE(length(input$cert_of_title_flag_input) > 0) && input$cert_of_title_flag_input == "No"){
          tagList(
            tags$p(style = "color:red",
              "Please obtain Certificate of Title. Note that you can get a Certificate of Title from ",
              tags$a(
                href = 'https://dyedurham.com.au/solutions/national-property-ownership/', 
                target = "_blank", "dyedurham.com.au"
              ),
              ".",
              "A Certificate of Title needs to be recent and needs to issued in the last three months.",
              "Note that there are costs associated with requesting for a Certificate of Title."
            )
          )
        }
      })
      
      observeEvent(input$cert_of_title_flag_input, {
        flog.debug(paste0("len", length(input$cert_of_title_flag_input)))
        if(isTRUE(length(input$cert_of_title_flag_input) > 0) && input$cert_of_title_flag_input == "Yes"){
          shinyjs::show(id = "cert_of_title_date_issued_input")
          shinyjs::show(id = "cert_of_title_volume_input")
          shinyjs::show(id = "cert_of_title_folio_input")
        } else {
          shinyjs::hide(id = "cert_of_title_date_issued_input")
          shinyjs::hide(id = "cert_of_title_volume_input")
          shinyjs::hide(id = "cert_of_title_folio_input")
        }
      })
      
      output$questions_development_permit <- renderUI({
        fluidPage(
          fluidRow(
            textAreaInput(
              inputId = session$ns("develop_permit_number_input"), 
              label = "Development permit number:", 
              placeholder = "Enter development permit number",
              width = "100%"
            )
          ),
          fluidRow(
            dateInput(
              inputId = session$ns("develop_permit_issue_date_input"), 
              label = "Development permit issue date:", 
              value = NULL,
              format = "d M yyyy",
              startview = "month",
              width = "100%"
            )
          ),
          fluidRow(
            textAreaInput(
              inputId = session$ns("develop_permit_condition_number_input"), 
              label = "Development permit condition number:", 
              placeholder = "Enter development permit condition number",
              width = "100%"
            )
          )
        )
      })
      
      output$questions_subdivision_permit <- renderUI({
        fluidPage(
          fluidRow(
            textAreaInput(
              inputId = session$ns("subdivision_permit_number_input"), 
              label = "Subdivision permit number:", 
              placeholder = "Enter subdivision permit number",
              width = "100%"
            )
          ),
          fluidRow(
            dateInput(
              inputId = session$ns("subdivision_permit_issue_date_input"), 
              label = "Subdivision permit issue date:", 
              value = NULL,
              format = "d M yyyy",
              startview = "month",
              width = "100%"
            )
          ),
          fluidRow(
            textAreaInput(
              inputId = session$ns("subdivision_permit_condition_number_input"), 
              label = "Subdivision permit condition number:", 
              placeholder = "Enter subdivision permit condition number",
              width = "100%"
            )
          )
        )
      })
      
      output$questions_mortgage_flag <- renderUI({
        fluidPage(
          fluidRow(
            radioButtons(
              inputId = session$ns("mortgage_flag_input"), 
              label = "Mortgage on the subject land:", 
              choices = c("Yes", "No"),
              selected = character(0),
              width = "100%"
            )
          )
        )
      })
      
      output$questions_mortgage_name <- renderUI({
        fluidPage(
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("mortgage_name_input"), 
                label = "Name of financial institution of mortgage:", 
                placeholder = "Enter name of financial institution of mortgage",
                width = "100%"
              )
            )
          )
        )
      })
      
      observeEvent(input$mortgage_flag_input, {
        if(isTRUE(length(input$mortgage_flag_input) > 0) && input$mortgage_flag_input == "Yes"){
          shinyjs::show(id = "mortgage_name_input")
        } else {
          shinyjs::hide(id = "mortgage_name_input")
        }
      })
      
      output$questions_caveator_flag <- renderUI({
        fluidPage(
          fluidRow(
            radioButtons(
              inputId = session$ns("caveator_flag_input"), 
              label = "Caveator on the subject land:", 
              choices = c("Yes", "No"),
              selected = character(0),
              width = "100%"
            )
          )
        )
      })
      
      output$questions_caveator_name <- renderUI({
        fluidPage(
          fluidRow(
            hidden(
              textAreaInput(
                inputId = session$ns("caveator_name_input"), 
                label = "Name of caveator on subject land:", 
                placeholder = "Enter name of caveator on subject land",
                width = "100%"
              )
            )
          )
        )
      })
      
      observeEvent(input$caveator_flag_input, {
        if(isTRUE(length(input$caveator_flag_input) > 0) && input$caveator_flag_input == "Yes"){
          shinyjs::show(id = "caveator_name_input")
        } else {
          shinyjs::hide(id = "caveator_name_input")
        }
      })
      #### END -Information to show relevant tabs in the form ---- 
      
      #### START - Information to show button to submit UI ----
      output$submit_matter_ui <- renderUI({
        fluidPage(
          fluidRow(
            actionButton(
              inputId = session$ns("save_form"), 
              label = "Save form", 
              width = "30%",
              icon = icon("save"),
              style = "font-size: 1vw"
            ),
            bsTooltip(
              id = session$ns("save_form"), 
              "Saves the information provided. A confirmation box should appear to ensure form is saved", 
              placement = "top", trigger = "hover",
              options = NULL
            ),
            actionButton(
              inputId = session$ns("generate_draft"), 
              label = "Draft 173 Agreement", 
              width = "30%",
              icon = icon("file-alt"),
              style = "font-size: 1vw"
           ),
           bsTooltip(
             id = session$ns("generate_draft"), 
             "Saves the form, generates the word document and downloads it for the user to view", 
             placement = "top", trigger = "hover",
             options = NULL
           ),
           if(isTRUE(length(input$matter_type_input) > 0) && input$matter_type_input == "Remove a section 173 agreement"){
             actionButton(
               inputId = session$ns("generate_draft_183"), 
               label = "Draft 183 Application", 
               width = "30%",
               icon = icon("file-alt"),
               style = "font-size: 1vw"
             )
           } else {
             actionButton(
               inputId = session$ns("generate_draft_181"), 
               label = "Draft 181 Application", 
               width = "30%",
               icon = icon("file-alt"),
               style = "font-size: 1vw"
             )
           },
           bsTooltip(
             id = session$ns("generate_draft_183"), 
             "A section 183 application only applies when type of lodgement is for a removal of a section 173 agreement", 
             placement = "top", trigger = "hover",
             options = NULL
           ),
           bsTooltip(
             id = session$ns("generate_draft_181"), 
             "Generates a section 181 application", 
             placement = "top", trigger = "hover",
             options = NULL
           )

          )
        )
      })
      #### END - Information to show button to submit UI ----
      
      #### START - What happens once user presses the generate draft button ----
      observeEvent(
        input$generate_draft, 
        {
          if(
            # If any of the required fields are empty, return an error
            !(input$matter_id_input == "" | input$council_name_input == "" | input$subject_address_input == "") 
          ){
            # Section copied and pasted from save form with one change in generate forms
            new_entry <- values$fact_matter()[matter_id == input$matter_id_input, unique_id]
            
            entry <- data.table(
              unique_id = new_entry,
              form_id = input$matter_id_input,
              matter_id = input$matter_id_input,
              matter_type = input$matter_type_input,
              subject_land_address = input$subject_address_input,
              council_name = input$council_name_input,
              no_of_owners = input$number_of_owners_input,
              owner_company_flag = paste(
                coalesceNULL(input$owner1_company_flag_input, ""), 
                coalesceNULL(input$owner2_company_flag_input, ""),
                coalesceNULL(input$owner3_company_flag_input, ""),
                coalesceNULL(input$owner4_company_flag_input, ""),
                coalesceNULL(input$owner5_company_flag_input, ""),
                sep = ",,,"
              ),
              owner_name = paste(
                input$owner1_name_input, 
                input$owner2_name_input,
                input$owner3_name_input,
                input$owner4_name_input,
                input$owner5_name_input,
                sep = ",,,"
              ),
              owner_address = paste(
                input$owner1_address_input,
                input$owner2_address_input,
                input$owner3_address_input,
                input$owner4_address_input,
                input$owner5_address_input,
                sep = ",,,"
              ),
              owner_ACN = paste(
                input$owner1_ACN_input,
                input$owner2_ACN_input,
                input$owner3_ACN_input,
                input$owner4_ACN_input,
                input$owner5_ACN_input,
                sep = ",,,"
              ),
              owner_company_director_no = paste(
                coalesceNULL(input$owner1_company_director_no_input, ""),
                coalesceNULL(input$owner2_company_director_no_input, ""),
                coalesceNULL(input$owner3_company_director_no_input, ""),
                coalesceNULL(input$owner4_company_director_no_input, ""),
                coalesceNULL(input$owner5_company_director_no_input, ""),
                sep = ",,,"
              ),
              cert_of_title_flag = ifelse(is.null(input$cert_of_title_flag_input), as.character(NA), input$cert_of_title_flag_input),
              cert_of_title_date_issued = coalesceNULL(input$cert_of_title_date_issued_input, as.Date(NA)), 
              cert_of_title_volume = ifelse(is.null(input$cert_of_title_volume_input), as.character(NA), input$cert_of_title_volume_input),
              cert_of_title_folio = ifelse(is.null(input$cert_of_title_folio_input), as.character(NA), input$cert_of_title_folio_input),
              develop_permit_number = input$develop_permit_number_input,
              develop_permit_issue_date = coalesceNULL(input$develop_permit_issue_date_input, as.Date(NA)),
              develop_permit_condition_number = input$develop_permit_condition_number_input,
              subdivision_permit_number = input$subdivision_permit_number_input,
              subdivision_permit_issue_date = coalesceNULL(input$subdivision_permit_issue_date_input, as.Date(NA)),
              subdivision_permit_condition_number = input$subdivision_permit_condition_number_input,
              mortgage_flag = ifelse(is.null(input$mortgage_flag_input), as.character(NA), input$mortgage_flag_input),
              mortgage_name = ifelse(is.null(input$mortgage_name_input), as.character(NA), input$mortgage_name_input),
              caveator_flag = ifelse(is.null(input$caveator_flag_input), as.character(NA), input$caveator_flag_input),
              caveator_name = ifelse(is.null(input$caveator_name_input), as.character(NA), input$caveator_name_input),
              draft_created = "Yes",
              draft_183_created = "No",
              created_date = coalesceLengthZero(
                values$fact_forms()[unique_id == new_entry, created_date],
                Sys.time()
              ),
              last_modified_date = Sys.time(),
              deleted_flag = "0",
              firestore_id = ifelse(
                nrow(values$fact_forms()[unique_id == new_entry]) > 0,
                values$fact_forms()[unique_id == new_entry, firestore_id],
                "Placeholder"
              )
            )
            
            dataJSON <- convertDTtoJSON(entry)
            
            if(nrow(values$fact_forms()[unique_id == new_entry]) > 0){
              # If there is an existing entry on firebase
              firestore_id_entry <- entry[, firestore_id]
              
              firestore_response <- sendJSONToFirestore(
                api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms, "/", firestore_id_entry),
                access_token = session$userData$current_user()$stsTokenManager$accessToken,
                data = dataJSON,
                replacement = TRUE
              )
            } else {
              # Create a new entry
              firestore_response <- sendJSONToFirestore(
                api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms),
                access_token = session$userData$current_user()$stsTokenManager$accessToken,
                data = dataJSON
              )
              
              # Add firestore_id
              firestore_id_entry <- getFirebaseIDFromPath(fromJSON(content(firestore_response,"text"))$name)
              entry[, firestore_id := firestore_id_entry]
            }
            
            adjusted_fact_forms <- rbind(values$fact_forms()[unique_id != new_entry], entry)
            saveRDS(object = adjusted_fact_forms, file = path_file_fact_forms)
            
            # Import document
            input_doc <- read_docx(path_input_template)

            # Replace words on the word document
            altered_doc <- body_replace_all_text(input_doc, "SubjectLand", input$subject_address_input, only_at_cursor = FALSE, warn = TRUE)
            altered_doc <- body_replace_all_text(altered_doc, "CouncilName", input$council_name_input, only_at_cursor = FALSE, warn = TRUE)
            
            # Owner 1
            if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 1){
              altered_doc <- body_replace_all_text(altered_doc, "Owner1Address", input$owner1_address_input, only_at_cursor = FALSE, warn = TRUE)
              
              if(isTRUE(length(input$owner1_company_flag_input) > 0) && input$owner1_company_flag_input == "Company"){
                # Remove signing for natural owner
                altered_doc <- cursor_reach(input_doc, keyword = "Signed, sealed and delivered by Owner1Name")
                altered_doc <- body_remove(altered_doc)
                # If company, then check whether it's a one person or two person company
                if(input$owner1_company_director_no_input == "1 company director"){
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner1Name2ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 1 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner1Name1ACN", 
                    paste0(input$owner1_name_input, " (", input$owner1_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                } else {
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner1Name1ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 2 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner1Name2ACN", 
                    paste0(input$owner1_name_input, " (", input$owner1_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                }
                
                # Replace rest of the owner 1 fields
                altered_doc <- body_replace_all_text(
                  altered_doc, "Owner1Name", 
                  paste0(input$owner1_name_input, " (", input$owner1_ACN_input, ")"), 
                  only_at_cursor = FALSE, warn = TRUE
                )
              } else {
                # Remove owner 1 signing page
                altered_doc <- cursor_reach(input_doc, keyword = "Owner1Name1ACN")
                altered_doc <- body_remove(altered_doc)
                altered_doc <- cursor_reach(input_doc, keyword = "Owner1Name2ACN")
                altered_doc <- body_remove(altered_doc)
                
                # Replace owner 1 with  owner name
                altered_doc <- body_replace_all_text(altered_doc, "Owner1Name", input$owner1_name_input, only_at_cursor = FALSE, warn = TRUE)
              }
            } else {
              for(i in 1:5){
                altered_doc <- cursor_reach(input_doc, keyword = "Owner1Name")
                altered_doc <- body_remove(altered_doc)
              }
            }
            
            # Owner 2
            if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 2){
              altered_doc <- body_replace_all_text(altered_doc, "Owner2Address", input$owner2_address_input, only_at_cursor = FALSE, warn = TRUE)
              
              if(isTRUE(length(input$owner2_company_flag_input) > 0) && input$owner2_company_flag_input == "Company"){
                # Remove signing for natural owner
                altered_doc <- cursor_reach(input_doc, keyword = "Signed, sealed and delivered by Owner2Name")
                altered_doc <- body_remove(altered_doc)
                # If company, then check whether it's a one person or two person company
                if(input$owner2_company_director_no_input == "1 company director"){
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner2Name2ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 1 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner2Name1ACN", 
                    paste0(input$owner2_name_input, " (", input$owner2_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                } else {
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner2Name1ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 2 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner2Name2ACN", 
                    paste0(input$owner2_name_input, " (", input$owner2_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                }
                
                # Replace rest of the owner 2 fields
                altered_doc <- body_replace_all_text(
                  altered_doc, "Owner2Name", 
                  paste0(input$owner2_name_input, " (", input$owner2_ACN_input, ")"), 
                  only_at_cursor = FALSE, warn = TRUE
                )
              } else {
                # Remove owner 2 signing page
                altered_doc <- cursor_reach(input_doc, keyword = "Owner2Name1ACN")
                altered_doc <- body_remove(altered_doc)
                altered_doc <- cursor_reach(input_doc, keyword = "Owner2Name2ACN")
                altered_doc <- body_remove(altered_doc)
                
                # Replace owner 2 with  owner name
                altered_doc <- body_replace_all_text(altered_doc, "Owner2Name", input$owner2_name_input, only_at_cursor = FALSE, warn = TRUE)
              }
            } else {
              for(i in 1:5){
                altered_doc <- cursor_reach(input_doc, keyword = "Owner2Name")
                altered_doc <- body_remove(altered_doc)
              }
            }
            
            # Owner 3
            if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 3){
              altered_doc <- body_replace_all_text(altered_doc, "Owner3Address", input$owner3_address_input, only_at_cursor = FALSE, warn = TRUE)
              
              if(isTRUE(length(input$owner3_company_flag_input) > 0) && input$owner3_company_flag_input == "Company"){
                # Remove signing for natural owner
                altered_doc <- cursor_reach(input_doc, keyword = "Signed, sealed and delivered by Owner3Name")
                altered_doc <- body_remove(altered_doc)
                # If company, then check whether it's a one person or two person company
                if(input$owner3_company_director_no_input == "1 company director"){
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner3Name2ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 1 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner3Name1ACN", 
                    paste0(input$owner3_name_input, " (", input$owner3_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                } else {
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner3Name1ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 2 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner3Name2ACN", 
                    paste0(input$owner3_name_input, " (", input$owner3_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                }
                
                # Replace rest of the owner 3 fields
                altered_doc <- body_replace_all_text(
                  altered_doc, "Owner3Name", 
                  paste0(input$owner3_name_input, " (", input$owner3_ACN_input, ")"), 
                  only_at_cursor = FALSE, warn = TRUE
                )
              } else {
                # Remove owner 3 signing page
                altered_doc <- cursor_reach(input_doc, keyword = "Owner3Name1ACN")
                altered_doc <- body_remove(altered_doc)
                altered_doc <- cursor_reach(input_doc, keyword = "Owner3Name2ACN")
                altered_doc <- body_remove(altered_doc)
                
                # Replace owner 3 with  owner name
                altered_doc <- body_replace_all_text(altered_doc, "Owner3Name", input$owner3_name_input, only_at_cursor = FALSE, warn = TRUE)
              }
            } else {
              for(i in 1:5){
                altered_doc <- cursor_reach(input_doc, keyword = "Owner3Name")
                altered_doc <- body_remove(altered_doc)
              }
            }
            
            # Owner 4
            if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 4){
              altered_doc <- body_replace_all_text(altered_doc, "Owner4Address", input$owner4_address_input, only_at_cursor = FALSE, warn = TRUE)
              
              if(isTRUE(length(input$owner4_company_flag_input) > 0) && input$owner4_company_flag_input == "Company"){
                # Remove signing for natural owner
                altered_doc <- cursor_reach(input_doc, keyword = "Signed, sealed and delivered by Owner4Name")
                altered_doc <- body_remove(altered_doc)
                # If company, then check whether it's a one person or two person company
                if(input$owner4_company_director_no_input == "1 company director"){
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner4Name2ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 1 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner4Name1ACN", 
                    paste0(input$owner4_name_input, " (", input$owner4_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                } else {
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner4Name1ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 2 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner4Name2ACN", 
                    paste0(input$owner4_name_input, " (", input$owner4_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                }
                
                # Replace rest of the owner 4 fields
                altered_doc <- body_replace_all_text(
                  altered_doc, "Owner4Name", 
                  paste0(input$owner4_name_input, " (", input$owner4_ACN_input, ")"), 
                  only_at_cursor = FALSE, warn = TRUE
                )
              } else {
                # Remove owner 4 signing page
                altered_doc <- cursor_reach(input_doc, keyword = "Owner4Name1ACN")
                altered_doc <- body_remove(altered_doc)
                altered_doc <- cursor_reach(input_doc, keyword = "Owner4Name2ACN")
                altered_doc <- body_remove(altered_doc)
                
                # Replace owner 4 with  owner name
                altered_doc <- body_replace_all_text(altered_doc, "Owner4Name", input$owner4_name_input, only_at_cursor = FALSE, warn = TRUE)
              }
            } else {
              for(i in 1:5){
                altered_doc <- cursor_reach(input_doc, keyword = "Owner4Name")
                altered_doc <- body_remove(altered_doc)
              }
            }
            
            # Owner 5
            if(isTRUE(length(input$number_of_owners_input) > 0) && input$number_of_owners_input >= 5){
              altered_doc <- body_replace_all_text(altered_doc, "Owner5Address", input$owner5_address_input, only_at_cursor = FALSE, warn = TRUE)
              
              if(isTRUE(length(input$owner5_company_flag_input) > 0) && input$owner5_company_flag_input == "Company"){
                # Remove signing for natural owner
                altered_doc <- cursor_reach(input_doc, keyword = "Signed, sealed and delivered by Owner5Name")
                altered_doc <- body_remove(altered_doc)
                # If company, then check whether it's a one person or two person company
                if(input$owner5_company_director_no_input == "1 company director"){
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner5Name2ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 1 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner5Name1ACN", 
                    paste0(input$owner5_name_input, " (", input$owner5_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                } else {
                  altered_doc <- cursor_reach(input_doc, keyword = "EXECUTED by Owner5Name1ACN")
                  altered_doc <- body_remove(altered_doc)
                  
                  # Keep signing page for 2 owner company
                  altered_doc <- body_replace_all_text(
                    altered_doc, "Owner5Name2ACN", 
                    paste0(input$owner5_name_input, " (", input$owner5_ACN_input, ")"), 
                    only_at_cursor = FALSE, warn = TRUE, fixed = TRUE
                  )
                }
                
                # Replace rest of the owner 5 fields
                altered_doc <- body_replace_all_text(
                  altered_doc, "Owner5Name", 
                  paste0(input$owner5_name_input, " (", input$owner5_ACN_input, ")"), 
                  only_at_cursor = FALSE, warn = TRUE
                )
              } else {
                # Remove owner 5 signing page
                altered_doc <- cursor_reach(input_doc, keyword = "Owner5Name1ACN")
                altered_doc <- body_remove(altered_doc)
                altered_doc <- cursor_reach(input_doc, keyword = "Owner5Name2ACN")
                altered_doc <- body_remove(altered_doc)
                
                # Replace owner 5 with  owner name
                altered_doc <- body_replace_all_text(altered_doc, "Owner5Name", input$owner5_name_input, only_at_cursor = FALSE, warn = TRUE)
              }
            } else {
              for(i in 1:5){
                altered_doc <- cursor_reach(input_doc, keyword = "Owner5Name")
                altered_doc <- body_remove(altered_doc)
              }
            }
            
            altered_doc <- body_replace_all_text(altered_doc, "CoTVolumeNumber", input$cert_of_title_volume_input, only_at_cursor = FALSE, warn = TRUE)
            altered_doc <- body_replace_all_text(altered_doc, "CoTFolioNumber", input$cert_of_title_folio_input, only_at_cursor = FALSE, warn = TRUE)
            
            altered_doc <- body_replace_all_text(altered_doc, "DevelopmentPermitNumber", input$develop_permit_number_input, only_at_cursor = FALSE, warn = TRUE)
            altered_doc <- body_replace_all_text(altered_doc, "DevelopmentPermitIssueDate", format(input$develop_permit_issue_date_input, "%Y-%b-%d"), only_at_cursor = FALSE, warn = TRUE)
            
            altered_doc <- body_replace_all_text(altered_doc, "SubdivisionPermitNumber", input$subdivision_permit_number_input, only_at_cursor = FALSE, warn = TRUE)
            altered_doc <- body_replace_all_text(altered_doc, "SubdivisionPermitIssueDate", format(input$subdivision_permit_issue_date_input, "%Y-%b-%d"), only_at_cursor = FALSE, warn = TRUE)
            altered_doc <- body_replace_all_text(altered_doc, "SubdivisionPermitCondition", input$subdivision_permit_condition_number_input, only_at_cursor = FALSE, warn = TRUE)
            
            # Remove section if no mortgage
            if(isTRUE(length(input$mortgage_flag_input) > 0) && input$mortgage_flag_input == "Yes"){
              altered_doc <- body_replace_all_text(altered_doc, "MortgageInstitution", input$mortgage_name_input, only_at_cursor = FALSE, warn = TRUE)
            } else {
              altered_doc <- cursor_reach(input_doc, keyword = "Mortgagee's Consent")
              altered_doc <- body_remove(altered_doc)
            }
            
            # Remove section if no caveator
            if(isTRUE(length(input$caveator_flag_input) > 0) && input$caveator_flag_input == "Yes"){
              altered_doc <- body_replace_all_text(altered_doc, "CaveatorName", input$caveator_name_input, only_at_cursor = FALSE, warn = TRUE)
            } else {
              altered_doc <- cursor_reach(input_doc, keyword = "registered Caveator under instrument")
              altered_doc <- body_remove(altered_doc)
            }
            
            # Replace council fields with look ups
            altered_doc <- body_replace_all_text(altered_doc, "CouncilAddress", dataCouncilInfo[Council == input$council_name_input,  paste0(ADDRESS, " ", SUBURB..TOWN)], only_at_cursor = FALSE, warn = TRUE)
            altered_doc <- body_replace_all_text(altered_doc, "CouncilEmail", dataCouncilInfo[Council == input$council_name_input, EMAIL], only_at_cursor = FALSE, warn = TRUE)
            altered_doc <- body_replace_all_text(altered_doc, "CouncilSignoffClause", dataCouncilInfo[Council == input$council_name_input, SignoffClause], only_at_cursor = FALSE, warn = TRUE)
            
            # Save the draft agreement
            titleOfFIle <- paste0(input$matter_id_input, " Section 173 Agreement draft.docx")
            pathToSaveFile <- paste0(path_output_draft, titleOfFIle)
            print(altered_doc, target = pathToSaveFile)
            
            shinyalert(paste0("Draft section 173 agreement generated"), type = "success")
            
            documentLocation <- list(location = pathToSaveFile,
                                     title = titleOfFIle)
            session$userData$downloadFile(documentLocation)
            click("portal173_tab-download_173_draft_agreement", asis = TRUE)
          } else {
            shinyalert(paste0("Not all required fields complete"), type = "warning")
          }
        }
      )
      #### END - What happens once user presses the generate draft button ----
      
      #### START - What happens once user presses the generate draft 181 button ----
      observeEvent(
        input$generate_draft_181, 
        {
          if(
            # If any of the required fields are empty, return an error
            !(input$matter_id_input == "" | input$council_name_input == "" | input$subject_address_input == "") 
          ){
            # Section copied and pasted from save form with one change in generate forms
            new_entry <- values$fact_matter()[matter_id == input$matter_id_input, unique_id]
            
            entry <- data.table(
              unique_id = new_entry,
              form_id = input$matter_id_input,
              matter_id = input$matter_id_input,
              matter_type = input$matter_type_input,
              subject_land_address = input$subject_address_input,
              council_name = input$council_name_input,
              no_of_owners = input$number_of_owners_input,
              owner_company_flag = paste(
                coalesceNULL(input$owner1_company_flag_input, ""), 
                coalesceNULL(input$owner2_company_flag_input, ""),
                coalesceNULL(input$owner3_company_flag_input, ""),
                coalesceNULL(input$owner4_company_flag_input, ""),
                coalesceNULL(input$owner5_company_flag_input, ""),
                sep = ",,,"
              ),
              owner_name = paste(
                input$owner1_name_input, 
                input$owner2_name_input,
                input$owner3_name_input,
                input$owner4_name_input,
                input$owner5_name_input,
                sep = ",,,"
              ),
              owner_address = paste(
                input$owner1_address_input,
                input$owner2_address_input,
                input$owner3_address_input,
                input$owner4_address_input,
                input$owner5_address_input,
                sep = ",,,"
              ),
              owner_ACN = paste(
                input$owner1_ACN_input,
                input$owner2_ACN_input,
                input$owner3_ACN_input,
                input$owner4_ACN_input,
                input$owner5_ACN_input,
                sep = ",,,"
              ),
              owner_company_director_no = paste(
                coalesceNULL(input$owner1_company_director_no_input, ""),
                coalesceNULL(input$owner2_company_director_no_input, ""),
                coalesceNULL(input$owner3_company_director_no_input, ""),
                coalesceNULL(input$owner4_company_director_no_input, ""),
                coalesceNULL(input$owner5_company_director_no_input, ""),
                sep = ",,,"
              ),
              cert_of_title_flag = ifelse(is.null(input$cert_of_title_flag_input), as.character(NA), input$cert_of_title_flag_input),
              cert_of_title_date_issued = coalesceNULL(input$cert_of_title_date_issued_input, as.Date(NA)), 
              cert_of_title_volume = ifelse(is.null(input$cert_of_title_volume_input), as.character(NA), input$cert_of_title_volume_input),
              cert_of_title_folio = ifelse(is.null(input$cert_of_title_folio_input), as.character(NA), input$cert_of_title_folio_input),
              develop_permit_number = input$develop_permit_number_input,
              develop_permit_issue_date = coalesceNULL(input$develop_permit_issue_date_input, as.Date(NA)),
              develop_permit_condition_number = input$develop_permit_condition_number_input,
              subdivision_permit_number = input$subdivision_permit_number_input,
              subdivision_permit_issue_date = coalesceNULL(input$subdivision_permit_issue_date_input, as.Date(NA)),
              subdivision_permit_condition_number = input$subdivision_permit_condition_number_input,
              mortgage_flag = ifelse(is.null(input$mortgage_flag_input), as.character(NA), input$mortgage_flag_input),
              mortgage_name = ifelse(is.null(input$mortgage_name_input), as.character(NA), input$mortgage_name_input),
              caveator_flag = ifelse(is.null(input$caveator_flag_input), as.character(NA), input$caveator_flag_input),
              caveator_name = ifelse(is.null(input$caveator_name_input), as.character(NA), input$caveator_name_input),
              draft_created = "Yes",
              draft_183_created = "No",
              created_date = coalesceLengthZero(
                values$fact_forms()[unique_id == new_entry, created_date],
                Sys.time()
              ),
              last_modified_date = Sys.time(),
              deleted_flag = "0",
              firestore_id = ifelse(
                nrow(values$fact_forms()[unique_id == new_entry]) > 0,
                values$fact_forms()[unique_id == new_entry, firestore_id],
                "Placeholder"
              )
            )
            
            dataJSON <- convertDTtoJSON(entry)
            
            if(nrow(values$fact_forms()[unique_id == new_entry]) > 0){
              # If there is an existing entry on firebase
              firestore_id_entry <- entry[, firestore_id]
              
              firestore_response <- sendJSONToFirestore(
                api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms, "/", firestore_id_entry),
                access_token = session$userData$current_user()$stsTokenManager$accessToken,
                data = dataJSON,
                replacement = TRUE
              )
            } else {
              # Create a new entry
              firestore_response <- sendJSONToFirestore(
                api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms),
                access_token = session$userData$current_user()$stsTokenManager$accessToken,
                data = dataJSON
              )
              
              # Add firestore_id
              firestore_id_entry <- getFirebaseIDFromPath(fromJSON(content(firestore_response,"text"))$name)
              entry[, firestore_id := firestore_id_entry]
            }
            
            adjusted_fact_forms <- rbind(values$fact_forms()[unique_id != new_entry], entry)
            saveRDS(object = adjusted_fact_forms, file = path_file_fact_forms)
            
            # Import document
            input_doc <- read_docx(path_input_181_template)
            
            # Replace words on the word document
            altered_doc <- body_replace_all_text(input_doc, "CouncilName", input$council_name_input, only_at_cursor = FALSE, warn = TRUE)
            
            altered_doc <- body_replace_all_text(altered_doc, "CoTVolumeNumber", input$cert_of_title_volume_input, only_at_cursor = FALSE, warn = TRUE)
            altered_doc <- body_replace_all_text(altered_doc, "CoTFolioNumber", input$cert_of_title_folio_input, only_at_cursor = FALSE, warn = TRUE)
            
            # Replace council fields with look ups
            altered_doc <- body_replace_all_text(altered_doc, "CouncilAddress", dataCouncilInfo[Council == input$council_name_input,  paste0(ADDRESS, " ", SUBURB..TOWN)], only_at_cursor = FALSE, warn = TRUE)
            
            # Save the draft agreement
            titleOfFIle <- paste0(input$matter_id_input, " Section 181 Agreement draft.docx")
            pathToSaveFile <- paste0(path_output_draft, titleOfFIle)
            print(altered_doc, target = pathToSaveFile)
            
            shinyalert(paste0("Draft section 181 agreement generated"), type = "success")
            
            documentLocation <- list(location = pathToSaveFile,
                                     title = titleOfFIle)
            session$userData$downloadFile(documentLocation)
            click("portal173_tab-download_181_draft_agreement", asis = TRUE)
          } else {
            shinyalert(paste0("Not all required fields complete"), type = "warning")
          }
        }
      )
      #### END - What happens once user presses the generate draft 181 button ----
      
      #### START - What happens once user presses the generate draft 183 button ----
      observeEvent(
        input$generate_draft_183, 
        {
          if(
            # If any of the required fields are empty, return an error
            !(input$matter_id_input == "" | input$council_name_input == "" | input$subject_address_input == "") 
          ){
            # Section copied and pasted from save form with one change in generate forms
            new_entry <- values$fact_matter()[matter_id == input$matter_id_input, unique_id]
            
            entry <- data.table(
              unique_id = new_entry,
              form_id = input$matter_id_input,
              matter_id = input$matter_id_input,
              matter_type = input$matter_type_input,
              subject_land_address = input$subject_address_input,
              council_name = input$council_name_input,
              no_of_owners = input$number_of_owners_input,
              owner_company_flag = paste(
                coalesceNULL(input$owner1_company_flag_input, ""), 
                coalesceNULL(input$owner2_company_flag_input, ""),
                coalesceNULL(input$owner3_company_flag_input, ""),
                coalesceNULL(input$owner4_company_flag_input, ""),
                coalesceNULL(input$owner5_company_flag_input, ""),
                sep = ",,,"
              ),
              owner_name = paste(
                input$owner1_name_input, 
                input$owner2_name_input,
                input$owner3_name_input,
                input$owner4_name_input,
                input$owner5_name_input,
                sep = ",,,"
              ),
              owner_address = paste(
                input$owner1_address_input,
                input$owner2_address_input,
                input$owner3_address_input,
                input$owner4_address_input,
                input$owner5_address_input,
                sep = ",,,"
              ),
              owner_ACN = paste(
                input$owner1_ACN_input,
                input$owner2_ACN_input,
                input$owner3_ACN_input,
                input$owner4_ACN_input,
                input$owner5_ACN_input,
                sep = ",,,"
              ),
              owner_company_director_no = paste(
                coalesceNULL(input$owner1_company_director_no_input, ""),
                coalesceNULL(input$owner2_company_director_no_input, ""),
                coalesceNULL(input$owner3_company_director_no_input, ""),
                coalesceNULL(input$owner4_company_director_no_input, ""),
                coalesceNULL(input$owner5_company_director_no_input, ""),
                sep = ",,,"
              ),
              cert_of_title_flag = ifelse(is.null(input$cert_of_title_flag_input), as.character(NA), input$cert_of_title_flag_input),
              cert_of_title_date_issued = coalesceNULL(input$cert_of_title_date_issued_input, as.Date(NA)), 
              cert_of_title_volume = ifelse(is.null(input$cert_of_title_volume_input), as.character(NA), input$cert_of_title_volume_input),
              cert_of_title_folio = ifelse(is.null(input$cert_of_title_folio_input), as.character(NA), input$cert_of_title_folio_input),
              develop_permit_number = input$develop_permit_number_input,
              develop_permit_issue_date = coalesceNULL(input$develop_permit_issue_date_input, as.Date(NA)),
              develop_permit_condition_number = input$develop_permit_condition_number_input,
              subdivision_permit_number = input$subdivision_permit_number_input,
              subdivision_permit_issue_date = coalesceNULL(input$subdivision_permit_issue_date_input, as.Date(NA)),
              subdivision_permit_condition_number = input$subdivision_permit_condition_number_input,
              mortgage_flag = ifelse(is.null(input$mortgage_flag_input), as.character(NA), input$mortgage_flag_input),
              mortgage_name = ifelse(is.null(input$mortgage_name_input), as.character(NA), input$mortgage_name_input),
              caveator_flag = ifelse(is.null(input$caveator_flag_input), as.character(NA), input$caveator_flag_input),
              caveator_name = ifelse(is.null(input$caveator_name_input), as.character(NA), input$caveator_name_input),
              draft_created = "Yes",
              draft_183_created = "Yes",
              created_date = coalesceLengthZero(
                values$fact_forms()[unique_id == new_entry, created_date],
                Sys.time()
              ),
              last_modified_date = Sys.time(),
              deleted_flag = "0",
              firestore_id = ifelse(
                nrow(values$fact_forms()[unique_id == new_entry]) > 0,
                values$fact_forms()[unique_id == new_entry, firestore_id],
                "Placeholder"
              )
            )
            
            dataJSON <- convertDTtoJSON(entry)
            
            if(nrow(values$fact_forms()[unique_id == new_entry]) > 0){
              # If there is an existing entry on firebase
              firestore_id_entry <- entry[, firestore_id]
              
              firestore_response <- sendJSONToFirestore(
                api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms, "/", firestore_id_entry),
                access_token = session$userData$current_user()$stsTokenManager$accessToken,
                data = dataJSON,
                replacement = TRUE
              )
            } else {
              # Create a new entry
              firestore_response <- sendJSONToFirestore(
                api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms),
                access_token = session$userData$current_user()$stsTokenManager$accessToken,
                data = dataJSON
              )
              
              # Add firestore_id
              firestore_id_entry <- getFirebaseIDFromPath(fromJSON(content(firestore_response,"text"))$name)
              entry[, firestore_id := firestore_id_entry]
            }
            
            adjusted_fact_forms <- rbind(values$fact_forms()[unique_id != new_entry], entry)
            saveRDS(object = adjusted_fact_forms, file = path_file_fact_forms)
            
            # Import document
            input_doc <- read_docx(path_input_183_template)
            
            # Replace words on the word document
            altered_doc <- body_replace_all_text(input_doc, "CouncilName", input$council_name_input, only_at_cursor = FALSE, warn = TRUE)
            
            altered_doc <- body_replace_all_text(altered_doc, "CoTVolumeNumber", input$cert_of_title_volume_input, only_at_cursor = FALSE, warn = TRUE)
            altered_doc <- body_replace_all_text(altered_doc, "CoTFolioNumber", input$cert_of_title_folio_input, only_at_cursor = FALSE, warn = TRUE)
            
            # Replace council fields with look ups
            altered_doc <- body_replace_all_text(altered_doc, "CouncilAddress", dataCouncilInfo[Council == input$council_name_input,  paste0(ADDRESS, " ", SUBURB..TOWN)], only_at_cursor = FALSE, warn = TRUE)

            # Save the draft agreement
            titleOfFIle <- paste0(input$matter_id_input, " Section 183 Agreement draft.docx")
            pathToSaveFile <- paste0(path_output_draft, titleOfFIle)
            print(altered_doc, target = pathToSaveFile)
            
            shinyalert(paste0("Draft section 183 agreement generated"), type = "success")
            
            documentLocation <- list(location = pathToSaveFile,
                                     title = titleOfFIle)
            session$userData$downloadFile(documentLocation)
            click("portal173_tab-download_183_draft_agreement", asis = TRUE)
          } else {
            shinyalert(paste0("Not all required fields complete"), type = "warning")
          }
        }
      )
      #### END - What happens once user presses the generate draft 183 button ----
      
      #### START - What happens once user presses the save form button ----
      observeEvent(
        input$save_form,
        {
          if(
            # If any of the required fields are empty, return an error
            !(input$matter_id_input == "" | input$council_name_input == "" | input$subject_address_input == "" |
              input$number_of_owners_input == "") 
          ){
            new_entry <- values$fact_matter()[matter_id == input$matter_id_input, unique_id]
            
            entry <- data.table(
              unique_id = new_entry,
              form_id = paste0("S173",sprintf("%04d", new_entry)),
              matter_id = input$matter_id_input,
              matter_type = input$matter_type_input,
              subject_land_address = input$subject_address_input,
              council_name = input$council_name_input,
              no_of_owners = input$number_of_owners_input,
              owner_company_flag = paste(
                coalesceNULL(input$owner1_company_flag_input, ""), 
                coalesceNULL(input$owner2_company_flag_input, ""),
                coalesceNULL(input$owner3_company_flag_input, ""),
                coalesceNULL(input$owner4_company_flag_input, ""),
                coalesceNULL(input$owner5_company_flag_input, ""),
                sep = ",,,"
              ),
              owner_name = paste(
                input$owner1_name_input, 
                input$owner2_name_input,
                input$owner3_name_input,
                input$owner4_name_input,
                input$owner5_name_input,
                sep = ",,,"
              ),
              owner_address = paste(
                input$owner1_address_input,
                input$owner2_address_input,
                input$owner3_address_input,
                input$owner4_address_input,
                input$owner5_address_input,
                sep = ",,,"
              ),
              owner_ACN = paste(
                input$owner1_ACN_input,
                input$owner2_ACN_input,
                input$owner3_ACN_input,
                input$owner4_ACN_input,
                input$owner5_ACN_input,
                sep = ",,,"
              ),
              owner_company_director_no = paste(
                coalesceNULL(input$owner1_company_director_no_input, ""),
                coalesceNULL(input$owner2_company_director_no_input, ""),
                coalesceNULL(input$owner3_company_director_no_input, ""),
                coalesceNULL(input$owner4_company_director_no_input, ""),
                coalesceNULL(input$owner5_company_director_no_input, ""),
                sep = ",,,"
              ),
              cert_of_title_flag = ifelse(is.null(input$cert_of_title_flag_input), as.character(NA), input$cert_of_title_flag_input),
              cert_of_title_date_issued = coalesceNULL(input$cert_of_title_date_issued_input, as.Date(NA)), 
              cert_of_title_volume = ifelse(is.null(input$cert_of_title_volume_input), as.character(NA), input$cert_of_title_volume_input),
              cert_of_title_folio = ifelse(is.null(input$cert_of_title_folio_input), as.character(NA), input$cert_of_title_folio_input),
              develop_permit_number = input$develop_permit_number_input,
              develop_permit_issue_date = coalesceNULL(input$develop_permit_issue_date_input, as.Date(NA)),
              develop_permit_condition_number = input$develop_permit_condition_number_input,
              subdivision_permit_number = input$subdivision_permit_number_input,
              subdivision_permit_issue_date = coalesceNULL(input$subdivision_permit_issue_date_input, as.Date(NA)),
              subdivision_permit_condition_number = input$subdivision_permit_condition_number_input,
              mortgage_flag = ifelse(is.null(input$mortgage_flag_input), as.character(NA), input$mortgage_flag_input),
              mortgage_name = ifelse(is.null(input$mortgage_name_input), as.character(NA), input$mortgage_name_input),
              caveator_flag = ifelse(is.null(input$caveator_flag_input), as.character(NA), input$caveator_flag_input),
              caveator_name = ifelse(is.null(input$caveator_name_input), as.character(NA), input$caveator_name_input),
              draft_created = "No",
              draft_183_created = "No",
              created_date = coalesceLengthZero(
                values$fact_forms()[unique_id == new_entry, created_date],
                Sys.time()
              ),
              last_modified_date = Sys.time(),
              deleted_flag = "0",
              firestore_id = ifelse(
                nrow(values$fact_forms()[unique_id == new_entry]) > 0,
                values$fact_forms()[unique_id == new_entry, firestore_id],
                "Placeholder"
              )
            )
            
            dataJSON <- convertDTtoJSON(entry)
            
            if(nrow(values$fact_forms()[unique_id == new_entry]) > 0){
              # If there is an existing entry on firebase
              firestore_id_entry <- entry[, firestore_id]
              
              firestore_response <- sendJSONToFirestore(
                api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms, "/", firestore_id_entry),
                access_token = session$userData$current_user()$stsTokenManager$accessToken,
                data = dataJSON,
                replacement = TRUE
              )
            } else {
              # Create a new entry
              firestore_response <- sendJSONToFirestore(
                api_call = paste0(db_endpoint_api, db_endpoint_dataDraftForms),
                access_token = session$userData$current_user()$stsTokenManager$accessToken,
                data = dataJSON
              )
              
              # Add firestore_id
              firestore_id_entry <- getFirebaseIDFromPath(fromJSON(content(firestore_response,"text"))$name)
              entry[, firestore_id := firestore_id_entry]
            }
            
            adjusted_fact_forms <- rbind(values$fact_forms()[unique_id != new_entry], entry)
            saveRDS(object = adjusted_fact_forms, file = path_file_fact_forms)
            shinyalert(paste0("Form saved"), type = "success")
          } else {
            shinyalert(paste0("Need to fill in required fields to save"), type = "warning")
          }
        }
      )
      #### END - What happens once user presses the save form button ----
      
      #### START - What happens once user presses the load form button ----
      observeEvent(
        input$load_draft_form, 
        {
          s = input$all_draft_forms_rows_selected
          if(!is.null(s)){
            # Expand forms box and collapse past forms box
            js$collapseBox("past_forms_box")
            js$expandBox("new_forms_box")
            
            # Get relevant entry
            entry <- values$fact_forms()[s]
            
            # Update all fields with loaded forms
            
            updateSelectizeInput(session, inputId = "matter_id_input", selected = entry[, matter_id]) 
            updateSelectizeInput(session, inputId = "matter_type_input", selected = entry[, matter_type]) 
            updateSelectizeInput(session, inputId = "council_name_input", selected = entry[, council_name]) 
            updateTextAreaInput(session, inputId = "subject_address_input", value = entry[, subject_land_address])
            updateSelectizeInput(session, inputId = "number_of_owners_input", selected = entry[, no_of_owners])
            
            updateRadioButtons(session, inputId = "owner1_company_flag_input", selected = unlist(strsplit(entry[, owner_company_flag], ",,,"))[1])
            updateRadioButtons(session, inputId = "owner2_company_flag_input", selected = unlist(strsplit(entry[, owner_company_flag], ",,,"))[2])
            updateRadioButtons(session, inputId = "owner3_company_flag_input", selected = unlist(strsplit(entry[, owner_company_flag], ",,,"))[3])
            updateRadioButtons(session, inputId = "owner4_company_flag_input", selected = unlist(strsplit(entry[, owner_company_flag], ",,,"))[4])
            updateRadioButtons(session, inputId = "owner5_company_flag_input", selected = coalesceNA(unlist(strsplit(entry[, owner_company_flag], ",,,"))[5], ""))
            
            updateTextAreaInput(session, inputId = "owner1_name_input", value = unlist(strsplit(entry[, owner_name], ",,,"))[1])
            updateTextAreaInput(session, inputId = "owner1_address_input", value = unlist(strsplit(entry[, owner_address], ",,,"))[1])
            updateTextAreaInput(session, inputId = "owner2_name_input", value = unlist(strsplit(entry[, owner_name], ",,,"))[2])
            updateTextAreaInput(session, inputId = "owner2_address_input", value = unlist(strsplit(entry[, owner_address], ",,,"))[2])
            updateTextAreaInput(session, inputId = "owner3_name_input", value = unlist(strsplit(entry[, owner_name], ",,,"))[3])
            updateTextAreaInput(session, inputId = "owner3_address_input", value = unlist(strsplit(entry[, owner_address], ",,,"))[3])
            updateTextAreaInput(session, inputId = "owner4_name_input", value = unlist(strsplit(entry[, owner_name], ",,,"))[4])
            updateTextAreaInput(session, inputId = "owner4_address_input", value = unlist(strsplit(entry[, owner_address], ",,,"))[4])
            updateTextAreaInput(session, inputId = "owner5_name_input", value = unlist(strsplit(entry[, owner_name], ",,,"))[5])
            updateTextAreaInput(session, inputId = "owner5_address_input", value = unlist(strsplit(entry[, owner_address], ",,,"))[5])
            
            updateTextAreaInput(session, inputId = "owner1_ACN_input", value = unlist(strsplit(entry[, owner_ACN], ",,,"))[1])
            updateTextAreaInput(session, inputId = "owner2_ACN_input", value = unlist(strsplit(entry[, owner_ACN], ",,,"))[2])
            updateTextAreaInput(session, inputId = "owner3_ACN_input", value = unlist(strsplit(entry[, owner_ACN], ",,,"))[3])
            updateTextAreaInput(session, inputId = "owner4_ACN_input", value = unlist(strsplit(entry[, owner_ACN], ",,,"))[4])
            updateTextAreaInput(session, inputId = "owner5_ACN_input", value = unlist(strsplit(entry[, owner_ACN], ",,,"))[5])
            
            updateRadioButtons(session, inputId = "owner1_company_director_no_input", selected = unlist(strsplit(entry[, owner_company_director_no], ",,,"))[1])
            updateRadioButtons(session, inputId = "owner2_company_director_no_input", selected = unlist(strsplit(entry[, owner_company_director_no], ",,,"))[2])
            updateRadioButtons(session, inputId = "owner3_company_director_no_input", selected = unlist(strsplit(entry[, owner_company_director_no], ",,,"))[3])
            updateRadioButtons(session, inputId = "owner4_company_director_no_input", selected = unlist(strsplit(entry[, owner_company_director_no], ",,,"))[4])
            updateRadioButtons(session, inputId = "owner5_company_director_no_input", selected = coalesceNA(unlist(strsplit(entry[, owner_company_director_no], ",,,"))[5], ""))
            
            updateRadioButtons(session, inputId = "cert_of_title_flag_input", selected = entry[, cert_of_title_flag])
            updateDateInput(session, inputId = "cert_of_title_date_issued_input", value = coalesceNA(entry[, cert_of_title_date_issued], NULL))
            updateTextAreaInput(session, inputId = "cert_of_title_volume_input", value = entry[, develop_permit_number])
            updateTextAreaInput(session, inputId = "cert_of_title_folio_input", value = entry[, develop_permit_number])
            updateTextAreaInput(session, inputId = "develop_permit_number_input", value = entry[, develop_permit_number])
            updateDateInput(session, inputId = "develop_permit_issue_date_input", value = coalesceNA(entry[, develop_permit_issue_date], NULL))
            updateTextAreaInput(session, inputId = "develop_permit_condition_number_input", value = entry[, develop_permit_condition_number])
            updateTextAreaInput(session, inputId = "subdivision_permit_number_input", value = entry[, subdivision_permit_number])
            updateDateInput(session, inputId = "subdivision_permit_issue_date_input", value = coalesceNA(entry[, subdivision_permit_issue_date], NULL))
            updateTextAreaInput(session, inputId = "subdivision_permit_condition_number_input", value = entry[, subdivision_permit_condition_number])
            updateRadioButtons(session, inputId = "mortgage_flag_input", selected = entry[, mortgage_flag])
            updateTextAreaInput(session, inputId = "mortgage_name_input", value = entry[, mortgage_name])
            updateRadioButtons(session, inputId = "caveator_flag_input", selected = entry[, caveator_flag])
            updateTextAreaInput(session, inputId = "caveator_name_input", value = entry[, caveator_name])
            
            # Notification when form is loaded
            shinyalert(paste0("Form loaded ", entry[, form_id]), type = "success")
          } else {
            shinyalert(paste0("No forms selected"), type = "warning")
          }
        }
      )
      #### END - What happens once user presses the load form button ----
      
      #### START - What happens once user presses the delete form button ----
      observeEvent(
        input$delete_draft_form, 
        {
          s = input$all_draft_forms_rows_selected
          if(!is.null(s)){
            entry <- values$fact_forms()[s]
            matterID <- entry[, matter_id]
            
            # Delete from the table
            saveRDS(object = values$fact_forms()[matter_id != matterID], file = path_file_fact_forms)
            
            # Delete draft agreement if exists
            file_to_delete <- paste0(path_output_draft, entry[, form_id], " Section 173 Agreement draft.docx")
            if (file.exists(file_to_delete)){
              unlink(file_to_delete)
            }
            
            shinyalert(paste0("Deleted ", matterID), type = "success")
          } else {
            shinyalert(paste0("No forms selected"), type = "warning")
          }
          
        }
      )
      #### END - What happens once user presses the delete form button ----
      
      #### START - button to view the draft agreement ----
      mod_view_draft_server(session, input, output, reactive({input$view_draft_agreement_button}), values)
      #### END - button to view the draft agreement ----
      
      #### START - What happens once user downloads the 173 draft agreement ----
      output$download_173_draft_agreement <- downloadHandler(
        filename = function(){
          paste0(session$userData$downloadFile()$title)
        },
        content = function(file){
          file.copy(session$userData$downloadFile()$location, file)
        }
      )
      #### END - What happens once user downloads the 173 draft agreement ----
      
      #### START - What happens once user downloads the 181 draft agreement ----
      output$download_181_draft_agreement <- downloadHandler(
        filename = function(){
          paste0(session$userData$downloadFile()$title)
        },
        content = function(file){
          file.copy(session$userData$downloadFile()$location, file)
        }
      )
      #### END - What happens once user downloads the 181 draft agreement ----
      
      #### START - What happens once user downloads the 183 draft agreement ----
      output$download_183_draft_agreement <- downloadHandler(
        filename = function(){
          paste0(session$userData$downloadFile()$title)
        },
        content = function(file){
          file.copy(session$userData$downloadFile()$location, file)
        }
      )
      #### END - What happens once user downloads the 183 draft agreement ----
    }
  )  
}

