
# UI for setting permissions
ui_settingsButton <- function(){
  dropdownMenu(
    type = "messages",
    icon = icon("cog"),
    badgeStatus = NULL, 
    headerText = "", 
    tags$li(
      class = "dropdown",
      actionButton(
        "refresh_button",
        "Refresh",
        icon = icon("sync"),
        width = "100%"
      )
    ),
    tags$li(
      class = "dropdown",
      actionButton(
        "settings_button",
        "Settings",
        icon = icon("cog"),
        width = "100%"
      )
    ),
    tags$li(
      class = "dropdown",
      actionButton(
        "submit_sign_out",
        "Logout",
        icon = icon("user"),
        width = "100%"
      )
    )
  )
  
}


# Module UI ---------------------------------------------------------------
mod_settings_button <- function(id){
  ns <- NS(id)
  
  fluidRow(
    bsModal(id = ns("settings_pop_up"),       # Name of the modal 
            title = "Settings",               # Title of the modal
            trigger = "settings_button"       # The button that triggers the modal
            , h4("General Account Settings")
            , DT::dataTableOutput(ns("data")), br(), br()
            , uiOutput(ns("select_user_ui"))
            # , verbatimTextOutput(ns("print_user")) # FOR DEBUGGING PURPOSES
            )
  )

}

mod_refresh_button <- function(id){
  ns <- NS(id)
  
  fluidRow(
    bsModal(id = ns("refresh_pop_up"),       # Name of the modal 
            title = "Refresh",               # Title of the modal
            trigger = "refresh_button",       # The button that triggers the modal
            h4("You sure you want to refresh data?"),
            actionButton(
              ns("refresh_button2"),
              "Yes, refresh data",
              icon = icon("sync"),
              width = "100%"
            )
    )
  )
  
}



# Module Server -----------------------------------------------------------
mod_settings_button_server <- function(id, users, values){
  
  moduleServer(
    id,
    function(input, output, session){
      
      #### Start Change user ####
      # Check if button is pressed (if there's no observe then it opens when the app opens)
      observeEvent(input$settings_button, {
        toggleModal(session, modalId = "settings_pop_up", toggle = "toggle")
      })
      
      # In the future, only admins can see other users
      user_list_can_view_as <- reactive({
        role_of_user <- values$dim_user()[user == users$logged_in_user(), role]
        return(role_of_user)
      })
      
      # Show user list
      output$data <- DT::renderDataTable(
        values$dim_user()
      )
      
      output$select_user_ui <- renderUI({
        fluidPage(
          h4("Admin settings:"),
          selectizeInput(
            inputId = session$ns("select_user"),
            label = "View as:",
            choices = values$dim_user()[, user], 
            selected = "",
            multiple = FALSE,
            options = list(placeholder = 'Select the user you want to view the dashboard as' # Placeholder text
            ),
            width = "100%"
          )
        )
      })
      
      # Make view as user details available 
      return(reactive({input$select_user}))
      #### End Change user ####
      
    }
  )
}


mod_refresh_button_server <- function(id, users, values){
  
  moduleServer(
    id,
    function(input, output, session){
      #### Start refresh tables ####
      observeEvent(input$refresh_button, {
        toggleModal(session, modalId = "refresh_pop_up", toggle = "toggle")
      })
      
      observeEvent(input$refresh_button2, {
        refreshAllTables(session)
        
        shinyalert(paste0("Data refreshed"), type = "success")
        toggleModal(session, modalId = "refresh_pop_up", toggle = "toggle")
      })
      #### End refresh tables ####
    }
  )
}


