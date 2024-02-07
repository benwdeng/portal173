source("global.R")


server <- function(input, output, session) {

  ##### Switch Views ------------------
  # if user click link to register, go to register view
  # observeEvent(input$go_to_register, {
  #   shinyjs::show("register_panel", anim = TRUE, animType = "fade")
  #   shinyjs::hide("sign_in_panel")
  # }, ignoreInit = TRUE)

  observeEvent(input$go_to_sign_in, {
    # shinyjs::hide("register_panel")
    shinyjs::show("sign_in_panel", anim = TRUE, animType = "fade")
  }, ignoreInit = TRUE)

  # switch between auth sign in/registration and app for signed in user
  observeEvent(session$userData$current_user(), {
    current_user <- session$userData$current_user()
    
    if (is.null(current_user)) {
      shinyjs::show("sign_in_panel")
      shinyjs::hide("main")
      shinyjs::hide("verify_email_view")
    } else {
      shinyjs::hide("sign_in_panel")
      # shinyjs::hide("register_panel")

      if (current_user$emailVerified == TRUE) {
        shinyjs::show("main")
      } else {
        shinyjs::show("verify_email_view")
      }

    }

  }, ignoreNULL = FALSE)


  # Signed in user --------------------
  # the `session$userData$current_user()` reactiveVal will hold information about the user
  # that has signed in through Firebase.  A value of NULL will be used if the user is not
  # signed in
  session$userData$current_user <- reactiveVal(NULL)

  # input$sof_auth_user comes from front end js in "www/sof-auth.js"
  observeEvent(input$sof_auth_user, {

    # set the signed in user
    session$userData$current_user(input$sof_auth_user)
    if(isTRUE(length(input$sof_auth_user$email) > 0)){
      # refreshAllTables(session)
      shinyalert(paste0("Logged in as ", input$sof_auth_user$email), type = "info")
    } else {
      shinyalert(paste0("Not logged in"), type = "info")
    }
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # input$listOfFiles comes from front end js in "www/sof-auth.js"
  session$userData$relevantFiles <- reactiveVal(NULL)
  
  observeEvent(input$listOfFiles, {
    
    # set the signed in user
    browser()
    fileNames <- list(filename = input$listOfFiles)
    session$userData$relevantFiles(fileNames)
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent(input$downloadPostCode, {
    browser()
    shinyalert(text = "downloading", type = "success")
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  

  ##### App for signed in user
  signed_in_user_df <- reactive({
    req(session$userData$current_user())

    out <- session$userData$current_user()
    out <- unlist(out)

    data.frame(
      name = names(out),
      value = unname(out)
    )
  })
  
  output$user_out <- DT::renderDT({
    datatable(
      signed_in_user_df(),
      rownames = FALSE,
      options = list(
        dom = "tp",
        scrollX = TRUE
      )
    )
  })
  
  #### Start of the main part of server 
  # Pop up on start up only
  users <- reactiveValues()
  
  # Logged in user ---------------------
  logged_in_user_id <- isolate({ # Only needs to be run once per session
    if(!is.null(session$user)){
      session$user # If running on the shiny server remotely
    } else if(is.null(session$user)){
      Sys.info()["user"]  # Only if running on a local computer
    } else{
      "User not found"
    }
  })
  
  users$logged_in_user <- reactive({
    # dataStorage$dim_user()[user %in% logged_in_user_id]
    logged_in_user_id
  }) 
  
  # View as user ------------------------
  users$view_as_user <- reactive({
    
    # If a 'view as' user has been selected, use that
    if(!is.null(select_user())){
      select_user()
    }
    # Else use the full name of the logged in user
    else{
      # users$logged_in_user()[, user]
      users$logged_in_user()
    }
  })
  
  # Control access to tabs --------------------------------------------------
  
  # Initialise a variable that indicates whether someone is an admin
  is_admin <- reactiveVal({"No"})
  
  # Every time the users object changes, check whether person should get permission to the My Team tab
  observeEvent(users$view_as_user(), priority = 1, {
    
    # Note the admin permissions of the new user
    new_is_admin <- if (users$view_as_user() %in% dataStorage$dim_user()[, user]){
      dataStorage$dim_user()[user %in% users$view_as_user(), role]
    } else {
      "Admin"
    }
    
    # If they are not the same as the previous permission (permission = YES) and they do not have permission, then hide the tab
    if(new_is_admin != is_admin() & new_is_admin != "Admin"){
      shinyjs::hide(selector = '[data-value="tab_overview"]')
      updateTabItems(session, "tabs", selected = "tab_permitholder") # Otherwise it selects the Team tab
      is_admin("No")
    }
    # If they are not the same as the previous permission (permission = NO) and they do not have permission, then show the tab
    else if(new_is_admin != is_admin() & new_is_admin == "Admin"){
      shinyjs::show(selector = '[data-value="tab_overview"]')
      updateTabItems(session, "tabs", selected = "tab_overview") # Otherwise it selects the Team tab
      is_admin("Yes")
    }
  })
  
  # Pop out who is the logged in person
  # observe({
  #   shinyalert(paste0("View as ", users$view_as_user()), type = "info")
  # }) 
  
  # Grab data
  dataStorage <- reactiveValues(
    dim_user = reactiveFileReader(intervalMillis = 60*1000, session = NULL, readFunc = readRDS, filePath = path_file_dim_user),
    dim_user_permission = reactiveFileReader(intervalMillis = 60*1000, session = NULL, readFunc = readRDS, filePath = path_file_dim_user_permission),
    fact_matter = reactiveFileReader(intervalMillis = 1000, session = NULL, readFunc = readRDS, filePath = path_file_fact_matter),
    fact_forms = reactiveFileReader(intervalMillis = 1000, session = NULL, readFunc = readRDS, filePath = path_file_fact_forms),
    fact_documents = reactiveFileReader(intervalMillis = 1000, session = NULL, readFunc = readRDS, filePath = path_file_fact_documents)
  )
  
  # Settings pop up page
  select_user <- mod_settings_button_server(
    id = "settings_mod",
    users = users,
    values = dataStorage
  )
  mod_refresh_button_server(id = "refresh_mod", values = dataStorage, users = users)
  
  # Server for the tabs
  mod_overview_server(id = "overview_tab")
  #### End of the main part of server ####
}
