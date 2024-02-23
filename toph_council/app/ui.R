###################################################################################################
#
# COVID Invest Test Dashboard - User Interface (UI) file
#
###################################################################################################

library(shiny)
library(shinydashboard)

source("global.R")

dataCouncilInfo <- data.table(read.csv(path_input_council_info, fileEncoding = 'UTF-8-BOM'))

fluidPage(
  shiny::singleton(
    shiny::tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$link(rel = "stylesheet", href = "snackbar.css"),
      tags$script(src="snackbar.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-app.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-auth.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/6.1.0/firebase-storage.js"),
      shiny::tags$script(src="sof-auth.js"),
      tags$style(HTML("
        .main-header .logo {
          float: left; /* Align logo to the left */
        }
        .main-header .navbar {
          margin-left: 0; /* Remove margin if needed */
        }
      "))
    )
  ),

  # load shinyjs on
  shinyjs::useShinyjs(),

  source("sof-auth/sign-in.R", local = TRUE)$value,
  # source("sof-auth/register.R", local = TRUE)$value,
  source("sof-auth/verify-email.R", local = TRUE)$value,

  hidden(
    fluidRow(
      id = "main",
      #### Start of the main part of the dashboard ####
      dashboardPage(
        # Dashboard Page Setup ----------------------------------------------------
        title = META$name,
        skin  = META$skin_color,
        
        dashboardHeader(
          title = HTML(glue::glue(
            '<span class="logo-mini">{META$logo_mini}</span>
            <span class="logo-lg">{META$logo_lg}</span>'
          )),
          titleWidth = "600",
          ui_settingsButton()
        ),
        
        # Dashboard Sidebar -------------------------------------------------------
        dashboardSidebar(
          sidebarMenu(
            id = "tabs",
            menuItem("Overview", tabName = "tab_overview", icon = icon("tachometer-alt")),
            menuItem("Permit holder", tabName = "tab_permitholder", icon = icon("pencil-alt")),
            #menuItem("Draft forms", tabName = "tab_section173", icon = icon("stream")),
            menuItem("About us", tabName = "tab_about", icon = icon("info"))
          ),
          collapsed = FALSE
        ),
        
        # Dashboard Body ----------------------------------------------------------
        dashboardBody(
          useShinyjs(),
          extendShinyjs(text = jscode, functions = c("collapseBox", "expandBox", "seque", "downloadFile", "deleteFile")),
          # useWaiter(), # Waiter is the name of the loading bar 
          # waiterShowOnLoad(html = spin_three_bounce()), # https://cran.r-project.org/web/packages/waiter/waiter.pdf 'spinners' for different options (page 18)
          tabItems(
            # Front page - tab_dashboard -----------------------------------------------
            tabItem("tab_overview", mod_overview_ui("overview_tab")),
            tabItem("tab_permitholder", mod_permitholder_ui("permitholder_tab")),
            #tabItem("tab_section173", mod_portal173_ui("portal173_tab")),
            tabItem("tab_about", mod_about_us_ui("about_us_tab"))
          ),
          mod_settings_button(id = "settings_mod"),
          mod_refresh_button(id = "refresh_mod")
        )
      )
      #### End of the main part of the dashboard ####
    )
  )
)





