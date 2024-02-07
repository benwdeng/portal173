###################################################################################################
#
# COVID Invest Test Dashboard - User Interface (UI) file
#
###################################################################################################

library(shiny)
library(shinydashboard)


fluidPage(
  shiny::singleton(
    shiny::tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$link(rel = "stylesheet", href = "snackbar.css"),
      tags$script(src="snackbar.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-app.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-auth.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/6.1.0/firebase-storage.js"),
      shiny::tags$script(src="sof-auth.js")
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
          titleWidth = "300",
          ui_settingsButton()
        ),
        
        # Dashboard Sidebar -------------------------------------------------------
        dashboardSidebar(
          sidebarMenu(
            id = "tabs",
            menuItem("Overview", tabName = "tab_overview", icon = icon("tachometer-alt")),
            menuItem("About us", tabName = "tab_about", icon = icon("info"))
          ),
          collapsed = FALSE
        ),
        
        # Dashboard Body ----------------------------------------------------------
        dashboardBody(
          useShinyjs(),
          extendShinyjs(text = jscode, functions = c("collapseBox", "expandBox")),
          # useWaiter(), # Waiter is the name of the loading bar 
          # waiterShowOnLoad(html = spin_three_bounce()), # https://cran.r-project.org/web/packages/waiter/waiter.pdf 'spinners' for different options (page 18)
          tabItems(
            # Front page - tab_dashboard -----------------------------------------------
            tabItem("tab_overview", mod_overview_ui("overview_tab")),
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



