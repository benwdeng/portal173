###################################################################################################
#
# COVID Invest Test Dashboard - User Interface (UI) file
#
###################################################################################################

library(shiny)
library(shinydashboard)


dashboardPage(
  # Dashboard Page Setup ----------------------------------------------------
  title = META$name,
  skin  = META$skin_color,
  dashboardHeader(
    title = HTML(glue::glue(
      '<span class="logo-mini">{META$logo_mini}</span>
      <span class="logo-lg">{META$logo_lg}</span>'
    )),
    titleWidth = "400"
  ),
  # Dashboard Sidebar -------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "tab_overview", icon = icon("tachometer-alt")),
      menuItem("Library", tabName = "tab_section173", icon = icon("book-open")),
      menuItem("About us", tabName = "tab_about", icon = icon("info"))
    ),
    collapsed = FALSE
  ),
  
  # Dashboard Body ----------------------------------------------------------
  dashboardBody(
    useShinyjs(),
    #extendShinyjs(text = jscode, functions = c("collapseBox", "expandBox")),
    tabItems(
      # Front page - tab_dashboard -----------------------------------------------
      tabItem("tab_overview", mod_overview_ui("overview_tab")),
      tabItem("tab_section173", mod_portal173_ui("portal173_tab")),
      tabItem("tab_about", mod_about_us_ui("about_us_tab"))
    )
  )
)
