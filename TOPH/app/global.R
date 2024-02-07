###################################################################################################
#
# COVID Invest Test Dashboard - Global data + functions + definitions file
#
###################################################################################################

#### Set timezone ####
TZ <- "Australia/Sydney"
Sys.setenv(TZ = TZ)
# Sys.timezone(TZ)
options(tz = TZ)

#### Required packages ####
## Check if your desktop has those packages and will install them if they are not.
list.of.packages <- c("data.table",
                      "shiny",
                      "shinydashboard",
                      "tidyverse",
                      "DT",
                      "shinyalert",
                      "shinyBS",
                      "shinyjs",
                      "officer",
                      "rmarkdown",
                      "leaflet",
                      "futile.logger",
                      
                      # Packages for ShinyApp server to work
                      "crosstalk",
                      "htmlwidgets",
                      "janitor",
                      "kableExtra",
                      "lazyeval",
                      "plotly",
                      "snakecase",
                      "svglite",
                      
                      # Packages that connect to database
                      "jsonlite",
                      "httr"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]

# Download all other standard packages
# if (length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# Read libraries
# lapply(list.of.packages, library, character.only = TRUE)
# Doing this manually so the shinyapp.io can pick it up
library(data.table)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyalert)
library(shinyBS)
library(shinyjs)
library(officer)
library(rmarkdown)
library(leaflet)
library(futile.logger)
library(crosstalk)
library(htmlwidgets)
library(janitor)
library(kableExtra)
library(lazyeval)
library(plotly)
library(snakecase)
library(svglite)
library(jsonlite)
library(httr)

# Load helpers -----------------------------------------------------------

# Modules
source("modules/mod_overview.R")
source("modules/mod_portal173.R")
source("modules/mod_overview_create_new_matter.R")
source("modules/mod_overview_change_matter_status.R")
source("modules/mod_overview_view_edit_matter.R")
source("modules/mod_overview_upload_document.R")
source("modules/mod_portal173_view_draft.R")
source("modules/mod_permitholder.R")
source("modules/mod_about_us.R")
source("utils/01_javascript_functions.R")
source("utils/02_functions.R")
source("utils/03_utils_settings_button.R")
source("utils/04_firestore_functions.R")



#### Global variables ####
path_input_template <- "inputs/Section 173 Agreement - template.docx"
path_input_181_template <- "inputs/Section 181 - template.docx"
path_input_183_template <- "inputs/Section 183 - template.docx"
path_input_council_info <- "inputs/2022-VIC-Council-Contact-Details-Nov-2021.csv"
path_output_draft <- "outputs/"
path_file_dim_user <- "data/dataUser"
path_file_dim_user_permission <- "data/dataUserPermission"
path_file_fact_matter <- "data/dataOverview"
path_file_fact_forms <- "data/dataDraftForms"
path_file_fact_document <- "data/dataDocument"
db_endpoint_api <- "https://firestore.googleapis.com/v1/"
db_endpoint_test <- "projects/the-online-planning-hub/databases/(default)/documents/table"
db_endpoint_dataOverview <- "projects/the-online-planning-hub/databases/(default)/documents/dataOverview"
db_endpoint_dataDraftForms <- "projects/the-online-planning-hub/databases/(default)/documents/dataDraftForms"
db_endpoint_dataDocument <- "projects/the-online-planning-hub/databases/(default)/documents/dataDocument"
db_endpoint_dataUser <- "projects/the-online-planning-hub/databases/(default)/documents/dataUser"
db_endpoint_dataUserPermission <- "projects/the-online-planning-hub/databases/(default)/documents/dataUserPermission"
status_choices <- c("1 Engagement", "2 Drafting stage", "3 Signing stage", 
                    "3 Pending council", "3 Pending owner",
                    "3 Pending other stakeholders",
                    "4 Lodgement", "5 Closed matter")
type_of_agreement <- c("Draft a section 173 agreement", 
                       "Draft a section 173 agreement to build over an easement", 
                       "Amend a section 173 agreement", 
                       "Remove a section 173 agreement",
                       "Review a section 173 agreement",
                       "Lodge only section 173 agreement")
client_roles <- c("Owner", 
                  "Surveyor",
                  "Land Consultant")
type_of_document <- c("Certificate/s of Title", "Planning Permits", 
                      "Company search", "Plan of Subdivision", "Power of Attorney",
                      "Other documents")


#### Settings ####
# ---- Metadata ----
META <- list(
  # Name of the app, used in the browser/tab title
  name        = "The Online Planning Hub",
  # A description of the app, used in social media cards
  description = "Dashboard build for Portal 173",
  # Link to the app, used in social media cards
  app_url     = "TBS",
  # Link to app icon image, used in social media cards
  app_icon    = "https://garrickadenbuie.com/images/2019/rstudioconf-2019-icon.png",
  # The name of the conference or organization
  conf_org    = "TE Company",
  # App title, long, shown when sidebar is open, HTML is valid
  logo_lg     = "<strong>The Online Planning Hub</strong>",
  # App title, short, shown when sidebar is collapsed, HTML is valid
  logo_mini   = "<em>rs</em><strong>c</strong>",
  # Icon for box with count of conference-related tweets
  topic_icon  = "comments",
  # Icon for box with count of "community"-related tweets
  topic_icon_full = "r-project",
  # AdminLTE skin color for the dashboard
  skin_color  = "blue",
  # AdminLTE theme CSS files
  theme_css   = c("ocean-next/AdminLTE.css", "ocean-next/_all-skins.css")
)

# ---- Topics Settings ----
# The dashboard is designed to show tweets related to a conference, using
# specific terms to locate conference tweets. If the conference tweets are part
# of a broader Twitter community, you can also show stats about the full
# community (see also `topic_icon_full` above).
#
# Note that currently, this app expects tweet gathering to be conducted by an
# external script or process. The terms below should then match or extend the
# terms used in the gathering process.
#
# See https://github.com/gadenbuie/gathertweet for a utility for tweet
# gathering.
TOPIC <- list(
  # Name of the conference or topic, for use in descriptive text
  name             = "The Online Planning Hub",
  # Name of the full Twitter community, for use in descriptive text
  full_community   = "The Online Planning Hub",
  # Terms related to the topic that must be included in topical tweet text
  terms            = c("rstudioconf", "rstudio conf", "rstudio::conf", "rstudiconf", "rstduioconf"),
  # Hashtags to exclude from the Top 10 Hashtags list (because they're implied by the topic)
  hashtag_exclude  = "rstudio?conf|rstduioconf|rstats|rstudio conf",
  # Words to exclude from the Top 10 Words list (because they're implied by the topic)
  wordlist_exclude = "rstudio|conf|rstats"
)


# ---- Colors ----
# Set these colors to match your AdminLTE styles. Note that this does not
# update the CSS, just makes it possible to use the colors in the plots
# displayed in the dashboard.
#
# See https://github.com/gadenbuie/AdminLTE/tree/ocean-next for an example.
ADMINLTE_COLORS <- list(
  "light-blue" = "#6699CC",
  "green"      = "#99C794",
  "red"        = "#EC5f67",
  "purple"     = "#C594C5",
  "aqua"       = "#a3c1e0",
  "yellow"     = "#FAC863",
  "navy"       = "#343D46",
  "olive"      = "#588b8b",
  "blue"       = "#4080bf",
  "orange"     = "#F99157",
  "teal"       = "#5FB3B3",
  "fuchsia"    = "#aa62aa",
  "lime"       = "#b0d4b0",
  "maroon"     = "#AB7967",
  "black"      = "#1B2B34",
  "gray-lte"   = "#D8DEE9",
  "primary"    = "#6699CC",
  "success"    = "#99C794",
  "danger"     = "#EC5f67",
  "info"       = "#a3c1e0",
  "warning"    = "#FAC863"
)

options("spinner.color" = ADMINLTE_COLORS$`gray-lte`)
options("spinner.color.background" = "#F9FAFB")

