###################################################################################################
#
# Portal 173 - About Us module
#
###################################################################################################

mod_about_us_ui <- function(id){
  
  # Defining namespace
  ns <- NS(id)
  
  fluidPage(
    #
    # Note About Credit
    # =================
    #
    # You may, of course, decide to change this section of the dashboard.
    # But PLEASE provide credit and link to my website: garrickadenbuie.com
    # Something like this would be great:
    #
    # "This dashboard was built using a template provided by
    # <a href="https://garrickadenbuie.com">Garrick Aden-Buie</a>."
    #
    # Thank you! -Garrick
    # About - About Me - start ------------------------------------------------
    fluidRow(
      h1("About us"),
      align = "center"
    ),
    box(
      title = "About us",
      status = "danger",
      width = "6 col-lg-6",
      tags$p(
        class = "text-center",
        tags$img(class = "img-responsive img-rounded center-block", src = "AMA logo.png", style = "max-width: 150px;")
      ),
      tags$p(
        class = "text-center",
        tags$strong("Hi! I'm Olivera")
      ),
      tags$p(
        "Founder and Principal of Centre for Mediation with degrees in Social Science and Law and formerly a family lawyer, I am a nationally accredited mediator under the National Mediation Accreditation System (NMAS) and a family dispute resolution practitioner with the Attorney General's Department."
      ),
      tags$p(
        "At the core of my practice is an unwavering commitment to mediating separated couples toward new beginnings. Happiness, success and personal wellbeing is what we aim to deliver."
      ),
      tags$p(
        "At the Centre for Mediation we conduct ourselves with absolute integrity and distinguish ourselves by creating an environment that fosters dignity, respect, equality, fairness and self-determination."
      ),
      tags$p(
        "Get in touch with by email at",
        HTML(paste0(tags$a(href = "mailto:olivera@centreformediation.com.au", "olivera@centreformediation.com.au"), "."))
      )
    ),
    # About - About Me - end --------------------------------------------------
    # About - About Dashboard - start -----------------------------------------
    box(
      title = "About this Dashboard",
      # status = "primary",
      width = "6 col-lg-6",
      tags$p(
        class = "text-center",
        tags$a(
          href = "https://www.r-project.org",
          target = "_blank",
          tags$img(class = "image-responsive",
                   src = "https://www.r-project.org/logo/Rlogo.svg",
                   style = "max-width: 150px;"
          )
        ),
        tags$a(
          href = "https://rstudio.com",
          target = "_blank",
          tags$img(class = "image-responsive",
                   src = "RStudio.svg",
                   style = "max-width: 150px; margin-left: 2em;"
          )
        )
      ),
      tags$p(
        "This dashboard was built in",
        tags$a(href = "https://r-project.org", target = "_blank", "R"),
        "and", tags$a(href = "https://rstudio.com", target = "_blank", "RStudio"), "with",
        tags$strong("shiny,"),
        tags$strong("shinydashboard,"),
        tags$strong("rtweet,"),
        tags$strong("plotly,"),
        "the", tags$strong("tidyverse,"),
        "and many more packages."
      )
      # About - About Dashboard - start -----------------------------------------
    )
  )
}
