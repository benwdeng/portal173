library(shiny)

ui <- fluidPage(
  # Include custom CSS for styling
  tags$head(
    tags$style(HTML("
      body {
        background-color: #6699CC; /* Set the background color */
      }
      .header-title {
        font-size: 32px; /* Adjust font size as needed */
        font-weight: bold; /* Make the text bold */
        color: #FFFFFF; /* Set the text color to white */
        text-align: center;
        margin: 20px 0;
      }
      .nav-buttons {
        text-align: center;
        margin-top: 20px;
      }
      .nav-button {
        margin: 5px; /* Spacing between buttons */
        /* Optional: Style adjustments for buttons */
      }
    "))
  ),
  
  # Header title
  div(class = "header-title", "The Online Planning Hub"),
  
  # Navigation buttons
  div(class = "nav-buttons",
      actionButton("go_to_toph_law_firm", "Law firm", class = "nav-button", onclick = "window.location.href='https://onlineplanninghub-bxcn5rgbmq-ts.a.run.app'"),
      actionButton("go_to_toph_council", "Council", class = "nav-button", onclick = "window.location.href='https://onlineplanninghub-council-bxcn5rgbmq-ts.a.run.app'"),
      actionButton("go_to_toph_owner", "Owner", class = "nav-button", onclick = "alert('Owners Portal is coming soon!');") # This button is a placeholder
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
