#' Launch the Extension as a Shiny App
#'
#' @description This function launches the Shiny app for a Requal extension module.
#'
#' @export
#'
#' @import shiny
mod_preview <- function() {
  # Define the UI
  ui <- fluidPage(
    mod_ui("requal_extension_module")
  )

  # Define the server logic
  server <- function(input, output, session) {
    mod_server("requal_extension_module", api = NULL)
  }

  # Run the application
  shinyApp(ui, server)
}
