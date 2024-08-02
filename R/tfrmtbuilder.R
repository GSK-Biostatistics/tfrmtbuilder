#' Run tfrmt Builder Shiny App
#'
#' @param tfrmt tfrmt object to be loaded into app from R session. Defaults to `NULL`
#' @param data data frame to be loaded into app from R session. Defaults to `NULL`
#' @param mockmode Whether to initialize the app in mock mode. Defaults to `TRUE`
#' @param run Boolean for whether the created object should be run directly. Set to `FALSE` for deployment
#' @export
#' @return Shiny app for creating and modifying tfrmt objects
#' @examples
#' if (interactive()){
#'   tfrmtbuilder()
#' }
tfrmtbuilder <- function(tfrmt = NULL, data = NULL, mockmode = TRUE, run = TRUE){
  app <- shinyApp(
    ui =  tfrmtbuilder_ui("tb", mockmode),
    server = function(input,output,session){
      tfrmtbuilder_server("tb", tfrmt, data)
      session$onSessionEnded(function() {
        stopApp()
      })
    }
  )

  if (run){
    runApp(app, launch.browser = TRUE)
  } else {
    app
  }

}
