#' Run tfrmt Builder Shiny App
#'
#' @param run Boolean for whether the created object should be run directly. Set to `FALSE` for deployment
#' @export
#' @return Shiny app for creating and modifying tfrmt objects
#' @examples
#' if (interactive()){
#'   tfrmtbuilder()
#' }
tfrmtbuilder <- function(run = TRUE){
  app <- shinyApp(
    ui =  tfrmtbuilder_ui("tb"),
    server = function(input,output,session){
      tfrmtbuilder_server("tb")
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
