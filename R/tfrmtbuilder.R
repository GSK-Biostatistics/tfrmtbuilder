#' Run tfrmt Builder Shiny App
#' 
#' @export
tfrmtbuilder <- function(){
  app <- shinyApp(
    ui =  tfrmtbuilder_ui("tb"),
    server = function(input,output,session){
      tfrmtbuilder_server("tb")
    }
  )
  runApp(app)
}
