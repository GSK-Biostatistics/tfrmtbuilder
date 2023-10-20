
#' Helper function for testing modules
#'
#' @param mod_name Module function name prefix (without `_ui` or `_server`)
#' @param args_ui named list of arguments/values to pass to the UI function
#' @param args_server named list of arguments/values to pass to the server function
#'
#' @noRd
test_module <- function(ui_fun, server_fun, args_ui = list(), args_server = list()) {

  id <- "mod_id"

  shinyApp(
    fluidPage(
      do.call(ui_fun, c(id = id, args_ui))
    ),
    function(input,output,session){
      vals <- do.call(server_fun, c(id = id, args_server))

      exportTestValues(vals = {vals()})
    }
  )
}
