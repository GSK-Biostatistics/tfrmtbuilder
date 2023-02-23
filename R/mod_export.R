
# module to export final tfrmt/table

export_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
             wellPanel(
               div(style = "height: 650px;",
                     h3("JSON", downloadButton(ns("json_save"), label = "Export", icon = icon("download"))),
                   div(style = "height: 550px; overflow-y:scroll; ",
                       verbatimTextOutput(ns("json"))
                   )
                 )
               )

             ) ,
      column(8,
             wellPanel(
               div(style = "height: 650px;",
                   h3("Table", downloadButton(ns("tbl_save"), label = "Export", icon = icon("download"))),
                   div(style = "height: 550px; overflow-y:scroll; ",
                       gt_output(ns("tbl"))
                   )
               )
             )
      )
    )
  )

}

#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app_out final tfrmt for the table
#' @param mode mock mode w/ no data, w/ data, reporting
#'
#' @noRd
export_server <- function(id, data, tfrmt_app_out, mode){

  moduleServer(
    id,
    function(input, output, session) {

      output$json <- renderText({
        req(tfrmt_app_out())
        tfrmt_app_out() %>% tfrmt_to_json()
      })

      tbl_out <- reactive({
        req(tfrmt_app_out())

        if (isolate(mode())=="reporting"){
          tfrmt_app_out() %>% print_to_gt(.data = data())
        } else {
          tfrmt_app_out() %>% print_mock_gt(.data = data())
          }
      })

      output$tbl <- render_gt({
         tbl_out()
      })

      output$json_save <- downloadHandler(
          filename = function() {
            paste('tfrmt-', Sys.Date(), '.json', sep='')
          },
          content = function(con) {
            tfrmt_to_json(tfrmt_app_out(), con)
          }
        )

      output$tbl_save <- downloadHandler(
        filename = function() {
          paste('tfrmt-', Sys.Date(), '.html', sep='')
        },
        content = function(con) {
          gtobj <- tbl_out()
          gtsave(gtobj, con)
        }
      )
    })
}