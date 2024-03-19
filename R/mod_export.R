
# module to export final tfrmt/table

export_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
             wellPanel(
               div(style = "height: 650px;",
                     h3("Table Metadata", class = "heading_style",
                        div(downloadButton(ns("json_save"), label = "JSON", icon = icon("download"))), class = "btn-export"),
                   div(style = "height: 550px; overflow-y:auto; ",
                       shinycssloaders::withSpinner(
                         color = getOption("spinner.color", default = "#254988"),
                         type = 4,
                         verbatimTextOutput(ns("json"))
                         )
                   )
                 )
               )

             ) ,
      column(8,
             wellPanel(
               div(style = "height: 650px;",
                   h3("Table", class = "heading_style",
                      span(class = "btn-export", style = "display: flex; gap: 5px;",
                           lapply(c("html","png","rtf","docx","pdf","tex"), function(ext){
                             mod_export_table_ui(ns(ext), ext=ext)
                           })
                      )
                      ),
                   div(style = "height: 550px; overflow-y:auto; ",
                       table_inner_ui(ns("tbl_view"))
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
export_server <- function(id, data, tfrmt_app_out, settings){

  moduleServer(
    id,
    function(input, output, session) {

      output$json <- renderText({
        req(tfrmt_app_out())
        tfrmt_app_out() %>% tfrmt_to_json()
      })

      auto_tbl <- reactiveVal(0)
      observeEvent(tfrmt_app_out(), {
        auto_tbl(auto_tbl()+1)
      })

      tbl_out <- table_inner_server("tbl_view", data = data, tfrmt_app_out = tfrmt_app_out, settings = settings, auto_tbl = auto_tbl)


      output$json_save <- downloadHandler(
          filename = function() {
            paste('tfrmt.json', sep='')
          },
          content = function(con) {
            tfrmt_to_json(tfrmt_app_out(), con)
          }
        )

      lapply(c("html","png","rtf","docx","pdf","tex"), function(ext){
        mod_export_table_server(ext, tbl=tbl_out, ext=ext)
      })
    })
}
