# tfrmt Table outer module

table_outer_ui <- function(id){

  ns <- NS(id)

  tagList(
    actionButton(ns("refresh"), "Refresh", icon = icon("sync"), class = "btn-refresh"),
    table_inner_ui(ns("tbl"))
  )
}


#' @param id module ID
#' @param tab_selected selected tab in the tabPanel
#' @param data data for the table
#' @param tfrmt_app_out final tfrmt for the table
#' @param settings mock mode w/ no data, w/ data, reporting
#'
#' @noRd
table_outer_server <- function(id, tab_selected, data, tfrmt_app_out, settings){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # hide/show the table
      observe({
        shinyjs::toggle("tbl_div", condition = !is.null(tfrmt_app_out()))
        shinyjs::toggle("tbl_div_msg", condition = is.null(tfrmt_app_out()))
      })

      # register when the tfrmt/table should update:
      #    - on initialization, if all valid
      #    - when refresh button is pressed
      #    - when selected tab changes & tbl is out of sync

      auto_tbl <- reactiveVal(0)

      tfrmt_counter <- reactiveVal(0)

      observeEvent(tfrmt_app_out(),{
        if (is.null(tfrmt_app_out())){
          tfrmt_counter(0)
        } else {
          tfrmt_counter(tfrmt_counter()+1)
        }
      })
      observe(print(tfrmt_counter()))
      # on initialization, if all valid
      observe({
        req(settings()$original==TRUE)
        req(tfrmt_counter()==1)

        isolate(
          auto_tbl(auto_tbl()+1)
        )
      })
      # refresh button pressed
      observeEvent(input$refresh, {
        auto_tbl(auto_tbl()+1)
      })
      # tab change
      observeEvent(tab_selected(), {
        if (tbl_invalid()){
          auto_tbl(auto_tbl()+1)
        }
      }, ignoreInit = TRUE)

      # no update if tfrmt is reset (starting from beginning) or incomplete
      observe({
        if (is.null(tfrmt_app_out())){
          auto_tbl(0)
        }
      })

      # track state of tbl (for css of refresh button)
      #  - when final tfrmt is changed, indicate refresh needed
      #  - if a refresh is triggered (automatically or by button press), remove the indication

      tbl_invalid<- reactiveVal(FALSE)

      # when the final tfrmt is changed, indicate refresh is needed
      observeEvent(tfrmt_app_out(), {
        shinyjs::addClass("refresh", class = "btn-danger")
        shinyjs::removeClass("refresh", class = "btn-refresh")

        tbl_invalid(TRUE)
      })
      # when display update is triggered, remove the indication
      observeEvent(req(auto_tbl()>0),{
        shinyjs::removeClass("refresh", class = "btn-danger")
        shinyjs::addClass("refresh", class = "btn-refresh")

        tbl_invalid(FALSE)
      })


      table_inner_server("tbl", data, tfrmt_app_out, settings, auto_tbl)


    }
  )
}
