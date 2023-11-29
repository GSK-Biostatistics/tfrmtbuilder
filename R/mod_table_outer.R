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

      retbl <- reactiveVal(0)

      # settings_counter for # of times the tfrmt settings are captured
      # aim is to trigger an auto-refresh when settings_count =1 and original settings are valid
      settings_count <- reactiveVal(NULL)
      observeEvent(settings(),{
        if (settings()$original==TRUE){
          settings_count(0)
        } else {
          settings_count(1)
        }
      })
      observeEvent(tfrmt_app_out(), {
        settings_count(settings_count() + 1)
      })
      # on initialization, if all valid
      observe({
        req(settings()$original==TRUE)
        req(tfrmt_app_out())
        req(settings_count()==1)

        isolate(
          retbl(retbl()+1)
        )
      })
      # refreshed
      observeEvent(input$refresh, {
        retbl(retbl()+1)
      })
      # tab change
      observeEvent(tab_selected(), {
        if (tbl_invalid()){
          retbl(retbl()+1)
        }
      }, ignoreInit = TRUE)

      # no update if tfrmt is reset (starting from beginning) or incomplete
      observe({
        if (is.null(tfrmt_app_out())){
          retbl(0)
        }
      })


      # track state of tbl (for css of refresh button)
      #  - when final tfrmt is changed, indicate refresh needed
      #  - if a refresh is triggered (automatically or by button press), remove the indication

      tbl_invalid<- reactiveVal(FALSE)

      # when the final tfrmt is changed, indicate refresh is needed
      observeEvent(tfrmt_app_out(),{
        shinyjs::addClass("refresh", class = "btn-danger")
        shinyjs::removeClass("refresh", class = "btn-refresh")

        tbl_invalid(TRUE)
      })
      # when display update is triggered, remove the indication
      observeEvent(req(retbl()>0),{
        shinyjs::removeClass("refresh", class = "btn-danger")
        shinyjs::addClass("refresh", class = "btn-refresh")

        tbl_invalid(FALSE)
      })


      tab <- table_inner_server("tbl", data, tfrmt_app_out, settings, retbl)

     return(tab)

    }
  )
}
