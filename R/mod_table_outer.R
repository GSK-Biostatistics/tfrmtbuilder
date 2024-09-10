# tfrmt Table outer module

table_outer_ui <- function(id){

  ns <- NS(id)

  tagList(
    actionButton(ns("refresh"), "Refresh", icon = icon("sync"), class = "btn-refresh"),
    table_inner_ui(ns("tbl"))
  )
}


#' @param id module ID
#' @param cur_tab Is this tab currently selected? TRUE/FALSE
#' @param subtab Name of selected tab in the Edit pane tabPanel
#' @param data data for the table
#' @param tfrmt_app_out final tfrmt for the table
#' @param mode mock mode w/ no data, w/ data, reporting
#'
#' @noRd
table_outer_server <- function(id, cur_tab, subtab, data, tfrmt_app_out, mode){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # hide/show the table
      observe({
        shinyjs::toggle("tbl_div", condition = !is.null(tfrmt_app_out()))
        shinyjs::toggle("tbl_div_msg", condition = is.null(tfrmt_app_out()))
      })

      # register when the tfrmt/table should AUTO update:
      #    - when refresh button is pressed
      #    - when selected tab changes & tbl is out of sync

      tbl_auto_refresh <- reactiveVal(0)

      # do not auto-update if no tfrmt exists
      observeEvent(tfrmt_app_out(),{
        if (is.null(tfrmt_app_out())){
          tbl_auto_refresh(0)
        }
      }, ignoreNULL = FALSE)

      # refresh button pressed
      observeEvent(input$refresh, {
        tbl_auto_refresh(tbl_auto_refresh()+1)
      })
      # tab is changed (either switching to the "edit" tab or switching within the edit tab)
      observeEvent(c(subtab(),(cur_tab()==TRUE)), {
        if (tbl_needs_refresh()){
          tbl_auto_refresh(tbl_auto_refresh()+1)
        }
      }, ignoreInit = TRUE)

      # track state of tbl (for css of refresh button)
      #  - when final tfrmt is changed, indicate refresh needed
      #  - if a refresh is triggered (captured by tbl_auto_refresh()), remove the indication

      tbl_needs_refresh<- reactiveVal(FALSE)

      # when the final tfrmt is changed, indicate refresh is needed
      observeEvent(tfrmt_app_out(), {
        shinyjs::addClass("refresh", class = "btn-danger")
        shinyjs::removeClass("refresh", class = "btn-refresh")

        tbl_needs_refresh(TRUE)
      })
      # when display update is triggered, remove the indication
      observeEvent(req(tbl_auto_refresh()>0),{
        shinyjs::removeClass("refresh", class = "btn-danger")
        shinyjs::addClass("refresh", class = "btn-refresh")

        tbl_needs_refresh(FALSE)
      })

      table_inner_server("tbl", data, tfrmt_app_out, mode, tbl_auto_refresh)

    }
  )
}
