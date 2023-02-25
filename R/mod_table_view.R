# tfrmt Table view module

table_view_ui <- function(id){

  ns <- NS(id)

  tagList(
    h3("Table", actionButton(ns("refresh"), "Refresh", icon = icon("sync"))),
    shinyjs::hidden(
      div(
      id = ns("tbl_div"),
      shinycssloaders::withSpinner(
        gt_output(ns("tbl_view")),
       type = 4
        )
      )
    )
    ,
    shinyjs::hidden(
      p(id = ns("tbl_div_msg"), style="color:red;",
        "Incomplete settings configuration")
    )
  )
}


#' @param id module ID
#' @param tab_selected selected tab in the tabPanel
#' @param data data for the table
#' @param tfrmt_app_out final tfrmt for the table
#' @param mode mock mode w/ no data, w/ data, reporting
#'
#' @noRd
table_view_server <- function(id, tab_selected, data, tfrmt_app_out, settings){

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

      # on initialization, if all valid
      observe({
        req(settings()$original==TRUE)
        req(tfrmt_app_out())
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
        tbl_invalid(TRUE)
      })

      # when display update is triggered, remove the indication
      observeEvent(req(retbl()>0),{
        print("ok")
        shinyjs::removeClass("refresh", class = "btn-danger")
        tbl_invalid(FALSE)
      })


      # view table
      output$tbl_view <- render_gt({

        req(retbl()>0)

        if (isolate(settings()$mode)=="reporting"){
          isolate(tfrmt_app_out())%>% print_to_gt(.data = isolate(data()))

        } else {

          isolate(tfrmt_app_out())%>% print_mock_gt(.data = isolate(data()))
        }

      })
    }
  )
}
