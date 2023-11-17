# tfrmt Table view module

table_view_ui <- function(id){

  ns <- NS(id)

  tagList(
    shinyjs::hidden(actionButton(ns("refresh"), "Refresh", icon = icon("sync"), class = "btn-refresh")),
    shinyjs::hidden(
      div(
        id = ns("tbl_div"),
        table_page_ui(ns("tbl_page")),
        shinycssloaders::withSpinner(
          color = getOption("spinner.color", default = "#254988"),
          type = 4,
          tagList(
            htmlOutput(ns("tbl_view"))
          )
        )
      )
    ) ,
    shinyjs::hidden(
      p(id = ns("tbl_div_msg"), style="color:red;",
        "Incomplete settings configuration")
    ),
    htmlOutput(ns("error_msg"))
  )
}


#' @param id module ID
#' @param tab_selected selected tab in the tabPanel
#' @param data data for the table
#' @param tfrmt_app_out final tfrmt for the table
#' @param mode mock mode w/ no data, w/ data, reporting
#'
#' @noRd
table_view_server <- function(id, tab_selected = reactive(NULL), data, tfrmt_app_out, settings, enable_refresh = FALSE){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # enable refresh?
      observe({
        shinyjs::toggle("refresh", condition = enable_refresh)
      })

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

      # table as reactive
      tab <- reactive({

        req(retbl()>0)

        tfrmt_app_out <- isolate(tfrmt_app_out())
        mode <- isolate(settings()$mode)
        data <- isolate(data())

        if (mode=="reporting"){
          tfrmt_app_out %>% safely(print_to_gt)(.data = data)

        } else if (mode=="mock_no_data"){

          tfrmt_app_out %>% safely(print_mock_gt)()

        } else {
          tfrmt_app_out %>% safely(print_mock_gt)(.data = data)
        }

      })

      # module to get current page
      page_cur <- table_page_server("tbl_page", reactive(tab()$result))

      # subset to selected
      tab_sub <- reactive({

        req(!is.null(tab()$result))

        if (inherits(tab()$result, "gt_group")){
          tab()$result |> grp_pull(page_cur())
        } else{
          tab()$result
        }
      })

      # view table
      output$tbl_view <- renderUI({

        req(tab_sub())

          div(style = "height:100%; overflow-x: auto; overflow-y: auto; width: 100%",
            as_raw_html(
              tab_sub() %>%
                tab_style(style = cell_text(whitespace = "pre"),
                          locations = list(cells_stub(), cells_body(), cells_row_groups()))  %>%
                tab_options(
                  table.align = "left"
                )
            , inline_css = FALSE)
        )

      })

      # error msgs print
      output$error_msg <- renderUI({
        req(!is.null(tab()$error))
        HTML(p(paste(tab()$error)))
      })

      return(reactive(tab()$result))
    }
  )
}
