# tfrmt Table inner module

table_inner_ui <- function(id){

  ns <- NS(id)

  tagList(
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
#' @param data data for the table
#' @param tfrmt_app_out final tfrmt for the table
#' @param mode mock mode w/ no data, w/ data, reporting
#' @param auto_tbl
#'
#' @noRd
table_inner_server <- function(id, data, tfrmt_app_out, settings, auto_tbl){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # hide/show the table
      observe({
        shinyjs::toggle("tbl_div", condition = !is.null(tfrmt_app_out()))
        shinyjs::toggle("tbl_div_msg", condition = is.null(tfrmt_app_out()))
      })


      # table as reactive
      tab <- reactive({

        req(auto_tbl()>0)

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
      page_info <- table_page_server("tbl_page", reactive(tab()$result))

      # subset to selected
      tab_sub <- reactive({

        req(!is.null(tab()$result))

        if (inherits(tab()$result, "gt_group")){
          tab()$result %>% grp_pull(page_info$page_cur())
        } else{
          tab()$result
        }
      })

      # view table
      output$tbl_view <- renderUI({

        req(tab_sub())

        div(
          p(paste0("Displaying page ", page_info$page_cur(), " of ", page_info$page_tot())),
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
