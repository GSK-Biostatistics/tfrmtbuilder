# Column plan - top level module

# returns column_plan

col_plan_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      h3("Col Plan", class = "heading_style",
      actionButton(ns("reset"), "Reset", icon = icon("undo")), class = "btn-reset")
    ),
    fluidRow(
      column(5, radioGroupButtons(ns("distribute"),label = NULL, choices = c("Drop all", "Keep all"), selected = "Keep all"))
    ),
    col_plan_edit_ui(ns("col_plan_edit")),
    br(),
    fluidRow(
      column(2,  actionButton(ns("save"), "Save", icon = icon("save"), class = "btn-primary"))
    )
  )
}

#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app tfrmt object
#' @param mode_load mock mode w/ no data, w/ data, reporting

#'
#' @noRd
col_plan_server <- function(id, data, tfrmt_app, mode_load){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      cols_dat_in <- reactiveVal(NULL)
      cols_dat_out <- reactiveVal(NULL)
      cols_confirmed <- reactiveVal(NULL)

      # beginning column data - 1 row per column cell
      observe({
        req(data())
        req(tfrmt_app())

        starting_dat <- cols_to_dat(data(), tfrmt_app())

        cols_dat_in(starting_dat)
        cols_dat_out(starting_dat)

        ncols <- ncol(starting_dat)
        cols_confirmed(rep(FALSE, ncols))
      })
      # reset
      observeEvent(input$reset,{
        cols_dat_out(cols_dat_in())

        ncols <- ncol(cols_dat_out())
        cols_confirmed(rep(FALSE, ncols))
      })


      plan <- col_plan_edit_server("col_plan_edit", cols_dat_out, cols_confirmed, reactive(input$distribute))


      # return
     # eventReactive(input$save,{
      eventReactive(plan(),{

        grps <- tfrmt_app()$group %>% map_chr(as_label)
        lbl <- tfrmt_app()$label %>% as_label
        col <- tfrmt_app()$column %>% map_chr(as_label)

        # TODO spanning - just keep lowest level in meantime
        cols_to_keep <- cols_dat_out()[[last(col)]] %>% as.character()

        args <- as.list(c(grps, lbl, cols_to_keep))
        args <- c(args,  list(.drop = TRUE))
        do.call("col_plan", args)

      })


    }
)
}
