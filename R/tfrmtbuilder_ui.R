#' Main UI function

tfrmtbuilder_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidPage(
      titlePanel(""),
      includeCSS(system.file("www","styles.css", package = "tfrmtbuilder")),
      useShinyjs(),
      navbarPage(
        "tfrmt Builder",
        # selected = "Edit",
        tabPanel("Load", load_ui(ns("load"))),
        tabPanel("Edit",
                 fluidPage(
                   fluidRow(
                     column(5,
                            navlistPanel(
                              id = ns("tabs"),
                              tabPanel("Data Mapping", datamapping_ui(ns("overview"))),
                              tabPanel("Body Plan", body_plan_ui(ns("body_plan"))),
                              tabPanel("Row Group Plan", row_grp_plan_ui(ns("row_grp_plan"))),
                              tabPanel("Column Plan", col_plan_ui(ns("col_plan"))),
                              tabPanel("Column Style Plan", col_style_plan_ui(ns("col_style_plan"))),
                              tabPanel("Footnote Plan", footnote_plan_ui(ns("footnote_plan"))),
                              tabPanel("Big Ns", big_n_ui(ns("big_n"))),
                              widths = c(3, 9),
                              well = FALSE
                            )
                     )  ,
                     column(7,
                            div(id = ns("sidebar"),
                                tabsetPanel(
                                  tabPanel("Table",
                                           br(),
                                        table_view_ui(ns("tbl_view"))),
                                  tabPanel("Data", DTOutput(ns("data_view")))
                                  )
                            )
                     )
                   )
                 )
        ) ,
        tabPanel("Export", export_ui(ns("export")))
      )

    )
  )
}

