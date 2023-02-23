#' Main UI function

tfrmtbuilder_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidPage(
      titlePanel(""),
      includeCSS(system.file("www","styles.css", package = "tfrmtbuilder")),
      useShinyjs(),
      navbarPage(
        windowTitle = "tfrmt Builder",
        title = "tfrmt Builder",
        theme = bs_theme(bootswatch = "flatly",
                         base_font = font_google("Lato"),
                         primary = "#254988",
                         bg = "#ffffff",
                         fg = "#E64500"
                         ),
        # selected = "Edit",
        tabPanel("Load", load_ui(ns("load"))),
        tabPanel("Edit",
                 fluidPage(
                   fluidRow(
                     column(7,
                            navlistPanel(
                              id = ns("tabs"),
                              tabPanel("Overview",
                                       fluidPage(
                                         fluidRow(
                                        #   column(5,
                                                  datamapping_ui(ns("overview")) #),
                                        #   column(7,
                                        #          h3("Data") ,
                                        # DTOutput(ns("data_view"))
                                        #           )
                                          )
                                       )),
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
                     column(5,
                            div(id = ns("sidebar"),
                                table_view_ui(ns("tbl_view"))
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

