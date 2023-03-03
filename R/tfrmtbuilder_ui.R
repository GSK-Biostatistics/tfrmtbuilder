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
        title = div("tfrmt Builder", class = "navheader_padding"),
        theme = bs_theme(bootswatch = "flatly",
                         base_font = c("sans-serif"),
                         primary = "#254988",
                         bg = "#ffffff",
                         fg  = "black"
                         #fg = "#E64500"
        ),
        tabPanel("Load", load_ui(ns("load"))),
        tabPanel("Edit",
                 fluidPage(
                   fluidRow(
                     column(6,
                            tags$div(
                              class = "side_panel",
                              navlistPanel(
                                id = ns("tabs"),
                                tabPanel(div( h6("Data Mapping", class = "zero_margin"),
                                              div("(Required)", id = "tab_note")),
                                         value = "Data Mapping",
                                         datamapping_ui(ns("overview"))
                                ),
                                tabPanel(div( h6("Body Plan", class = "zero_margin"),
                                              div("(Required)", id = "tab_note")),
                                         value = "Body Plan",
                                         body_plan_ui(ns("body_plan"))),
                                tabPanel(div( h6("Row Group Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         value = "Row Group Plan",
                                         row_grp_plan_ui(ns("row_grp_plan"))),
                                tabPanel(div( h6("Column Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         value = "Column Plan",
                                         col_plan_simple_ui(ns("col_plan"))),
                                tabPanel(div( h6("Column Style Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         value = "Column Style Plan",
                                         col_style_plan_ui(ns("col_style_plan"))),
                                tabPanel(div( h6("Footnote Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         value = "Footnote Plan",
                                         footnote_plan_ui(ns("footnote_plan"))),
                                tabPanel(div( h6("Big Ns", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         value = "Big Ns",
                                         big_n_ui(ns("big_n"))),
                                widths = c(3, 9),
                                well = FALSE
                              )
                            )
                     )  ,
                     column(6,
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

