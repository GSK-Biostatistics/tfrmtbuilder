#' Main UI function
#' @noRd

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
                         base_font = font_collection(font_google("Lato", local = FALSE), "sans-serif"),
                         primary = "#254988",
                         bg = "#ffffff",
                         fg = "#000000"
                         ),
        tabPanel("Home", home_ui(ns("home"))),
        tabPanel("Initialize", load_ui(ns("load"))),
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
                                         div( datamapping_ui(ns("overview")), id = "content_border")),

                                tabPanel(div( h6("Body Plan", class = "zero_margin"),
                                              div("(Required)", id = "tab_note")),
                                         div( body_plan_ui(ns("body_plan")), id = "content_border")),

                                tabPanel(div( h6("Row Group Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( row_grp_plan_ui(ns("row_grp_plan")), id = "content_border")),

                                tabPanel(div( h6("Column Plan", class = "zero_margin"),
                                            div("(Optional)", id = "tab_note")),
                                         value = "Column Plan",
                                         div( col_plan_simple_ui(ns("col_plan")), id = "content_border")),

                                tabPanel(div( h6("Column Style Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( col_style_plan_ui(ns("col_style_plan")), id = "content_border")),

                                tabPanel(div( h6("Footnote Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( footnote_plan_ui(ns("footnote_plan")), id = "content_border")),

                                tabPanel(div( h6("Big Ns", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( big_n_ui(ns("big_n")), id = "content_border")),

                                tabPanel(div( h6("Titles", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( titles_ui(ns("titles")), id = "content_border")),

                                widths = c(3, 9),
                                well = FALSE
                                )
                              )
                            ),
                     column(6,
                            div(id = ns("sidebar"),
                                tabsetPanel(
                                  tabPanel(title = div("Table", class = "tab_names"), br(),
                                           table_view_ui(ns("tbl_view"))),
                                  tabPanel(title = div("Data", class = "tab_names"),
                                           DTOutput(ns("data_view")))
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

