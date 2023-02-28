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
                         base_font = c("sans-serif"), #font_google("Lato"),
                         primary = "#254988",
                         bg = "#ffffff",
                         fg = "#E64500"
                         ),
        # selected = "Edit",
        tabPanel("Load", load_ui(ns("load"))),
        tabPanel("Edit",
                 fluidPage(
                   fluidRow(
                     column(5,
                            tags$div(
                              class = "side_panel",
                            navlistPanel(
                              id = ns("tabs"),
                              tabPanel(div( h6("Data Mapping", class = "zero_margin"),
                                            div("(Required)", id = "tab_note")),
                                       datamapping_ui(ns("overview"))
                              ),
                              tabPanel(div( h6("Body Plan", class = "zero_margin"),
                                            div("(Required)", id = "tab_note")),
                                       body_plan_ui(ns("body_plan"))),
                              tabPanel(div( h6("Row Group Plan", class = "zero_margin"),
                                            div("(Optional)", id = "tab_note")),
                                       row_grp_plan_ui(ns("row_grp_plan"))),
                              tabPanel(div( h6("Column Plan", class = "zero_margin"),
                                            div("(Optional)", id = "tab_note")),
                                       col_plan_ui(ns("col_plan"))),
                              tabPanel(div( h6("Column Style Plan", class = "zero_margin"),
                                            div("(Optional)", id = "tab_note")),
                                       col_style_plan_ui(ns("col_style_plan"))),
                              tabPanel(div( h6("Footnote Plan", class = "zero_margin"),
                                            div("(Optional)", id = "tab_note")),
                                       footnote_plan_ui(ns("footnote_plan"))),
                              tabPanel(div( h6("Big Ns", class = "zero_margin"),
                                            div("(Optional)", id = "tab_note")),
                                       big_n_ui(ns("big_n"))),
                              widths = c(3, 9),
                              well = FALSE
                            )
                            )
                     )  ,
                     column(7,
                            div(id = ns("sidebar"),
                                tabsetPanel(
                                  tabPanel(title = "Table",
                                           br(),
                                           table_view_ui(ns("tbl_view"))),
                                  tabPanel(title = "Data",
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

