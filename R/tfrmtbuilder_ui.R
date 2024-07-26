#' Main UI function
#' @noRd

tfrmtbuilder_ui <- function(id, mockmode){

  ns <- NS(id)

  tagList(
    fluidPage(
      titlePanel(""),
      includeCSS(system.file("www","styles.css", package = "tfrmtbuilder")),
      html_dependency_pretty(),
      useShinyjs(),
      useShinyFeedback(),
      navbarPage(
        windowTitle = "tfrmt Builder",
        title = span(tagList(fa_i("person-digging"), "tfrmt Builder"), class = "navheader_padding"),
        theme = bs_theme(bootswatch = "flatly",
                         base_font = font_collection(font_google("Lato", local = FALSE), "sans-serif"),
                         primary = "#254988",
                         bg = "#ffffff",
                         fg = "#000000"
                         ),
        id = ns("all_tabs"),
        tabPanel("Home", home_ui(ns("home"))),
        tabPanel("Initialize", load_ui(ns("load"))),
        tabPanel("Edit",
                 page_fluid(
                   layout_sidebar(
                     navset_pill_list(
                       id = ns("tabs"),
                                nav_panel(div( h6("Data Mapping", class = "zero_margin"),
                                              div("(Required)", id = "tab_note")),
                                         value = "Data Mapping",
                                         div( datamapping_ui(ns("overview")), id = "content_border")),

                       nav_panel(div( h6("Body Plan", class = "zero_margin"),
                                              div("(Required)", id = "tab_note")),
                                         div( body_plan_ui(ns("body_plan")), id = "content_border")),

                       nav_panel(div( h6("Row Group Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( row_grp_plan_ui(ns("row_grp_plan")), id = "content_border")),

                       nav_panel(div( h6("Column Plan", class = "zero_margin"),
                                            div("(Optional)", id = "tab_note")),
                                         value = "Column Plan",
                                         div( col_plan_simple_ui(ns("col_plan")), id = "content_border")),

                       nav_panel(div( h6("Column Style Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( col_style_plan_ui(ns("col_style_plan")), id = "content_border")),

                       nav_panel(div( h6("Footnote Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( footnote_plan_ui(ns("footnote_plan")), id = "content_border")),

                       nav_panel(div( h6("Page Plan", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( page_plan_ui(ns("page_plan")), id = "content_border")),

                       nav_panel(div( h6("Big Ns", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( big_n_ui(ns("big_n")), id = "content_border")),

                       nav_panel(div( h6("Titles", class = "zero_margin"),
                                              div("(Optional)", id = "tab_note")),
                                         div( titles_ui(ns("titles")), id = "content_border")),

                                widths = c(3, 9),
                                well = FALSE
                                ),
                     sidebar = sidebar(
                       navset_tab(
                         nav_panel(title = "Table", br(),
                                  table_outer_ui(ns("tbl_view"))),
                         nav_panel(title = "Data",
                                  DTOutput(ns("data_view")))
                       ),
                       position = "right",
                       width = "40%"
                     ),
                     fill = FALSE,
                     fillable = FALSE,
                     border = FALSE,
                     border_radius = FALSE
                              )
                            )
        ) ,
        tabPanel("Export", export_ui(ns("export"))),
        header = tags$script(HTML(paste0("var header = $('.navbar> .container-fluid');",
                             "header.append('<div style=\"float:right; margin-bottom:0; color:#fff;\">",
                                "<div class=\"form-group shiny-input-container\" style = \"margin-bottom:0; color:#fff;\">",
                                    "<div class=\"pretty p-default p-switch p-fill\">",
                                      "<input id=\"", ns("mockmode"), "\" type=\"checkbox\"/ ", ifelse(mockmode, "checked=\"checked\">", ">"),
                                      "<div class=\"state p-danger\">",
                                        "<label>",
                                        "<span >Mock Mode</span>",
                                        "</label>",
                                      "</div>",
                                    "</div>",
                                "</div>",
                             "</div>');")))
      )

    )
  )
}

