# Body plan - editor module - frmts section

# returns frmt or frmt_combine object

body_plan_edit_frmts_ui <- function(id){

  ns <- NS(id)

  tagList(
    div(style="display: inline-block; vertical-align:center; horizontal-align:center", class = "row-fluid",
        div(actionButton(ns("pst_frmt"), "frmt", icon = icon("plus")), class = "btn-frmt"),
        div(actionButton(ns("pst_frmt_when"), "frmt_when", icon = icon("plus")), class = "btn-frmt"),
        div(actionButton(ns("pst_frmt_combine"), "frmt_combine", icon = icon("plus")), class = "btn-frmt")
    ),
    div(id = ns("frmt_outer"),
        textAreaInput(ns("frmt"), label = "", value = "frmt('XXX.X')")),
    p(id = ns("invalid_txt"), style = "color: red;", "Invalid format entry")
  )

}

#' @param id module ID
#' @param selected body_plan that is selected in (or being added to) the table
#'
#'
#' @noRd
body_plan_edit_frmts_server <- function(id, selected){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # fill the text input with pre-selection
      # also reset back to default for new additions
      observe({

        if (!is.null(selected())){

          sel_frmt <- selected()$frmt_to_apply
          existing_frmt <- as.character(sel_frmt[[1]])
        } else {

          existing_frmt <- dummy_frmt()

        }

        updateTextAreaInput(session,
                            inputId = "frmt",
                            value = existing_frmt)
      })



      # add a frmt when requested
      lapply(c("frmt","frmt_when","frmt_combine"), function(x) {

        observeEvent(input[[paste0("pst_", x)]],{

          dummy_fun <- match.fun(paste0("dummy_", x))
          updateTextAreaInput(session,
                              inputId = "frmt",
                              value = paste0(input$frmt, dummy_fun()))
        })
      })


      # text entered - evaluate and check
      frmt_out <- reactive({
        string_to_tfrmtobj(input$frmt)
      }) %>%
        debounce(500)

      # validation indicators
      observe({
        shinyjs::toggleCssClass("frmt_outer", class = "invalid", condition = is.null(frmt_out()))
        shinyjs::toggle("invalid_txt", condition = is.null(frmt_out()))
      })

      return(frmt_out)

    }
  )

}
