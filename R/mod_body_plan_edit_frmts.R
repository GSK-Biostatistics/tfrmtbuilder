# Body plan - editor module - frmts section

# returns frmt or frmt_combine object

body_plan_edit_frmts_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      span(style = "display: flex; gap: 5px;",
           div(actionButton(ns("pst_frmt"), "frmt", icon = icon("plus")), class = "btn-frmt"),
           div(actionButton(ns("pst_frmt_when"), "frmt_when", icon = icon("plus")), class = "btn-frmt"),
           div(actionButton(ns("pst_frmt_combine"), "frmt_combine", icon = icon("plus")), class = "btn-frmt")
      )
    ),
    fluidRow(
      div(style = "margin-top:20px; width: 75%",
          div(id = ns("frmt_outer"),
              aceEditor(ns("frmt"), mode = "r", fontSize = 16, value = "frmt('XXX.X')",
                        wordWrap = TRUE,
                        minLines = 5,
                        maxLines = 8,
                        debounce = 500,
                        showLineNumbers = FALSE,
                        highlightActiveLine = FALSE,
                        autoScrollEditorIntoView = TRUE)
          )
      ),
      shinyjs::hidden(p(id = ns("invalid_txt"), style = "color: red;", "Invalid format entry"))
    )
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

        updateAceEditor(session,
                            editorId = "frmt",
                            value = existing_frmt)
      })



      # add a frmt when requested
      lapply(c("frmt","frmt_when","frmt_combine"), function(x) {

        observeEvent(input[[paste0("pst_", x)]],{

          dummy_fun <- get(paste0("dummy_", x), envir = asNamespace("tfrmtbuilder"))
          updateAceEditor(session,
                          editorId = "frmt",
                              value = paste0(input$frmt, dummy_fun()))
        })
      })


      # text entered - evaluate and check
      frmt_out <- reactive({
        string_to_tfrmtobj(input$frmt)
      })

      # validation indicators
      observe({
        shinyjs::toggleCssClass("frmt_outer", class = "invalid", condition = is.null(frmt_out()))
        shinyjs::toggle("invalid_txt", condition = is.null(frmt_out()))
      })

      return(frmt_out)

    }
  )

}
