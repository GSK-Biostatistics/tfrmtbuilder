# row group plan editor module

# returns row_grp_structure() object

row_grp_plan_edit_ui <- function(id){

  ns <- NS(id)

  tagList(
    h3("Filter conditions"),
    filters_ui(ns("filters")),
    h3("Post Space"),
    fluidRow(
      div(style = "width: 50%;",
      aceEditor(ns("post_space"), value = "\"  \"", mode = "r", debounce = 0,
                fontSize = 16,
                wordWrap = TRUE,
                minLines = 2,
                maxLines = 2,
                showLineNumbers = FALSE,
                highlightActiveLine = FALSE,
                autoScrollEditorIntoView = TRUE)
      )
    )

  )

}

#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app tfrmt object
#' @param selected body_plan that is selected in (or being added to) the table
#'
#' @noRd
row_grp_plan_edit_server <- function(id, data, tfrmt_app, selected){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # fill the text input with pre-selection
      # also reset back to default for new additions
      observe({

        if (!is.null(selected())){
          existing_post_space <- selected()$block_to_apply$post_space
        } else {
          existing_post_space <- "  "
        }

        updateAceEditor(session,
                            editorId = "post_space",
                            value = paste0("\"", existing_post_space, "\""))
      })


      # data filters module
      collected_filters <- filters_server("filters", data, tfrmt_app, selected,
                                          include = c("group"),
                                          null_to_default = TRUE,
                                          allow_create = reactive(FALSE))

      # combine filters + post space into a row_grp_structure
      reactive({

        req(collected_filters()$group_val)
        req(!is.null(input$post_space))

        row_grp_structure(group_val = collected_filters()$group_val,
                         element_block(post_space = eval(parse( text = input$post_space))))

      })

    }
  )
}
