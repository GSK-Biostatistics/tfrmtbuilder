# Footnote plan - editor module

# returns footnote_structure

footnote_plan_edit_ui <- function(id){

  ns <- NS(id)

  tagList(
    h3("Filter conditions"),
    filters_ui(ns("filters")),
    h3("Footnote"),
    textInput(ns("footnote"), label = NULL, value = "", width = "100%",
              placeholder = "Enter footnote text")
  )

}

#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app tfrmt object
#' @param selected body_plan that is selected in (or being added to) the table
#'
#'
#'
#' @noRd
footnote_plan_edit_server <- function(id, data, tfrmt_app, selected){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # fill the text input with pre-selection
      # also reset back to default for new additions
      observe({

       if (!is.null(selected())){
          existing_footnote <- selected()$footnote
       } else {
          existing_footnote <- NULL
        }

        updateTextAreaInput(session,
                            inputId = "footnote",
                            value = existing_footnote)
      })


      # data filters module
      collected_filters <- filters_server("filters", data, tfrmt_app, selected,
                                          include = c("column", "group","label"),
                                          null_to_default = FALSE,
                                          allow_create = reactive(FALSE))


      # combine filters + footnote into a footnote_structure
      reactive({

        req(length(collected_filters())>0)

        req(!is.null(input$footnote))

        footnote_structure(footnote_text = input$footnote,
                           column_val = collected_filters()$column_val,
                           group_val = collected_filters()$group_val,
                           label_val = collected_filters()$label_val)
      })

    }
  )
}
