# page plan editor module

# returns page_structure() object

page_plan_edit_ui <- function(id){

  ns <- NS(id)

  tagList(
    h3("Filter conditions"),
    filters_ui(ns("filters"))
  )

}

#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app tfrmt object
#' @param selected body_plan that is selected in (or being added to) the table
#'
#' @noRd
page_plan_edit_server <- function(id, data, tfrmt_app, selected){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns


      # data filters module
      collected_filters <- filters_server("filters", data, tfrmt_app, selected,
                                          include = c("group", "label"),
                                          null_to_default = FALSE,
                                          add_default_opt = TRUE,
                                          allow_create = reactive(FALSE))

      # combine filters + post space into a page_structure
      reactive({

         req(!is.null(collected_filters()$group_val) || !is.null(collected_filters()$label_val))

        page_structure(group_val = collected_filters()$group_val,
                       label_val = collected_filters()$label_val)

      })

    }
  )
}
