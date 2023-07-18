# Body plan - editor module

# returns frmt_structure

body_plan_edit_ui <- function(id){

  ns <- NS(id)

  tagList(
    h3("Filter conditions"),
    filters_ui(ns("filters")),
    h3("Format"),
    body_plan_edit_frmts_ui(ns("formats"))
  )
}

#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app tfrmt object
#' @param selected body_plan that is selected in (or being added to) the table
#'
#'
#' @noRd
body_plan_edit_server <- function(id, data, tfrmt_app, selected){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # data filters module

      # TODO - no allow_create for the uploaded data
      collected_filters <- filters_server("filters", data, tfrmt_app, selected,
                                          include = c("group", "label", "param"),
                                          allow_create = reactive(TRUE),
                                          null_to_default = TRUE)

      # output of the frmts editor
      frmt_out <- body_plan_edit_frmts_server("formats", selected = selected)

      # combine filters + frmts into a frmt_structure
      reactive({

        req(length(collected_filters())>0)
        req(!is.null(frmt_out()))

        frmt_out_ready <- list(frmt_out())

        if (!all(collected_filters()$param_val==".default") &&
            length(collected_filters()$param_val)==1){
          frmt_out_ready <- setNames(frmt_out_ready, collected_filters()$param_val)
        }

        do.call("frmt_structure", args = c(list(group_val = collected_filters()$group_val,
                                              label_val = collected_filters()$label_val),
                                              frmt_out_ready))

      })

    })
}
