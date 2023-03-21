# Big N - editor module

# returns big_n_structure

big_n_edit_ui <- function(id){

  ns <- NS(id)

  tagList(
    h3("Filter conditions"),
    filters_ui(ns("filters")),
    h3("Format"),
    div(id = ns("frmt_outer"),
        textAreaInput(ns("frmt"), label = NULL, value = "frmt(\"\\nN = xx\")"), width = "100%", rows = 3),
    p(id = ns("invalid_txt"), style = "color: red;", "Invalid format entry")
      )

}

#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app tfrmt object
#' @param selected body_plan that is selected in (or being added to) the table
#' @param mode_load mock mode w/ no data, w/ data, reporting
#'
#' @noRd
big_n_edit_server <- function(id, data, tfrmt_app, selected, mode_load){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns


      # fill the text input with pre-selection
      # also reset back to default for new additions
      observe({

        if (!is.null(selected())){

          existing_frmt <- as.character(selected()$n_frmt)
          existing_frmt <- gsub("\n","\\\\n", existing_frmt)

        } else {

          existing_frmt <- "frmt(\"\\nN = xx\")"

        }

        updateTextAreaInput(session,
                            inputId = "frmt",
                            value = existing_frmt)
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

      # data filters module
      collected_filters <- filters_server("filters",
                                          data = reactive({if(mode_load()=="mock_no_data") NULL else data()}),
                                          tfrmt_app, selected,
                                          include = c("param"),
                                          null_to_default = FALSE,
                                          allow_create = reactive({if(mode_load()=="mock_no_data") TRUE else FALSE}))



      # combine filters + frmt into big_n_structure
      reactive({

          req(length(collected_filters())>0)
          req(frmt_out())

          param_val <- collected_filters()$param_val

        do.call("big_n_structure", list(param_val = param_val,
                                        n_frmt =  frmt_out()))

      })

    }
  )
}
