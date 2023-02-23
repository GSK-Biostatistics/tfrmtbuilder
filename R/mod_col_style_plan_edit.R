# Col style plan - editor module

# returns col_style_structure

col_style_plan_edit_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(6,
             h3("Filter conditions"),
             filters_ui(ns("filters"))
      ),
      column(6,
             h3("Align"),
             radioButtons(ns("align_opts"), label = NULL,
                          choices = list("left", "right", "custom"), selected = character(0)),
             conditionalPanel("input.align_opts=='custom'",
                              textInput(ns("align_custom"), label = NULL, placeholder = "Enter character(s) to align on"),
                              ns = ns),
             h3("Width"),
             textInput(ns("width"), label = NULL, value = "")
      )
    )
  )

}

#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app tfrmt object
#' @param selected body_plan that is selected in (or being added to) the table
#'
#'
#' @noRd
col_style_plan_edit_server <- function(id, data, tfrmt_app, selected){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # fill the text input with pre-selection
      # also reset back to default for new additions
      observe({

       if (!is.null(selected())){
          existing_align <- selected()$align
          existing_width <- selected()$width
       } else {
         existing_align <- "left"
         existing_width <- NULL
        }

        if (existing_align %in% c("left","right")){
          updateRadioButtons(session, inputId = "align_opts", selected = existing_align)
        } else {
          updateRadioButtons(session, inputId = "align_opts", selected = custom)
          updateTextInput(session, inputId = "align_custom", existing_align)
        }


        if (!is.null(existing_width)){
          updateTextAreaInput(session,
                              inputId = "align",
                              value = existing_width)
        }
      })


      # data filters module
      collected_filters <- filters_server("filters", data, tfrmt_app, selected,
                                          include = c("column"),
                                          null_to_default = TRUE,
                                          allow_create = reactive(FALSE))


      # capture align
      align <- reactive({
        req(input$align_opts)

        if(input$align_opts=="custom"){
          req(input$align_custom)
          input$align_custom
        } else{
          paste0("\"", input$align_opts, "\"")
        }
      })

      # combine filters + align + width into col_style_structure
      reactive({

        req(length(collected_filters())>0)

        req(align())

                # TODO - accommodate spanning alignment

        cols <- collected_filters() %>% unlist() %>% unname()
        cols <- setdiff(cols, ".default")

        req(length(cols)>0)

        width <- if (input$width=="") "NULL" else input$width

        eval(parse(text = paste0("col_style_structure(col = c(", paste0(cols, collapse = ", "), "),",
                                 "align = ", align(), ", width = ", width, ")")))
      })

    }
  )
}