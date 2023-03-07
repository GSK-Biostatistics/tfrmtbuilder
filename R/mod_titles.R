# titles module

# Returns  title and subtitle

titles_ui <- function(id){
  ns <- NS(id)

  tagList(
    h3("Titles", class = "heading_style",
       actionButton(ns("reset"), "Reset", icon = icon("undo")), class = "btn-reset"),
    textInput(ns("title"), "Title", value = NULL, placeholder = "None"),
    textInput(ns("subtitle"), "Subtitle", value = NULL, placeholder = "None"),
    div(actionButton(ns("save"), "Save", icon = icon("save")), class = "btn-save")
  )
}


titles_server <- function(id, tfrmt_orig){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns


      title_list_in <- reactiveVal(NULL)
      title_list_out <- reactiveVal(NULL)

      # set up the defaults
      observeEvent(tfrmt_orig(),{
         title_list_in(
           list(
           title = tfrmt_orig()$title,
           subtitle = tfrmt_orig()$subtitle
          )
         )

        title_list_out(title_list_in())
      })
      # reset to defaults
      observeEvent(input$reset,{
        title_list_in(
          list(
            title = tfrmt_orig()$title,
            subtitle = tfrmt_orig()$subtitle
          )
        )
        title_list_out(title_list_in())
      })

    observe({
      existing_title <- title_list_in()$title
      existing_subtitle <- title_list_in()$subtitle

      if (!is.null(existing_title)){
        updateTextInput(session, "title", value = existing_title)
      }
      if (!is.null(existing_subtitle)){
        updateTextInput(session, "subtitle", value = existing_subtitle)
      }
    })

    observeEvent(input$save,{

      title <- if (input$title=="") NULL else input$title
      subtitle <- if (input$subtitle=="") NULL else input$subtitle

      title_list_out(
        list(
          title = title,
          subtitle = subtitle
        )
      )
    })

    return(title_list_out)


    }
  )
}
