# Big N  - top level module

# returns big_n_structure

big_n_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      h3("Big Ns", class = "heading_style",
         actionButton(ns("reset"), "Reset", icon = icon("undo")), class = "btn-reset"),
      shinyjs::hidden(
        p(id = ns("none"),
          "None supplied.")
      ),
      p(id = ns("some"), "Click table to edit. Only 1 entry possible"),
      uiOutput(ns("tbl")),
      br(),
      fluidRow(
        column(3, div(actionButton(ns("add"), "New", icon = icon("plus")), class = "btn-new")),
        column(3, offset = 1, div(shinyjs::disabled(actionButton(ns("delete"), "Delete", icon = icon("trash")))), class = "btn-delete")
      )
    ),
    br(),
    shinyjs::hidden(
      div(id = ns("customize"),
          fluidRow(
            big_n_edit_ui(ns("customize_pane"))
          ),
          fluidRow(
            column(3, div(actionButton(ns("save"), "Save", icon = icon("save")), class = "btn-save")),
            column(4, shinyjs::hidden(div(id = ns("invalid"), "Invalid Entry", style = "color: red;")))
          )
      )
    )
  )
}

#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app tfrmt object
#' @param mode_load mock mode w/ no data, w/ data, reporting
#'
#' @noRd
big_n_server <- function(id, data, tfrmt_app, mode_load){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      struct <- reactiveVal(NULL)

      data_bp <- reactiveVal(NULL)

      # reset to defaults
      observeEvent(input$reset,{
        req(mode()=="done")
        data_bp(data())
        struct(tfrmt_app()$big_n)
      })

      # set up the defaults
      observeEvent(tfrmt_app(),{
        struct(tfrmt_app()$big_n)
      })
      observeEvent(data(),{
        data_bp(data())
      })

      # display the  big_n_structure
      output$tbl <- renderUI({

        req(length(struct())>0)

        struct_txt <-  paste0(format_big_n_struct(struct()), collapse = "<br>") %>% list()

        create_struct_list_sortable(ns, struct_txt, mode())

      })

      # when any are selected, switch to edit mode
      onclick("items", expr = {

        if (!is_empty(struct())){
          mode("edit")
        }
      })

      # reactive representing currently selected row's data
      # reset to NULL if new format to be added
      selected <- reactiveVal(NULL)

      observeEvent(req(mode()=="edit"),{
        selected(
          struct()
        )
      })


      # add mode - add placeholder frmt structure & clear row selection so it is not passed along
      observeEvent(input$add, {

        # clear selection
        selected(NULL)

      })

      # keep track of which "mode" we're in: add new, edit existing, done (saved/deleted)
      mode <- reactiveVal("done")
      observeEvent(req(input$add>0), mode("add"))


      # css changes in response to mode/struct change
      observe({
        if (mode()=="edit"){
          item_active_id <- "item-1"
          shinyjs::addClass(id = item_active_id, class = "rank-list-select")

        } else if (mode()=="done"){

          shinyjs::removeClass(id = "item-1", class = "rank-list-select")

        }

        # show/hide the UI
        # enable/disable the add, delete buttons
        shinyjs::toggle("customize", condition = (mode() %in% c("add", "edit")))
        shinyjs::toggleState("add", condition = (mode()=="done" & length(struct())==0))
        shinyjs::toggleState("delete", condition = (mode() %in% c("add", "edit")))
        shinyjs::toggleState("delete", condition = (mode()=="add" | mode()=="edit" |
                                                      (mode()=="done" & length(struct())==1)))
        shinyjs::toggleClass(id = "sortable", class = "unclickable", condition = (mode() %in% c("add", "edit")))

      })


      # toggle the "no formats" message"
      observe({
        any_items <- length(struct())==0
        shinyjs::toggle("none", condition = any_items)
        shinyjs::toggle("some", condition = !any_items)
      })

      # customize server
      plans <- big_n_edit_server("customize_pane", data_bp, tfrmt_app, selected, mode_load)


      # when user presses "save", collect the inputs
      observeEvent(input$save,{

        if (!is.null(plans())){

          # save the current selections when button is pressed
          struct(plans())

          mode("done") # set to done mode
          selected(NULL) # clear row selections

        } else {

          shinyjs::show("invalid")
        }
      })

      # delete rows if requested
      observeEvent(input$delete,{

        # remove from list
        struct(NULL)

        # reset mode
        mode("done")
      })

      # return final struct only when in done mode
      big_n_out <- reactive({
        req(mode()=="done")
        if (is.null(struct())){
          list()
        } else {
          struct()
        }
      })

      # return
      return(
        big_n_out
      )


    })
}
