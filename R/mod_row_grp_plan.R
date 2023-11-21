# Row group plan - top level module

# returns row_grp_plan()

row_grp_plan_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      h3("Row Group Plan", class = "heading_style",
         actionButton(ns("reset"), "Reset", icon = icon("undo")), class = "btn-reset"),
      h4("Label location"),
      shinyWidgets::radioGroupButtons(
        inputId = ns("label_loc"), label = NULL,
        choices = c("indented", "spanning", "column", "noprint", "gtdefault"),
        selected = character(0)
      ),
      h4("Row Group Structures"),
      shinyjs::hidden(
        p(id = ns("none"),
          "None supplied.")
      ),
      p(id = ns("some"), "Click table entry to edit"),
      div(
        id = ns("sortable"),
        uiOutput(ns("tbl"))
        ),
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
           row_grp_plan_edit_ui(ns("customize_pane"))
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
row_grp_plan_server <- function(id, data, tfrmt_app, mode_load){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      struct_list <- reactiveVal(NULL)

      data_bp <- reactiveVal(NULL)

      # reset to defaults
      observeEvent(input$reset,{
        req(mode()=="done")
        data_bp(data())
        struct_list(tfrmt_app()$row_grp_plan$struct_list)

        shinyWidgets::updateRadioGroupButtons(session, "label_loc",
                                              selected = tfrmt_app()$row_grp_plan$label_loc$location)
      })

      # set up the defaults
      observeEvent(tfrmt_app(),{

        existing_rgp <- tfrmt_app()$row_grp_plan$struct_list
        if (!is_empty(existing_rgp)){
          struct_list(existing_rgp)
        }

        selected <- tfrmt_app()$row_grp_plan$label_loc$location %||% "indented"

        shinyWidgets::updateRadioGroupButtons(session, "label_loc",
                                              selected = selected)
      })
      observeEvent(data(),{
        data_bp(data())
      })


    #  display the row_grp_structures
      output$tbl <- renderUI({

        req(length(struct_list())>0)

        struct_list_txt <- map(struct_list(),
                               ~.x %>% format_row_grp_struct() %>% {paste0(., collapse = "<br>")})

        create_struct_list_sortable(ns, struct_list_txt, mode())

      })

      # when the list is sorted, reshuffle the row_grp_structures
      observeEvent(input$item_list, {

        list_ord <- input$item_list %>% as.numeric()

        # if out of order, reshuffle

        if (!all(sort(list_ord)==list_ord)){

          struct_list(struct_list()[list_ord])

          mode("done")

        }
      })


      # when any are selected, switch to edit mode
      onclick("items", expr = {

        last_struct <- pluck(struct_list(), length(struct_list()))
        if(!is_empty(last_struct)){
          mode("edit")
        }
      })

      # reactive representing currently selected row's data
      # reset to NULL if new format to be added
      selected <- reactiveVal(NULL)
      item_num_active <- reactiveVal(NULL)

      observeEvent(req(mode()=="edit"),{

        item_num <- as.numeric(input$`button-item`)

        item_num_active(item_num)

        selected(
          struct_list()[[item_num]]
        )
      })


      # add mode - add placeholder frmt structure & clear row selection so it is not passed along
      observeEvent(input$add, {

        # add an empty row_grp_structure
        struct_list(c(struct_list(), list(NULL)))
        item_num_active(length(struct_list()))

        # clear selection
        selected(NULL)

      })

      # keep track of which "mode" we're in: add new, edit existing, done (saved/deleted)
      mode <- reactiveVal("done")
      observeEvent(req(input$add>0), mode("add"))


      # css changes in response to mode change
      observeEvent(mode(),{

        if (mode()=="edit"){
          item_active_id <- paste0("item-", item_num_active())
          shinyjs::addClass(id = item_active_id, class = "rank-list-select")

        } else if (mode()=="done"){

          len_items <- length(struct_list())
          item_ids <- paste0("item-", 1:len_items)
          for (i in item_ids){
            shinyjs::removeClass(id = i, class = "rank-list-select")
          }
        }

        # show/hide the UI
        # enable/disable the add, delete buttons
        shinyjs::toggle("customize", condition = (mode() %in% c("add", "edit")))
        shinyjs::toggleState("add", condition = (mode()=="done"))
        shinyjs::toggleState("delete", condition = (mode() %in% c("add", "edit")))
        shinyjs::toggleClass(id = "sortable", class = "unclickable", condition = (mode() %in% c("add", "edit")))

      })


      # toggle the "no formats" message"
      observe({
        any_items <- length(struct_list())==0
        shinyjs::toggle("none", condition = any_items)
        shinyjs::toggle("some", condition = !any_items)

      })

      # ensure selected() is updated in case of 2 "adds" in a row (selected stays NULL)
      selected2 <- reactive({
        req(item_num_active())
        selected()
      })
      # customize server
       plans <- row_grp_plan_edit_server("customize_pane", data_bp, tfrmt_app, selected2)


      # when user presses "save", collect the inputs
      observeEvent(input$save,{

        struct_list_existing <- struct_list()

        # replace the highlighted row
        current_id <- item_num_active()

        # update the list of row_grp_structures
        if (!is.null(plans())){
          struct_list_existing[[current_id]] <- plans()

          # save the current selections to the list when button is pressed
          struct_list(struct_list_existing)

          mode("done") # set to done mode
          selected(NULL) # clear row selections

        } else {

          shinyjs::show("invalid")
        }
      })

      # delete rows if requested
      observeEvent(input$delete,{

        # remove from list
        if(!is.null(item_num_active())){
          struct_list(
            struct_list()[-item_num_active()]
          )
        }

        # reset mode
        mode("done")
      })


      # recreate data (mock no data only) when row_grp_structure_list is updated following a save, deletion, or reorder
      observeEvent(struct_list(), {

        req(mode()=="done")

        shinyjs::hide("invalid")

        if (mode_load()=="mock_no_data"){

          new_tfrmt <- tfrmt_app()

          new_tfrmt$row_grp_plan <- do.call("row_grp_plan", struct_list())

          if (length(struct_list())>0){
            new_data <- make_mock_data(new_tfrmt)
          } else {
            new_data <- data()
          }

          data_bp(new_data)

        }

      })

      # return final struct_list only when in done mode
      row_grp_plan_out <- reactive({
        req(mode()=="done")
        req(input$label_loc)

        arg_list <- list(label_loc = element_row_grp_loc(location = input$label_loc))
        if (!is.null(struct_list())){
          arg_list <- c(struct_list(), arg_list)
        }

         do.call("row_grp_plan", arg_list)
      })

      # return
      return(
        row_grp_plan_out
      )


    })

}
