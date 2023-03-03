# Data mapping module

# Returns updated tfrmt with new column names

datamapping_ui <- function(id){
  ns <- NS(id)

  tagList(
    h3("Settings", class = "heading_style",
       div(style = "display: inline-block;",
          # actionButton(ns("save"), "Save", icon = icon("save")),
           actionButton(ns("reset"), "Reset", icon = icon("undo")), class = "btn-reset")),
    div(class = "form-group shiny-input-container",
        tags$label("Groups"),
        span(
          div(circleButton(ns("addgrp"), icon = icon("plus"), size = "xs"), class = "btn-circle"),
          div(circleButton(ns("dropgrp"), icon = icon("minus"), size = "xs"), class = "btn-circle"),
          style = "display: flex; gap: 5px;"
        ),
        uiOutput(ns("grps"))),
    uiOutput(ns("single_settings")),
    div(class = "form-group shiny-input-container",
        tags$label("Columns"),
        span(
          div(circleButton(ns("addcol"), icon = icon("plus"), size = "xs"), class = "btn-circle"),
          div(circleButton(ns("dropcol"), icon = icon("minus"), size = "xs"), class = "btn-circle"),
          style = "display: flex; gap: 5px;"
        ),
        uiOutput(ns("cols"))) ,
    div(class = "form-group shiny-input-container",
        tags$label("Sorting Columns"),
        span(
          div(circleButton(ns("addsortcol"), icon = icon("plus"), size = "xs"), class = "btn-circle"),
          div(circleButton(ns("dropsortcol"), icon = icon("minus"), size = "xs"), class = "btn-circle"),
          style = "display: flex; gap: 5px;"
        ),
        uiOutput(ns("sortcols"))),
    div(actionButton(ns("save"), "Save", icon = icon("save")), class = "btn-save")
  )
}

#' @param id module ID
#' @param settings - list of data, tfrmt, mode
#'
#'
#' @noRd
datamapping_server <- function(id, data, tfrmt_orig, mode){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # set up the settings to be captured
      settings_default <- reactive({
          data()

           nms <- c("group","label","param","value","column","sorting_cols")
           tfrmt_orig()[nms] %>%
            map(function(x){
              if (is.null(x)){
                NULL
              } else if (is.list(x)){
                map_chr(x, as_label)
              } else {
                as_label(x)
              }
            }) %>%
            setNames(nms)

          })

      # tfrmt to go out
      settings_out <- reactiveVal(NULL)

      # define # of grp/cols vars
      num_grps <- reactiveVal(NULL)
      num_cols <- reactiveVal(NULL)

      sel_grps <- reactiveVal(NULL)
      sel_cols <- reactiveVal(NULL)

      num_sortcols <- reactiveVal(NULL)
      sel_sortcols <- reactiveVal(NULL)

      # track if the default settings are "complete" on init
      settings_default_complete <- reactiveVal(NULL)

      # reset to defaults
      reset <- reactiveVal(0)
      observeEvent(req(input$reset>0), {
         reset(reset()+1)
      })
      observeEvent(settings_default(),{
        reset(reset()+1)
      })

      observeEvent(req(reset()>0),{

       # settings_out(settings_default())

        sel_grps(settings_default()$group)
        sel_cols(settings_default()$column)
        sel_sortcols(settings_default()$sorting_cols)

        num_grps(length(sel_grps()))
        num_cols(length(sel_cols()))
        num_sortcols(length(sel_sortcols()))

        # will the settings be all filled out?
        if (is.null(data())){
          settings_default_complete(TRUE)
        } else {
          all_cols <- names(data())
          cols <- unlist(settings_default())
          if (all(cols %in% all_cols)){
            settings_default_complete(TRUE)
          } else {
            settings_default_complete(FALSE)
          }
        }

      })


      # adding a group
      observeEvent(input$addgrp,{
        num_grps(num_grps()+1)
      })

      # dropping a group
      observeEvent(input$dropgrp,{

        # define # of groups (not to go below starting #)
        min_grps <- length(settings_default()$group) #length(sel_grps())
        new <- num_grps()-1
        if (new<min_grps) new <- min_grps
        num_grps(new)

        # set up the selected groups reactive
        if(num_grps()>0){
          sel_grps(sel_grps()[1:num_grps()])
        } else {
          sel_grps(NULL)
        }
      })

      # adding a column
      observeEvent(input$addcol,{
        num_cols(num_cols()+1)
      })

      # dropping a column
      observeEvent(input$dropcol,{
        # define # of columns (not to go below starting #)
        min_cols <- length(settings_default()$column)#length(sel_cols())
        if (min_cols==0){ min_cols <- 1}
        new <- num_cols()-1
        if (new<min_cols) new <- min_cols
        num_cols(new)

        # set up the selected cols reactive
        sel_cols(sel_cols()[1:num_cols()])
      })

      # adding a sort col
      observeEvent(input$addsortcol,{
        num_sortcols(num_sortcols()+1)
      })

      # dropping a sortcol
      observeEvent(input$dropsortcol,{

        # define # of groups (not to go below starting #)
        min_sortcols <- length(settings_default()$sorting_cols)
        new <- num_sortcols()-1
        if (new<min_sortcols) new <- min_sortcols
        num_sortcols(new)

        # set up the selected groups reactive
        if(num_sortcols()>0){
          sel_sortcols(sel_sortcols()[1:num_sortcols()])
        } else {
          sel_sortcols(NULL)
        }
      })


      # fill in the default values
      output$single_settings <- renderUI({

        reset()

        settings_default <- settings_default()
        data <- isolate(data())

        if (is.null(data)){

          map(c("label", "param", "value"),
              function(x){
                div(
                  id = ns(paste0(x, "_outer")),
                  textInput(ns(x), label = stringr::str_to_title(x),
                          value = settings_default[[x]])
                )
              }) %>%
            tagList()

        } else{
          choices <- c("", names(data))

          map(c("label", "param", "value"),
              function(x){

                val <- if (settings_default[[x]] %in% choices) {settings_default[[x]]} else {character(0)}

                div(
                  id = ns(paste0(x, "_outer")),
                  selectInput(ns(x), label = stringr::str_to_title(x),
                              choices = choices, selected = val, selectize = TRUE)
                )
              }) %>%
            tagList()

        }
      })


      # generate the group selection based on # of groups selected
      output$grps <- renderUI({

        req(num_grps())
        reset()

        sel_grps <- isolate(sel_grps())

        input_dynamic_vars(ns, sel_grps, "group", num_grps(), isolate(data()))

      })

      # generate the col selection based on # of cols selected
      output$cols <- renderUI({

        req(num_cols())
        reset()
        sel_cols <- isolate(sel_cols())

        input_dynamic_vars(ns, sel_cols, "column", num_cols(), isolate(data()))

      })

      # generate the sorting cols selection based on # of sorting cols selected
      output$sortcols <- renderUI({

        req(num_sortcols())
        reset()
        sel_sortcols <- isolate(sel_sortcols())

        input_dynamic_vars(ns, sel_sortcols, "sorting_cols", num_sortcols(), isolate(data()))

      })


      # generate these UI even when tab not showing
      outputOptions(output, "single_settings", suspendWhenHidden = FALSE)
      outputOptions(output, "grps", suspendWhenHidden = FALSE)
      outputOptions(output, "cols", suspendWhenHidden = FALSE)
      outputOptions(output, "sortcols", suspendWhenHidden = FALSE)


      # get status of settings to determine whether to highlight "save" button
      settings_complete <- reactiveVal(NULL)
      observeEvent(reactiveValuesToList(input), {

        # get names of all current & expected inputs
        inputs <- reactiveValuesToList(input) %>% names
        expected_grps <- if (num_grps()>0) {paste0("group-", 1:num_grps())} else {character(0)}
        expected_cols <- if (num_cols()>0) {paste0("column-", 1:num_cols())} else {character(0)}
        expected_sortcols <- if (num_sortcols()>0) {paste0("sorting_cols-", 1:num_sortcols())} else {character(0)}
        expected_inputs <- c(expected_grps, expected_cols, expected_sortcols, "label", "param", "value")

        # # require them to be available
         if(all(expected_inputs %in% inputs)){

        # status check
        input_vals <- map(expected_inputs, function(x) input[[x]]) %>% set_names(expected_inputs)

         input_vals_empty <- map_lgl(input_vals, ~.x=="")
         if (any(input_vals_empty)){
           settings_complete(FALSE)

           # highlight inputs that are empty
           inputs_to_flag <- input_vals_empty[which(input_vals_empty)] %>% names()

           map(inputs_to_flag, ~shinyjs::addClass(paste0(.x, "_outer"), class = "invalid"))

           inputs_to_unflag <- input_vals_empty[which(! input_vals_empty)] %>% names()
           if (length(inputs_to_unflag)>0){
             map(inputs_to_unflag, ~shinyjs::removeClass(paste0(.x, "_outer"), class = "invalid"))
           }
         } else {
           settings_complete(TRUE)

           map(expected_inputs, ~shinyjs::removeClass(paste0(.x, "_outer"), class = "invalid"))
         }
         }
      })

      observe({
        shinyjs::toggleState("save", condition = settings_complete())
      })

      # collect all settings when "save" pressed or if all complete on initialization
      # - on initialization:
      observe({
              req(reset()>0)
              req(settings_default_complete())

              isolate({
                data <- data() %||% tfrmt:::make_mock_data(tfrmt_orig())

                settings_out(
                  list(tfrmt = tfrmt_orig(),
                       data = data,
                       mode = mode(),
                       original = TRUE)
                )
                }
              )
      })
      # - if save button pressed:
      observeEvent(input$save, {

          if (num_grps()>0){
            group_inputs <- paste0("group-", 1:num_grps())
            for (grp in group_inputs) req(input[[grp]])
            group_vars <- map_chr(group_inputs, function(x){
              val <- input[[x]]
              if (!val=="") val else NULL})

            sel_grps(group_vars)
          } else{
            group_vars <- vars()
          }

        column_inputs <- paste0("column-", 1:num_cols())

        for (col in column_inputs) req(input[[col]])
        column_vars <- map_chr(column_inputs, ~ input[[.x]])
        sel_cols(column_vars)


        if (num_sortcols()>0){
          sc_inputs <- paste0("sorting_cols-", 1:num_sortcols())
          for (sc in sc_inputs) req(input[[sc]])
          sc_vars <- map_chr(sc_inputs, function(x){
            val <- input[[x]]
            if (!val=="") val else NULL})

          sel_sortcols(sc_vars)
        } else{
          sc_vars <- NULL
        }

        req(input$label, input$param, input$value)

       new_settings <- list(
            group = group_vars,
            label = input$label,
            param = input$param,
            value = input$value,
            column = column_vars,
            sorting_cols = sc_vars
          ) %>%
            discard(is.null)

        if (!is_empty(new_settings)){
          # tfrmt old and new (to be layered)
          tf <- isolate(tfrmt_orig())

          tfrmt_new <- do.call(tfrmt, new_settings)

          # update groups if needed
          old_grps <- tf$group %>% map_chr(as_label)
          new_grps <- new_settings$group[1:length(old_grps)]

          grps_to_update <- which(!old_grps==new_grps)

          if (length(grps_to_update)>0){

            txt <- character(length(grps_to_update))
            for (i in grps_to_update){
              txt[i] <- paste0(new_grps[i], " = ", old_grps[i])
            }
            tf_txt <- paste0("tf %>% update_group(", paste(txt, collapse = ", "), ")")

            tf <- eval(parse(text = tf_txt))
          }

          tfrmt_out <- layer_tfrmt(tf, tfrmt_new)
          data <- data() %||% tfrmt:::make_mock_data(tfrmt_out)

          # layer for return
          settings_out(
            list(
              tfrmt = tfrmt_out,
            data = data,
            mode = mode(),
            original= FALSE
            )
          )
        } else {
          settings_out(NULL)
        }

      })

     return(settings_out)

    }
  )
}
