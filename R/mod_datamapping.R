# Data mapping module - refactored

# Returns reactive list including:
#  - tfrmt: updated tfrmt with new column names
#  - data: updated generated mock data or original uploaded data (without the value column if in mock mode)
#  - mode: mock with data, mock no data, non mock "reporting"
#  - original: does this represent the original state (for automatically refreshing the table view)

datamapping_ui <- function(id){
  ns <- NS(id)

  tagList(
    h3("Data Mapping", class = "heading_style",
       div(style = "display: inline-block;",
           actionButton(ns("reset"), "Reset", icon = icon("undo")), class = "btn-reset")),

    datamapping_inputs_ui(ns("groups"), setting_name = "Groups"),
    datamapping_inputs_ui(ns("label"), setting_name = "Label"),
    datamapping_inputs_ui(ns("param"), setting_name = "Param"),
    datamapping_inputs_ui(ns("value"), setting_name = "Value"),
    datamapping_inputs_ui(ns("columns"), setting_name = "Columns"),
    datamapping_inputs_ui(ns("sorting_cols"), setting_name = "Sorting Columns"),


    actionButton(ns("save"), "Save", icon = icon("save"), class = "btn-refresh")
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

      # reset to defaults
      reset <- reactiveVal(0)
      observeEvent(req(input$reset>0), {
        reset(reset()+1)
      })
      observeEvent(settings_default(),{
        reset(reset()+1)
      })

      # server for each set of inputs
      groups_out <- datamapping_inputs_server("groups",
                                              data,
                                              settings_in = reactive(settings_default()$group),
                                              reset,
                                              multiple = TRUE,
                                              required = TRUE)

      label_out <- datamapping_inputs_server("label",
                                             data,
                                             settings_in = reactive(settings_default()$label),
                                             reset,
                                             multiple = FALSE,
                                             required = TRUE)

      param_out <- datamapping_inputs_server("param",
                                             data,
                                             settings_in = reactive(settings_default()$param),
                                             reset,
                                             multiple = FALSE,
                                             required = TRUE)
      value_out <- datamapping_inputs_server("value",
                                             data,
                                             settings_in = reactive(settings_default()$value),
                                             reset,
                                             multiple = FALSE,
                                             required = TRUE)

      columns_out <- datamapping_inputs_server("columns",
                                               data,
                                               settings_in = reactive(settings_default()$column),
                                               reset,
                                               multiple = TRUE,
                                               required = TRUE)

      sorting_cols_out <- datamapping_inputs_server("sorting_cols",
                                                    data,
                                                    settings_in = reactive(settings_default()$sorting_cols),
                                                    reset,
                                                    multiple = TRUE,
                                                    required = FALSE)

      # are all the inputs valid?
      valid <- reactive({

        req(!is.null(groups_out$valid()),
            !is.null(label_out$valid()),
            !is.null(param_out$valid()),
            !is.null(value_out$valid()),
            !is.null(columns_out$valid()),
            !is.null(sorting_cols_out$valid()))

        if (all(c(groups_out$valid(),
                  label_out$valid(),
                  param_out$valid(),
                  value_out$valid(),
                  columns_out$valid(),
                  sorting_cols_out$valid())==TRUE)) {
          TRUE
        } else {
          FALSE
        }
      })

      # is it the initial state and also valid? (for auto saving)
      initial_valid <- reactive({
        req(valid())

        req(!is.null(groups_out$initial_state()),
            !is.null(label_out$initial_state()),
            !is.null(param_out$initial_state()),
            !is.null(value_out$initial_state()),
            !is.null(columns_out$initial_state()))

        if (all(c(groups_out$initial_state(),
                  label_out$initial_state(),
                  param_out$initial_state(),
                  value_out$initial_state(),
                  columns_out$initial_state())==TRUE)) {
          TRUE
        } else {
          FALSE
        }

      })

      # allow saving if inputs are valid
      observe({
        shinyjs::toggleState("save", condition = valid())
      })

      # collect all selections
      settings_collected <- reactive({

        req(valid())

        list(
          group = groups_out$settings(),
          label = label_out$settings(),
          param = param_out$settings(),
          value = value_out$settings(),
          column = columns_out$settings(),
          sorting_cols = sorting_cols_out$settings()
        ) %>%
          discard(is.null)
      })

      # if invalid on initial state and settings are updated (and therefore valid),
      #   save button ready (add orange glow)
      observeEvent(settings_collected(),{

        req(initial_valid()==FALSE)

        shinyjs::addClass("save", class = "btn-danger")
        shinyjs::removeClass("save", class = "btn-refresh")
      })

      # if invalid, remove orange glow
      observe({
        req(valid()==FALSE)

        shinyjs::removeClass("save", class = "btn-danger")
        shinyjs::addClass("save", class = "btn-refresh")
      })

      # if save button pressed, remove orange glow
      observeEvent(input$save,{
        shinyjs::removeClass("save", class = "btn-danger")
        shinyjs::addClass("save", class = "btn-refresh")
      })


      # return settings if valid on initial state, or save button pressed
      save_counter <- reactiveVal(0)
      observeEvent(req(initial_valid()==TRUE),{
        save_counter(save_counter()+1)
      })
      observeEvent(input$save,{
        save_counter(save_counter()+1)
      })

     settings_out <- eventReactive(req(save_counter()>0),{

        tf <- isolate(tfrmt_orig())

        tfrmt_new <- do.call(tfrmt, settings_collected())

        # update groups if needed
        old_grps <- tf$group %>% map_chr(as_label)
        new_grps <- settings_collected()$group[1:length(old_grps)]

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

        if (mode()=="mock_with_data"){
          data <- data() %>% select(-as_label(tfrmt_out$value))
        } else {
          data <- data() %||% (make_mock_data(tfrmt_out))
        }

        # layer for return
        list(
            tfrmt = tfrmt_out,
            data = data,
            mode = mode(),
            original= initial_valid()
        )
      })
      return(settings_out)

    })
}
