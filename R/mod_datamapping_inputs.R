# Data mapping module - input creation/selection - to be used for each tfrmt data arg

# Returns list of reactives including:
#   - settings: selected variables
#   - valid: whether or not the settings are complete/valid
#   - initial_state: whether it represents the "initial" state prior to any edits.

datamapping_inputs_ui <- function(id, setting_name){
  ns <- NS(id)

  tagList(
    div(class = "form-group shiny-input-container",
        tags$label(setting_name),
        div(
          id = ns("item_div"),
          span(id = ns("multiples"),
              div(circleButton(ns("addinput"), icon = icon("plus"), size = "xs"), class = "btn-circle"),
              div(circleButton(ns("dropinput"), icon = icon("minus"), size = "xs"), class = "btn-circle"),
              style = "display: flex; gap: 5px;")
        )
        )
  )
}

#' @param id module ID
#' @param settings
#'
#'
#' @noRd
datamapping_inputs_server <- function(id, data, settings_in, reset, multiple, required = reactive(TRUE)){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # hide add/drop inputs if multiple = FALSE
      observe({
        toggle("multiples", condition = multiple==TRUE)
      })

      # starts at zero, 1 for initial state, then increments for each edit
      state_counter <- reactiveVal(0)

      # define # of active dropdown menus
      active_items <- reactiveVal(NULL)

      # collect all input selections
      selected_items <- reactiveVal(NULL)

      # are all inputs filled in?
      settings_complete <- reactiveVal(NULL)

      observeEvent(req(reset()>0),{

        active_items(0)
        selected_items(NULL)
        settings_complete(NULL)

        state_counter(0)

        removeUI(paste0("#", ns("item_div_inputs")))

        existing_inputs <- names(input)[str_detect(names(input), "^item-")]
        for (i in existing_inputs){
          remove_shiny_inputs(ns, i, input)
        }

        insertUI(
            selector = paste0("#", ns("item_div")),
            where = "afterEnd",
            ui = div(id = ns("item_div_inputs"))
                )

        if(length(settings_in())>0){
          active <- seq_len(length(settings_in()))
        } else {
          active <- 0
        }

        # create the UI from scratch
        to_add <- append_input_vars(ns, input, settings_in(), active, data(), all = TRUE)

        insertUI(
          selector = paste0("#", ns("item_div_inputs")),
          where = "beforeEnd",
          ui = to_add
        )

        lapply(paste0("item-", active), function(x) freezeReactiveValue(input, x))

        # capture the unique ID #s for the current inputs
        active_items(active)

      })

      # capture # of additions/deletions for creating unique IDs
      counter <- reactiveVal(0)
      observeEvent(c(input$addinput, input$dropinput),{
        req(sum(input$addinput, input$dropinput)>0)
        counter(counter()+1)
      })

      # adding an input
      observeEvent(input$addinput,{

        if (length(active_items())>0){
          item_id <- last(active_items()) + counter()
        } else {
          item_id <- counter()
        }

        to_add <- append_input_vars(ns, input, settings_in(), item_id, data(), all = FALSE)

        insertUI(
          selector = paste0("#", ns("item_div_inputs")),
          where = "beforeEnd",
          ui = to_add,
          immediate = TRUE
        )

        freezeReactiveValue(input, paste0("item-", item_id))

        # remove any zeros (ahould only happen on init) & append new item id
         if (length(active_items())==0 || all(active_items()==0)){
          active_items(item_id)
        } else {
          active_items(c(active_items(), item_id))
        }

      })

      observe({
        req(active_items()>0)
        active_items <- (active_items())

        expected_inputs <- paste0("item-", active_items)

        vals <- lapply(expected_inputs, function(ind){
          input[[ind]]
        }) %>% set_names(expected_inputs)

        selected_items(vals)

        isolate({
          state_counter(state_counter()+1)
        })

      })


    # dropping an input
    observeEvent(input$dropinput,{

      # define # of items (not to go below starting #)
      min_items <- length(settings_in())

      len_grps_new <- length(active_items())-1

      if (len_grps_new >= min_items){

        item_id <- last(active_items())
        removeUI(
          selector = paste0("#", ns(paste0("item-", item_id, "_outer *"))),
          immediate = TRUE
        )
        remove_shiny_inputs(ns, paste0("item-", item_id), input)

        selected_items(selected_items()[-length(selected_items())])
        active_items(active_items()[-length(active_items())])
      }
    })

    observeEvent(reset(), {
      if(required()==FALSE) settings_complete(TRUE)
    })

    # mark invalid if empty selection
    observeEvent(selected_items(),{

      expected_inputs <- paste0("item-", active_items())
      req(all(expected_inputs %in% names(selected_items())))

      imap(selected_items(),  function(value, name){

        show <- is.null(value)

        feedbackDanger(inputId = name, color = "red", icon = NULL, text = NULL, show = show)

      })

      if (any(map_lgl(selected_items(), is.null))){
        settings_complete(FALSE)
      } else {
        settings_complete(TRUE)
      }

    })

    # selected items out
    settings <- reactive({
      if (!is.null(selected_items())){
        keep(selected_items(), function(x)!is.null(x)) %>% unlist() %>% unname()
      } else {
        NULL
      }
    })

    # output
    return(
      list(
        settings = settings,
        valid = reactive(settings_complete()),
        state_counter = state_counter #reactive(state_counter()==1)
      )
    )


    })
}
