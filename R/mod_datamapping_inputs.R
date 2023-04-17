# Data mapping module

# Returns updated tfrmt with new column names

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
#' @param settings - list of data, tfrmt, mode
#'
#'
#' @noRd
datamapping_inputs_server <- function(id, data, settings_in, reset, mode, multiple, required = TRUE){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # hide add/drop inputs if multiple = FALSE
      observe({
        toggle("multiples", condition = multiple==TRUE)
        toggle("multiples", condition = multiple==TRUE)

      })

      # starts at zero, 1 for initial state, then increments for each edit
      state_counter <- reactiveVal(0)

      # define # of active dropdown menus
      active_items <- reactiveVal(NULL)

      observeEvent(req(reset()>0),{

        active_items(0)

        state_counter(0)

        removeUI(paste0("#", ns("item_div_inputs")))
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

        # remove any zeros (ahould only happen on init) & append new item id
         if (length(active_items())==0 || all(active_items()==0)){
          active_items(item_id)
        } else {
          active_items(c(active_items(), item_id))
        }

      })


     # collect all input selections
      selected_items <- reactiveVal(NULL)


      observe({
        active_items <- (active_items())

        expected_inputs <- paste0("item-", active_items)

        vals <- lapply(expected_inputs, function(ind){
          input_ind <- input[[ind]]
          req(!is.null(input_ind))
            input_ind

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

    # are all inputs filled in?
    settings_complete <- reactiveVal(NULL)

    observe({
      if(required==FALSE) settings_complete(TRUE)
    })
    observeEvent(req(length(selected_items())==0 && length(active_items())==0),{
      if (required==FALSE){
        settings_complete(TRUE)
      }
    })

    # mark invalid if empty selection
    observeEvent(selected_items(),{

      expected_inputs <- paste0("item-", active_items())
      req(all(expected_inputs %in% names(selected_items())))


      imap(selected_items(),  function(value, name){

        toggleClass(id = paste0(name, "_outer"), class = "invalid", condition = (value==""))

      })

      if (any(selected_items()=="")){
        settings_complete(FALSE)
      } else {
        settings_complete(TRUE)
      }

    })


    # output
    return(
      list(
        settings = reactive({
          keep(selected_items(), function(x)!x=="") %>% unlist() %>% unname()
        }),
        valid = reactive(settings_complete()),
        initial_state = reactive(state_counter()==1)
      )
    )


    })
}
