# Column plan - top level module

# returns column_plan

col_plan_simple_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      h3("Column Plan", class = "heading_style",
      actionButton(ns("reset"), "Reset", icon = icon("undo")), class = "btn-reset")
    ),
    p(id = ns("some"), "Click column name to edit. Note: Group/label columns have a black border and cannot be moved. Only groups may be dropped."),
    fluidRow(
      column(7,
             div(
               id = ns("sortable"),
               uiOutput(ns("all_buckets"))
             )
             ),
      column(5,
             br(),
             br(),
             br(),
             div(id = ns("rename_div"),
                 textInput(ns("rename"), label = NULL),
             actionButton(ns("save"), "Save", icon = icon("save"), class = "btn-primary"))
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
col_plan_simple_server <- function(id, data, tfrmt_app, mode_load){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      cols_dat_in <- reactiveVal(NULL)
      cols_dat_out <- reactiveVal(NULL)

      #mode
      mode_mock <- reactive({
        if(!mode_load()=="reporting") TRUE else FALSE
      })

      # beginning column data - 1 row per column cell
      observe({
        req(data())
        req(tfrmt_app())

        starting_dat <- cols_to_dat(data(), tfrmt_app(), isolate(mode_mock())) %>%
          mutate("__col_plan_selected__" = FALSE)

        cols_dat_in(starting_dat)
        cols_dat_out(starting_dat)

      })
      # reset
      observeEvent(input$reset,{
        cols_dat_out(cols_dat_in())
      })

      mode <- reactiveVal("done")

      # group/label order for tfrmt
      grp_lbl_ord <- eventReactive(tfrmt_app(),{
        c(tfrmt_app()$group %>% map_chr(as_label),
          tfrmt_app()$label %>% as_label())
      })


      col_name <- reactive({
        names(cols_dat_out() %>%
                            select(-contains("__col_plan_"), -contains("__tfrmt_new_name_"))) %>% last()
      })

      # Create all bucket lists - lower level column only
      output$all_buckets <- renderUI({

        col_name <- col_name()
        col_num <- ncol(cols_dat_out())

        new_name_col <- paste0("__tfrmt_new_name__", col_name())
        col_levs <- cols_dat_out()[[new_name_col]] %>% as.character()
        col_levs_orig <- cols_dat_out()[[col_name]]

        col_fixed <- cols_dat_out()$`__col_plan_fixed__`
        col_dropped <- cols_dat_out()$`__col_plan_dropped__`
        col_stub <- cols_dat_out()$`__col_plan_fixed_ord__`>0

        create_col_plan_sortable_simple(ns, col_levs, col_levs_orig, col_stub, col_fixed, col_dropped, mode())

      })

      observeEvent(input$item_list, {

        col_name <- names(cols_dat_out() %>% select(-contains("__col_plan_"))) %>% last()
        col_levs <- cols_dat_out()[[col_name]] %>% as.character()

        keep_ord <- input$item_list %>% as.numeric()
        drop_ord <- input$drop_list %>% as.numeric()

        keep_levs <- col_levs[keep_ord]
        drop_levs <- col_levs[drop_ord]
        all_new_levs <- c(keep_levs, drop_levs)

        new_dat <- cols_dat_out()

        # indicate which have been dropped
        if (length(drop_levs)>0){
          new_dat <- new_dat %>%
              mutate(`__col_plan_dropped__` = .data[[col_name]] %in% drop_levs)
        }
        if (length(keep_levs)>0){
          new_dat <- new_dat %>%
            mutate(`__col_plan_dropped__` = ! .data[[col_name]] %in% keep_levs)
        }

        # if out of order, reshuffle
        if (!all(sort(keep_ord)==keep_ord)){
          new_dat <- new_dat %>%
            mutate(!! col_name := factor(.data[[col_name]], levels = all_new_levs))  %>%
            arrange(desc(.data$`__col_plan_fixed_ord__`), .data[[col_name]])
        }

        cols_dat_out(new_dat)
        mode("done")
      })

      # when any are selected, switch to edit mode

      selected <- reactiveVal(NULL)
      selected_num <- reactiveVal(NULL)

      onclick("items", expr = {
        mode("edit")
        item_num <- as.numeric(input$`button-item`)

        selected_num(item_num)
        new_name_col <- paste0("__tfrmt_new_name__", col_name())

        selected_col <- cols_dat_out() %>%
          filter(row_number()==item_num) %>%
          select(orig = .data[[col_name()]],
                 new = .data[[new_name_col]])

        selected(selected_col)
      })

      observeEvent(mode(),{
        shinyjs::toggle("rename_msg", condition = !mode()=="edit")
        shinyjs::toggle("rename_div", condition = mode()=="edit")

        shinyjs::toggleClass(id = "sortable", class = "unclickable", condition = mode()=="edit")

      })

      observeEvent(req(mode()=="done"), {
        selected(NULL)
      })

      observeEvent(req(mode()=="edit"),{

        updateTextInput(session, inputId = "rename",
                        label = paste0("Edit ", selected()$orig), value = selected()$new,
                        placeholder = "Enter new name")

      })

      observeEvent(input$save, {
        req(input$rename)
        mode("done")

        new_name_col <- paste0("__tfrmt_new_name__", col_name())

        new_cols <- cols_dat_out() %>%
          mutate(across(all_of(c(col_name(), new_name_col)), ~ as.character(.x))) %>%
          mutate(!!new_name_col := ifelse(row_number()==selected_num(),
                                          input$rename,
                                          .data[[new_name_col]]))
        cols_dat_out(new_cols)
      })

       # return
       eventReactive(cols_dat_out(),{

         new_name_col <- paste0("__tfrmt_new_name__", col_name())
        col <- tfrmt_app()$column %>% map_chr(as_label)
        cols_to_keep <- cols_dat_out() %>%
          filter(!.data$`__col_plan_dropped__`)

        cols_to_keep_orig <- cols_to_keep[[col_name()]]
        cols_to_keep_rnm <- cols_to_keep[[new_name_col]]


        cols_out <- c()
        for (i in seq_along(cols_to_keep_orig)){
          orig <- cols_to_keep_orig[i]
          new <- cols_to_keep_rnm[i]
          new_el <- list(orig)

          if (!orig==new){
            names(new_el)<- new
          }
          cols_out <- c(cols_out, new_el)
        }

       args <- c(cols_out, list(.drop = TRUE))

       do.call("col_plan", args)

      })


    }
)
}
