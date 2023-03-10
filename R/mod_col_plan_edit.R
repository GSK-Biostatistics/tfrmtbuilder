# Column plan - editor module

# returns column_plan

col_plan_edit_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(3, p("Mark complete"), style= "font-weight: bold;"),
      column(2, p("Drop"), style= "font-weight: bold;"),
      column(7, p("Arrange Columns"), align = "center", style = "font-weight: bold;")
    ),
    uiOutput(ns("all_buckets"))
  )
}

#' @param id module ID
#' @noRd
col_plan_edit_server <- function(id, cols_dat_out, cols_confirmed, distribute){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

      # Create all bucket lists
      output$all_buckets <- renderUI({

        all_cols <- names(cols_dat_out())
        cols_confirmed <- isolate(cols_confirmed())

        lapply(1:length(all_cols), function(col_num){
          col_name <- all_cols[col_num]
          col_levs <- cols_dat_out() %>% select(1:col_num) %>% unique() %>% pull(.data[[col_name]])
          col_confirmed <- cols_confirmed[col_num]

          create_col_plan_sortable(ns, col_num, col_name, col_levs, col_confirmed, distribute(), width = "600px")

        })

      })

      # reactive to capture if any of the rows have been confirmed, and which ones
      cols_done <- reactive({
        cols_dat_out_cur <- req(isolate(cols_dat_out()))
        ncols <- cols_dat_out_cur %>% ncol()

        cols_done <- lapply(1:ncols, function(col_num){
          col_confirm <- input[[paste0("confirm_", col_num)]]
          if(!is.null(col_confirm) && col_confirm==TRUE){
            col_num
          }
        })

        cols_done %>% unlist %>% sort
      })


      ## observer when "confirm" button pressed - input$confirm_[col_num]
      # needs work b/c it is going to fire too much
      observeEvent(cols_done(), {

        cols_dat_out_cur <- cols_dat_out()
        cols_confirmed_new <- cols_confirmed()
        cols_confirmed_new[cols_done()] <- TRUE
        cols_confirmed(cols_confirmed_new)

        if (length(cols_done())>0){

          for (col_num in cols_done()){
            # browser()
            col <- names(cols_dat_out_cur)[col_num]

            # if (col_num>1){
            #
            #   keep_ord <- map(1:length(cols_dat_out_cur[[col]]), ~ input[[paste0("keep_", col_num, "_", .x)]]) %>%
            #     map_chr(~if(is_empty(.x)) NA else .x) %>%
            #     {bind_cols(cols_dat_out_cur, tibble(`__new_keep_ord`=.))}%>%
            #     group_by(across(1:(col_num-1))) %>%
            #     group_split() %>%
            #     map(~.x %>% pluck("__new_keep_ord") %>% na.omit())
            #
            #   nested_dat <- cols_dat_out_cur %>%
            #     group_by(across(1:(col_num-1))) %>%
            #     nest() %>%
            #     mutate(data = map2(data, row_number(), ~.x %>% filter(.data[[col]] %in% keep_ord[[.y]])))
            #
            # } else {

            keep_ord <- map(1:length(cols_dat_out_cur[[col]]), ~input[[paste0("keep_", col_num, "_", .x)]]) %>%
              discard(is_empty) %>%
              unlist()

            # adjust the data/levels
            new_dat <- cols_dat_out_cur %>%
              filter(.data[[col]] %in% keep_ord) %>%
              mutate(across(all_of(col), ~ factor(.x, levels = keep_ord)))%>%
              mutate(across(everything(), ~ fct_drop(.x))) %>%
              arrange(across(everything()))
            #  }

            cols_dat_out(new_dat)
          }

        }
      })

      return(cols_dat_out)

    }
  )
}
