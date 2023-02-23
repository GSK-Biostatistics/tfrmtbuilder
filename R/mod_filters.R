# Variable value filtering module

# returns list of configured values for the requested parameters

filters_ui <- function(id){

  ns <- NS(id)

  tagList(
    uiOutput(ns("filters"))
  )
}


#' @param id module ID
#' @param data data for the table
#' @param tfrmt_app tfrmt object
#' @param selected *_plan that is selected in (or being added to) the table
#' @param include Character vector of parameters (group, label, param, column) to include in the filter creation
#' @param null_to_default Set any variables without values selected to ".default" (required for body_plan)
#' @param allow_create Allow for the creation of new values in the selectInputs (for tfrmt-driven mock creation)
#'
#'
#' @noRd
filters_server <- function(id, data, tfrmt_app, selected,
                           include,
                           null_to_default = TRUE,
                           allow_create = reactive(TRUE)){

    moduleServer(
      id,
      function(input, output, session){


        ns <- session$ns

        # shell of all requested vars to keep track
        var_shell <- reactive({
          req(tfrmt_app())

          include %>% map(~tfrmt_app()[[.x]] %>%
                            {if (is.list(.)){
                                map_chr(., as_label)
                              } else {
                                as_label(.)
                              }
                            }) %>%
            setNames(include)

        })


        # loop through all variables in the var shell
        output$filters <- renderUI({

          data <- isolate(data())

          ui_list <- vector("list", length(var_shell()))

          for (var in names(var_shell())){

            i <- which(var==names(var_shell()))

            if (tfrmt:::is_col_style_structure(selected())){
              selected_vars <- selected()$cols[[1]] %>% as_label %>%
                list(column_val = .)
            } else{
              selected_vars <- selected() %>%
                keep_at(paste0(var, "_val"))
            }

            all_vars <- var_shell()[[var]]
             ui_list[[i]] <- create_filter_select(ns, paste0(var, "_val"), data, selected_vars, all_vars, allow_create())

          }

          ui_list

        })


        collected_filters <- reactive({

          var_shell <- isolate(var_shell())
          all_vars <- var_shell %>% unlist() %>% unname()

          # get all input values at variable level (group1 = val1, group2 = val2)
          input_list <- lapply(all_vars,
                               function(i) {
                                 val<-input[[paste0("values-", i)]]
                                 if (null_to_default){
                                   if (is.null(val)){
                                    val <- ".default"
                                   }
                                 }
                                 val
                               }) %>%
            setNames(all_vars) %>%
            discard(is.null)

          # convert back to tfrmt input parameter level (group_val = list(group1 = val1, group2 = val2))
          vars_list <- list()

          for (var in names(var_shell)){

            i <- which(var==names(var_shell))
            selected_vars <- keep_at(input_list, var_shell[[var]])

            ## unlist if not a list to begin with
            if (! is.list(isolate(tfrmt_app()[[var]]))){
              selected_vars <- selected_vars %>% unlist() %>% unname()
            }

            vars_list[[paste0(var, "_val")]] <- selected_vars
          }

          vars_list %>%
            map(function(x){if(length(x)==0) NULL else x})

        })

        return(collected_filters)
      }
    )
}