# for use with removeUI
remove_shiny_inputs <- function(ns, id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(ns(i))
    })
  )
}

# data-driven selectInputs: for selecting values of the tfrmt parameters (group, value, etc)
create_filter_select <- function(ns, type, data, existing_filters, var_vec,
                                 allow_create = TRUE,
                                 null_to_default = TRUE,
                                 add_default_opt = FALSE){

  # get the incoming settings for the given filter type (group_val, etc)
  existing_vars <- existing_filters %>%
    keep_at(type) %>%
    pluck(type)

  # create a named list
  #  - if already a named list (e.g. list(group = "val1")) then return
  #  - if not (e.g. ".default") then make it a named list (e.g. list(group=".default"))
  existing_vars <- map(existing_vars, function(x){

      if (is.list(x)){
        x
      } else if (all(x==".default")){
        rep(".default", length(var_vec)) %>% as.list() %>% setNames(var_vec)
      } else {
        list(x) %>% setNames(var_vec)
      }
    }) %>%
    list_flatten(name_spec = "{inner}")

  # remove any default values if all null are to be set to .default
  #   (placeholder text will say ".default" when non selected)
  if (null_to_default ||
      (!null_to_default && !add_default_opt)){
    existing_vars <- existing_vars  %>%
      discard(~all(.x==".default"))
  }

  # create a select input for each variable to be represented
  lapply(var_vec, function(v){

    # pull anything pre-selected for this variable
    filter_keep <-existing_vars %>% keep_at(v)

    # define pre-selections, if any
    if (length(filter_keep)>0){
      selected_vals <- filter_keep %>% unlist() %>% unname()
    } else {
      selected_vals <- character(0)
    }

    # define choices in the drop-down:
    #   - no data: choices are only the pre-selections
    #   - data: choices are the values in the data

    if (is.null(data)){
      if (length(selected_vals)>0){
        choices <- selected_vals
      } else {
        choices <- NULL
      }
    } else {
      choices <- data %>% pull(all_of(v)) %>% unique()
    }

    # add ".default" as a choice if required
    if (add_default_opt){
      choices <- c(".default",choices)
    }

    # define the placeholder text, depending on whether setting to NULL is an option
    if (null_to_default){
      placeholder <- ".default"
    } else {
      placeholder <- "None"
    }

    selectizeInput(inputId = ns(paste0("values-", v)),
                   label = HTML(paste0(type, ": <span style=\"font-weight: 400;\">", v, "</span>")),
                   choices = choices,
                   selected = selected_vals,
                   multiple = TRUE,
                   options = list(placeholder = placeholder, create = allow_create))

  })
}

input_dynamic_vars <- function(ns, all_vars, var, num_vars, data = NULL){

  if (num_vars==0){
    div()
  } else {
    # subset to only those in the data
    if (!is.null(data)){
      all_vars <- all_vars[which(all_vars %in% names(data))]
    }

    lapply(1:num_vars, function(i){

      if (i>length(all_vars)){
        placeholder <- "Enter variable name"
        value <- NULL
        choices <- c("",names(data))
        selected <- NULL
      } else {
        placeholder <- NULL
        choices <- names(data)
        selected <- value <- all_vars[i]
      }

      id <- ns(paste0(var, "-", i))

      if (is.null(data)){
        input_div <- textInput(id, label = NULL, value = value, placeholder = placeholder)
      } else {
        input_div <- selectInput(id, label = NULL, selected = selected, choices = choices)
      }

      div(id = paste0(id,"_outer"), input_div)

    })
  }

}

# function to dynamically insert variable selections
#' @param ns namespace
#' @param var_name name of variable class (for assigning ID's): "group", "label", etc
#' @param selected_vars selections for pre-populating
#' @param num_vars total # of entries to create
#' @param data data
#' @param all If num_vars>1, create entries for all (TRUE), or just the most recently added/last (FALSE)
#'
#' @noRd
append_input_vars <- function(ns,
                              input,
                              selected_vars,
                              active_vars,
                              data = NULL,
                              all = TRUE){

  if (all(active_vars==0)){
    div()
  } else {
    # subset to only those in the data
    if (!is.null(data)){
      selected_vars <- selected_vars[which(selected_vars %in% names(data))]
    }

    if (all){
      inputs <- active_vars  # create inputs for all
    } else {
      inputs <- last(active_vars) # just create an input for the last one
    }

    lapply(inputs, function(i){

      if (i>length(selected_vars)){
        value <- NULL
      selected <- character(0)
      } else {
        selected <- value <- selected_vars[i]
      }

      id <- ns(paste0("item-", i))

      if (is.null(data)){
        choices <- selected
        allow_create <- TRUE
        placeholder <- "Type or select variable"
      } else {
        choices <- c("", names(data))
        allow_create <- FALSE
        placeholder <- "Select variable"

      }
      input_div <- selectizeInput(id, label = NULL, selected = selected, choices = choices,
                     multiple = TRUE,
                     options = list(placeholder = placeholder, create = allow_create,
                                    maxItems = 1))

      div(id = paste0(id,"_outer"), input_div)

    })
  }
}

# create sortable list of *structure objects
create_struct_list_sortable <- function(ns, struct_list_txt, mode){

  ind <- 1:length(struct_list_txt)

  divs <- lapply(ind,
                 function(i) {

                   # if in add mode, highlight the added row
                   if (isolate(mode=="add") && i==max(ind)){
                     class = "class = \"rank-list-select\""
                   } else {
                     class = ""
                   }

                   # item HTML
                   HTML(paste0(
                     "<div id = ", ns(paste0("item-", i)),
                     " onclick = \"Shiny.setInputValue('",ns("button-item"), "', '", i,"')\" ",
                     class,
                     " style = \"padding: 10px 15px;\"",
                     " >",
                     struct_list_txt[[i]],
                     "</div>"
                   ))
                 }) %>%
    setNames(as.character(ind))

  # rank list for sortable
  rank_list(text = "",
            labels = divs,
            css_id = ns("items"),
            input_id = ns("item_list"))
}





# create sortable list of *structure objects
create_col_plan_sortable_simple <- function(ns, col_levs, col_levs_orig, col_stub, col_fixed, col_dropped, mode){

  ind <- 1:length(col_levs)

  divs <- lapply(ind,
                 function(i) {

                   # if in add mode, highlight the added row
                   if (isolate(mode=="add") && i==max(ind)){
                     class = "rank-list-select"
                   } else {
                     class = ""
                   }
                   if (col_fixed[i]){
                     class <- paste(class, "no-move")
                   }
                   if (col_stub[i]){
                     class <- paste(class, "rank-list-item-stub")
                   }

                   # item HTML
                   HTML(paste0(
                     "<div id = ", ns(paste0("item-", i)),
                     " onclick = \"Shiny.setInputValue('",ns("button-item"), "', '", i,"')\"",
                     " style = \"padding: 10px 15px;\"",
                     " class = \"", class , "\"",
                     " >",
                     col_levs[i],
                     "</div>"
                   ))
                 }) %>%
    setNames(as.character(ind))

  which_keep <- divs[! col_dropped]
  which_drop <- divs[col_dropped]

  bucket_list(
    header = NULL,
    add_rank_list(
      text = "Order Columns",
      labels = which_keep,
      css_id = ns("items"),
      input_id = ns("item_list"),
      options = sortable_options(filter = ".no-move")),
    add_rank_list(
      text ="Drop Columns",
      labels = which_drop,
      input_id = ns("drop_list")
    ),
    orientation = "vertical"
  )
}


# create sortable row of column values
create_col_plan_sortable <- function(ns, col_num, col_name, col_levs, col_confirmed, distribute, width){

  # UI elements for the "dropped" levels
  if (distribute=="Keep all"){
    contents_drop <- list()
  } else {
    contents_drop <- lapply(col_levs, function(x){div(class="itemlist-item", x)})
  }

  css_id_drop <- ns(paste0("levs_drop_", col_num))
  levs_drop <- tagList(
    div(class = "itemlist-start",
        div(class = "itemlist",
            id = css_id_drop,
            contents_drop)),
    sortable_js(css_id = css_id_drop,
                options = sortable_options(
                  group = list(
                    group = col_name,
                    put = TRUE,
                    pull = TRUE
                  ),
                  onSort = sortable_js_capture_input(input_id = ns(paste0("drop_", col_num)))
                )))

  # UI elements for the "keep" levels
  levs_keep <- lapply(1:length(col_levs), function(lev_num){

    css_id <- ns(paste0("levs_keep_", col_num, "_", lev_num))

    if (distribute=="Keep all"){
      col_val <- col_levs[lev_num]
      contents_keep <- div(class="itemlist-item", draggable = "false", col_val)
    } else {
      contents_keep <- list()
    }

    tagList(
      div(class = "itemlist-end",
          style = paste0("width: ", 100*(1/length(col_levs)), "%"),
          div(class = "itemlist",
              id = css_id,
             contents_keep)),
      sortable_js(css_id = css_id,
                  options = sortable_options(
                    swap = TRUE,
                    group = list(
                      group = col_name,
                      put = TRUE,
                      pull = TRUE
                    ),
                    onLoad = sortable_js_capture_input(input_id = ns(paste0("keep_", col_num, "_", lev_num))),
                    onSort = sortable_js_capture_input(input_id = ns(paste0("keep_", col_num, "_", lev_num)))
                  )))
  })


  tagList(
      p("Arrange Columns", style = "font-weight: bold;"),
      div(style = paste0("width:", width, "; display: flex; flex-direction: row; flex-wrap: wrap;"), levs_keep),
      br(),
      p("Drop Columns", style = "font-weight: bold;"),
      div(levs_drop, style = paste0("width: ", 100*(1/length(col_levs)), "%"))
    )

}



# arrange UI elements (like filters) in a grid

arrange_ui_grid <- function(el_list, el_width = 6){

  num_cols <- floor(12/el_width)
  num_rows <- ceiling(length(el_list)/num_cols)

  # for each element, determine the col row # and arrange
  row_num <- map_dbl(seq_along(el_list), function(i){ ceiling(i/num_cols)})

  el_list_cols <- lapply(el_list, function(el){
    column(width = el_width, el)
  })

  # assign the column elements to each row in a different list
  row_list <- vector(mode = "list", length = num_rows)
  for (i in seq_along(el_list_cols)){

    row <- row_num[i]
    row_list[[row]] <- c(row_list[[row]], el_list_cols[i])
  }

  # arrange in fluidRows
  lapply(row_list, function(row){fluidRow(row)})

}

