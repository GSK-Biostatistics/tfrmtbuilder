# for use with removeUI
remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}

# data-driven selectInputs: for selecting values of the tfrmt parameters (group, value, etc)
create_filter_select <- function(ns, type, data, existing_filters, var_vec, allow_create = TRUE){
 
  existing_vars <- existing_filters %>%
    keep_at(type) %>%
    map2(., names(.), function(x, y ){

      if (is.list(x)){
        x
      } else if (is.null(x) || x==".default"){
         x
      } else {
        as.list(x) %>% setNames(var_vec)
      }
    }) %>%
    list_flatten(name_spec = "{inner}") %>%
    discard(~all(.x==".default"))

  lapply(var_vec, function(v){

    filter_keep <-existing_vars %>% keep_at(v)
    if (length(filter_keep)>0){
      selected_vals <- filter_keep %>% unlist() %>% unname()
    } else {
      selected_vals <- character(0)
    }
    
    choices <- if (is.null(data)) NULL else data %>% pull(all_of(v)) %>% unique()
    selectizeInput(inputId = ns(paste0("values-", v)),
                   label = HTML(paste0(type, ": <span style=\"font-weight: 400;\">", v, "</span>")),
                   choices = choices,
                   selected = selected_vals,
                   multiple = TRUE,
                   options = list( placeholder = "None", create = allow_create))

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
        textInput(id, label = NULL, value = value, placeholder = placeholder)
      } else {
        selectInput(id, label = NULL, selected = selected, choices = choices)
      }

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
                     "style = \"padding: 10px 15px;\"",
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


# create sortable row of column values
create_col_plan_sortable <- function(ns, col_num, col_name, col_levs, col_confirmed, distribute){

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
      contents_keep <- div(class="itemlist-item", col_val)
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
    fluidRow(
      column(3,   
             prettyCheckbox(ns(paste0("confirm_", col_num)),
                              label = paste0("`", col_name, "`"),
                              status = "success", 
                              value = col_confirmed,
                              icon = icon("check"))
             ),
      column(2,  
             levs_drop),
      column(7, 
             div(style = "width:800px; display: flex; flex-direction: row; flex-wrap: wrap;", levs_keep))
      ) 
    )
      
}

