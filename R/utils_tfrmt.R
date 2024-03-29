
# fill in any missing bits of the input tfrmt for use in the app (helpful for testing)
prep_tfrmt_app <- function(tf){
  if (is_empty(tf$group)){
    tf$group <- vars(!!sym("group"))
  }

  if (quo_is_missing(tf$label)){
    tf$label <- quo(!!sym("label"))
  }

  if (quo_is_missing(tf$param)){
    tf$param <- quo(!!sym("param"))
  }
  if (is_empty(tf$column)){
    tf$column <- vars(!!sym("column"))
  }

  if(quo_is_missing(tf$value)){
    tf$value <- quo(!!sym("value"))
  }

  if(is.null(tf$body_plan)){
    tf$body_plan <- body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  }

  if(is.null(tf$page_plan)){
    tf$page_plan <- page_plan(
      max_rows = 60
    )
  }

  tf
}

# Helpers to format *plans for display in the sortable table ------------

# format at the variable level
create_format_txt <- function(x){

  if(is.null(x)){
    vals <- character(0)
  }else  if (is.list(x)){
    vals <- x %>% map(unique)
  } else {
    vals <- unique(x)
  }

  if(is.list(vals)){
    vals_string <- paste0(
      sapply(names(vals), function(y) {
        paste0(" `",y,"` - ", paste0("\"", x[[y]], "\"", collapse = ", "))
      }),
      collapse = ";"
    )
  }else if (length(vals)>0){
    vals_string <- paste0(" \"",vals,"\"", collapse=",")
  } else {
    vals_string <- ""
  }

  vals_string
}

# format frmt_structure objects
format_frmt_struct <- function(x){

    if (is.null(x)){
      return(c("<b>Group Values:", "Label Values:","Format:</b>"))
    }

    group_string <- create_format_txt(x$group_val)
    labels <- unique(x$label_val)
    param <- unique(x$param_val)
    fmts <- x$frmt_to_apply[[1]]

    frmt_struct_str <- c(
      paste0("<b>Group Values:</b>",group_string),
      paste0("<b>Label Values:</b>",paste0("\"",labels,"\"", collapse=", "))
    )

    if(!identical(param,".default")){
      frmt_struct_str <- c(
        frmt_struct_str,
        paste0("<b>Param Values:</b>",paste0("\"",param,"\"", collapse=", "))
      )
    }

    frmt_struct_str <- c(
      frmt_struct_str,
      paste0("<b>Format:</b>",format(fmts))
    )

    frmt_struct_str
}


# format footnote_structure objects
format_footnote_struct <- function(x){

  if (is.null(x)){
    return(c("<b>Column Values:","Group Values:","Label Values:","Footnote:</b>"))
  }
  column_string <- create_format_txt(x$column_val)
  group_string <- create_format_txt(x$group_val)
  label_string <- create_format_txt(x$label_val)
  fmts <- x$footnote_text

  footnote_struct_str <- c(
    paste0("<b>Column Values:</b>",column_string),
    paste0("<b>Group Values:</b>",group_string),
    paste0("<b>Label Values:</b>",label_string),
    paste0("<b>Footnote:</b>\"",fmts,"\"")
  )

  footnote_struct_str

}

# format row_grp_structure objects
format_row_grp_struct <- function(x){

  if (is.null(x)){
    return(c("<b>Group Values:", "Post Space:</b>"))
  }

  group_string <- create_format_txt(x$group_val)
  fmts <- x$block_to_apply$post_space

  row_grp_struct_str <- c(
    paste0("<b>Group Values:</b>",group_string),
    paste0("<b>Post Space:</b> \"", fmts, "\"")
  )

  row_grp_struct_str
}

# format col_style_structure objects
format_col_style_struct <- function(x){

  if (is.null(x)){
    return(c("<b>Column Values:", "Align:", "Width:</b>"))
  }

  col_string <- map_chr(x$cols, as_label)
  col_string <-  paste(paste0("\"", col_string, "\""), collapse = ", ")

  align <- x$align
  align <-  paste(paste0("\"", align, "\""), collapse = ", ")
  width <- x$width %||% "default"


  col_style_struct_str <- c(
    paste0("<b>Column Values:</b> ",col_string),
    paste0("<b>Align:</b> ", align),
    paste0("<b>Width:</b> ", width)
  )

  col_style_struct_str
}

# format big_n_structure objects
format_big_n_struct <- function(x){

  if (is_empty(x)){
    return(c("<b>Param Value:", "N Format:</b>"))
  }

  param <- unique(x$param_val)
  fmts <- x$n_frmt

  frmt_struct_str <- paste0("<b>Param Value:</b> \"",param,"\"")

  frmt_struct_str <- c(
    frmt_struct_str,
    paste0("<b>N Format:</b>",format(fmts))
  )

  frmt_struct_str
}

# format page_structure objects
format_page_struct <- function(x){

  if (is.null(x)){
    return(c("<b>Group Values:","Label Values:</b>"))
  }
  group_string <- create_format_txt(x$group_val)
  label_string <- create_format_txt(x$label_val)

  page_struct_str <- c(
    paste0("<b>Group Values:</b>",group_string),
    paste0("<b>Label Values:</b>",label_string)
  )

  page_struct_str

}

# template frmt objects
dummy_frmt <- function(){
  "frmt(\"xx.x\",
  missing = NULL,
  scientific = NULL,
  transform = NULL)"
}
dummy_frmt_when <- function(){
  "frmt_when(
  \">3\" ~ frmt(\"(X.X%)\"),
  \"<=3\" ~ frmt(\"Undetectable\")
  )"

}
dummy_frmt_combine <- function(){
  "frmt_combine(
 \"{param1} {param2}\",
 param1 = frmt(\"XXX %\"),
 param2 = frmt(\"XX.XXX\")
)"
}

# convert string to frmt obj (for going from text -> R obj)
string_to_tfrmtobj <- function(obj){

  tryCatch({
    obj_eval <- eval(parse(text = obj))
    if (is_frmt(obj_eval)) obj_eval else NULL
  },
  error = function(e){
    NULL
  })

}

# function inspired by tfrmt column helpers to get the order of col levels/spans based on the data and col_plan
cols_to_dat <- function(data, tfrmt, mock){

  label <- tfrmt$label %>% as_label
  groups <- tfrmt$group %>% map_chr(as_label)
  groups_lowest <- groups %>% last()
  columns <- tfrmt$column %>% map_chr(as_label)
  columns_lowest <- columns %>% last() %>% sym()
  value <- tfrmt$value %>% as_label

  tfrmt$big_n <- NULL

  if (! value %in% names(data)) {
    data <- data %>% mutate(!!value := "xx")
  }

  data_wide <- getFromNamespace("pivot_wider_tfrmt", "tfrmt")(data, tfrmt, mock)
  col_plan_vars <- getFromNamespace("create_col_order", "tfrmt")(names(data_wide), cp = tfrmt$col_plan, columns = tfrmt$column)

  allcols <- col_plan_vars %>% map_chr(as_label)
  allcols <- getFromNamespace("split_data_names_to_df","tfrmt")(data_names= c(),
                                                                preselected_cols = allcols,
                                                                column_names = columns)

  num_fix_ord <- c(groups, label) %>% length()
  allcols %>%
    mutate(`__col_plan_fixed__` = .data[[columns_lowest]] %in% label, #c(groups_lowest, label),
           `__col_plan_fixed_ord__` = .data[[columns_lowest]] %in% c(groups, label),
           `__col_plan_fixed_ord__` = ifelse(.data$`__col_plan_fixed_ord__`, rev(seq_len(num_fix_ord)), 0)) %>%
    rename(`__col_plan_dropped__` = "subtraction_status") %>%
    mutate(across(.data[[paste0("__tfrmt_new_name__", columns_lowest)]], function(x)str_remove(x, '^-')))
}

