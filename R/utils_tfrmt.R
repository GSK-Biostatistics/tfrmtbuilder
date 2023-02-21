
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

  col_string <- map_chr(x$cols, as_label) %>%
    paste0("\"", ., "\"") %>%
    paste(., collapse = ", ")

  align <- x$align  %>%
    paste0("\"", ., "\"") %>%
    paste(., collapse = ", ")
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


# template frmt objects
dummy_frmt <- function(){
  "frmt(\"xx.x\", missing = NULL, scientific = NULL)"
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

#  convert a frmt to string (for going from R obj -> editable textbox)

frmt_to_string <- function(x, ...){
  UseMethod("frmt_to_string")
}

#' @exportS3Method frmt_to_string frmt
frmt_to_string.frmt <- function(frmt_obj, param_val = NULL){

  if(!is.null(param_val)) param <- paste0(param_val, " = ") else param <- NULL
  frmt_txt <- frmt_obj$expression
  if (!is.null(frmt_obj$missing )) missing <- paste0(", missing = \"", frmt_obj$missing, "\"") else missing <- NULL
  if (!is.null(frmt_obj$scientific )) scientific <- paste0(", scientific = \"", frmt_obj$scientific, "\"") else scientific <- NULL

  paste0(
    param, "frmt(\"", frmt_txt, "\"", missing, scientific, ")"
  )
}

#' @exportS3Method frmt_to_string frmt_when

frmt_to_string.frmt_when <- function(frmt_obj, param_val = NULL){

  if(!is.null(param_val)) param <- paste0(param_val, " = ") else param <- NULL
  if (!is.null(frmt_obj$missing )) missing <- paste0(", missing = \"", frmt_obj$missing, "\"") else missing <- NULL

  frmt_txt <- map_chr(frmt_obj$frmt_ls, function(x){
    lhs <- paste0("\"", x[[2]], "\"")
    rhs <- x[[3]]
    if (is.list(rhs)){
      rhs <- rhs %>% frmt_to_string
    } else {
      rhs <- paste0("\"", rhs, "\"")
    }
    paste0(lhs, " ~ ", rhs)
  }) %>%
    paste0(collapse = ", ")

  paste0(
    param, "frmt_when(", frmt_txt, missing, ")"
  )
}

#' @exportS3Method frmt_to_string frmt_combine

frmt_to_string.frmt_combine <- function(frmt_obj, ...){

  if (!is.null(frmt_obj$missing )) missing <- paste0(", missing = \"", frmt_obj$missing, "\"") else missing <- NULL
  frmt_glue <- paste0("\"", frmt_obj$expression, "\"")

  frmt_txt <- imap_chr(frmt_obj$frmt_ls, frmt_to_string.frmt) %>%
    paste0(collapse = ", ")

  paste0(
    "frmt_combine(", frmt_glue, ", ", frmt_txt, missing, ")"
  )
}

# convert string to frmt obj (for going from text -> R obj)
string_to_tfrmtobj <- function(obj, class){

  tryCatch({
    obj_eval <- eval(parse(text = obj))
    is_expected_class <- match.fun(paste0("is_", class))
    if (is_expected_class(obj_eval)) obj_eval else NULL
  },
  error = function(e){
    NULL
  })

}

# helper to get values for the col plan
cols_to_dat <- function(data, tfrmt){

  cols <- tfrmt$column %>% map_chr(as_label)

  # establish order of all slots
  cols_dat <- data %>%
    ungroup() %>%
    select(any_of(cols)) %>%
    unique %>%
    mutate(across(everything(), function(x){
      ifelse(is.na(x), paste0("__span_empty_", cumsum(is.na(x))), x)
    })) %>%
    mutate(across(everything(), ~fct_inorder(.x))) %>%
    arrange(across(everything()))

  # bind sorting cols
  if (!is.null(tfrmt$sorting_cols)){
    col_last <- last(cols)
    ord_cols <- map_chr(tfrmt$sorting_cols, as_label)
    cols_dat <- bind_rows(cols_dat, tibble(!!col_last:= ord_cols))
  }

  cols_dat
}
