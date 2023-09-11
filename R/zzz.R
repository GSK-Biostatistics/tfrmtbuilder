#' @import shiny
#' @import tfrmt
#' @importFrom shinyjs useShinyjs enable disable addClass removeClass toggleState show hide toggle onclick toggleClass
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import purrr
#' @importFrom DT datatable DTOutput renderDT
#' @import gt
#' @import sortable
#' @importFrom rlang quo_is_missing
#' @import bslib
#' @import shinyWidgets
#' @import forcats
#' @importFrom rio import
#' @importFrom shinycssloaders withSpinner
#' @import webshot2
#' @import shinyAce
#' @importFrom stats setNames
#' @importFrom rlang `:=` `!!`
#' @importFrom shinyFeedback useShinyFeedback feedbackDanger
#' @importFrom fontawesome fa_i
NULL

globalVariables(".")

is_col_style_structure <- utils::getFromNamespace("is_col_style_structure","tfrmt")
split_data_names_to_df <- utils::getFromNamespace("split_data_names_to_df","tfrmt")
make_mock_data <- utils::getFromNamespace("make_mock_data","tfrmt")
apply_tfrmt <- utils::getFromNamespace("apply_tfrmt","tfrmt")




