
table_page_ui <- function(id){

  ns <- NS(id)

  tagList(
    h3(
      span(
      actionButton(ns("prev_tbl"), "Previous", icon = icon("backward-step"), class = "btn-page"),
      actionButton(ns("next_tbl"), "Next", icon = icon("forward-step"), class = "btn-page")
    )
    )
  )

}

#' @param id module ID
#' @param tab_gt rendered tfrmt as gt/gt_group
#'
#' @noRd
table_page_server <- function(id, tab_gt){

  moduleServer(
    id,
    function(input, output, session){

      ns <- session$ns

    # determine length
    page_tot <- reactiveVal(1)
    page_cur <- reactiveVal(1)

    # reset indices when table updates
    observeEvent(tab_gt(), {
      if (inherits(tab_gt(), "gt_group")){
        tot <- nrow(tab_gt()$gt_tbls)
        page_tot(tot)
      } else {
        page_tot(1)
      }
      page_cur(1)
    })

    # toggle buttons
    observe({
      shinyjs::toggleState("prev_tbl", condition = !page_cur()==1)
      shinyjs::toggleState("next_tbl", condition = !page_cur()==page_tot())
    })

    # indices respond to buttons
    observeEvent(input$prev_tbl, {
      new_idx <- page_cur()-1
      if (new_idx>0){
        page_cur(new_idx)
      }
    })
    observeEvent(input$next_tbl, {
      new_idx <- page_cur()+1
      if (new_idx<=page_tot()){
        page_cur(new_idx)
      }
    })

    return(
      list(page_cur = page_cur,
           page_tot = page_tot)
    )


  })
}
