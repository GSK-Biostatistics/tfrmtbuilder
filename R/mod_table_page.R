
table_page_ui <- function(id){

  ns <- NS(id)

  tagList(
    span(
      style = "display: flex; gap: 5px;",
      htmlOutput(ns("page_txt")),
      actionButton(ns("prev_tbl"), "Previous", icon = icon("backward-step"), class = "btn-page"),
      actionButton(ns("next_tbl"), "Next", icon = icon("forward-step"), class = "btn-page")
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
        tot <- length(tab_gt())
        page_tot(tot)
      } else {
        page_tot(1)
      }
      page_cur(1)
    })

    # update txt
    output$page_txt <- renderUI({

      p(paste0("Displaying page ", page_cur(), " of ", page_tot()),
        style="font-size: 90%; margin-top:4px; margin-bottom: 4px; text-align:center;")

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

    return(page_cur)


  })
}
