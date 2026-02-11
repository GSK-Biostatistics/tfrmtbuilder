
# module for table download button for given output type

mod_export_table_ui <- function(id, ext){

  ns <- NS(id)

  tagList(
    div(downloadButton(ns("tbl_save"), label = toupper(ext), icon = icon("download")))
  )
}

mod_export_table_server <- function(id, tbl, ext){

  moduleServer(
    id,
    function(input, output, session) {


      output$tbl_save <- downloadHandler(
        filename = function() {
          if (inherits(tbl(), "gt_group") && tolower(ext) %in% c("png", "pdf")){
            paste0('tfrmt_', tolower(ext), '.zip')
          } else {
            paste0('tfrmt.', tolower(ext))
          }
        },
        content = function(con) {

          if (inherits(tbl(), "gt_group") && tolower(ext) %in% c("png", "pdf")){
            temp_dir <- tempdir()
            dir.create(temp_dir)

            n_tbls <- nrow(tbl()$gt_tbls)
            purrr::walk(1:n_tbls, function(x){
              tbl() %>% gt::grp_pull(x) %>% gt::gtsave(filename = paste0("tfrmt_",x,".", tolower(ext)), path = temp_dir)
            })

            utils::zip(
              zipfile = con,
              files = file.path(temp_dir, paste0("tfrmt_", seq_along(tbl()), ".", tolower(ext))),
              flags = "-r9Xj"
            )

          } else{

            gtobj <- tbl()
            gt::gtsave(gtobj, con)

          }
        }
      )



    }
  )
}
