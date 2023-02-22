#' Main server function

tfrmtbuilder_server <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      # ui for loading
      loaded_list <- load_server("load")

      # collapse tfrmt view for column plan
      observe({
        shinyjs::toggle("sidebar", condition = !input$tabs == "Column Plan")
      })


      # tfrmt data mapping - returns an updated tfrmt to be fed into the other modules
      tfrmt_app <- datamapping_server("overview", loaded_list$data, loaded_list$tfrmt)

      # data to be passed through app - either the loaded data or baseline generated mock data
      data <- eventReactive(tfrmt_app(),{
        if (loaded_list$mode()=="mock_no_data"){
          tfrmt:::make_mock_data(tfrmt_app())
        } else {
          loaded_list$data()
        }
      })

      # body plan creation
      bp_out <- body_plan_server("body_plan", data, tfrmt_app, loaded_list$mode)
      # row group plan creation
      rg_out <- row_grp_plan_server("row_grp_plan", data, tfrmt_app, loaded_list$mode)
      # footnote plan creation
      fn_out <- footnote_plan_server("footnote_plan", data, tfrmt_app)
      # col style plan creation
      cs_out <- col_style_plan_server("col_style_plan", data, tfrmt_app)
      # col plan creation
      cp_out <- col_plan_server("col_plan", data, tfrmt_app, loaded_list$mode)
      # big N creation
      bn_out <- big_n_server("big_n", data, tfrmt_app, loaded_list$mode)

      # final tfrmt to combine results of all modules
      # TODO - store as reactiveValues so it can be fed back into the plans??
      tfrmt_app_out <- reactiveVal(NULL)

      observeEvent(loaded_list$data(), # when data changes, reset
                   tfrmt_app_out(NULL))

      # generate the updated tfrmt
      observe({
        req(tfrmt_app())
        req(bp_out())
        req(rg_out())
        req(fn_out())
        req(cs_out())
        req(cp_out())
        req(bn_out())

        tfrmt_app <-  tfrmt_app()
        tfrmt_app$body_plan <- bp_out()
        tfrmt_app$row_grp_plan <- rg_out()
        tfrmt_app$col_style_plan <- cs_out()
        tfrmt_app$col_plan <- cp_out()

        if (length(fn_out()$struct_list)>0){
          tfrmt_app$footnote_plan <- fn_out()
        } else {
          tfrmt_app$footnote_plan <- NULL
        }

        if (!is_empty(bn_out())){
          tfrmt_app$big_n <- bn_out()
        }

        tfrmt_app_out(tfrmt_app)
      })

      # data to display
      data_out <- reactive({
        if (!loaded_list$mode()=="mock_no_data"){
          # original data loaded in
          loaded_list$data()
        } else {
          req(tfrmt_app_out())
          # regenerate mock data from new tfrmt
          tfrmt:::make_mock_data(tfrmt_app_out())
        }
      })

      # table viewer module
      table_view_server("tbl_view",
                        tab_selected = reactive(input$tabs),
                        data = loaded_list$data,
                        tfrmt_app_out = tfrmt_app_out,
                        mode = loaded_list$mode)

      # export module
      export_server("export",
                    data = loaded_list$data,
                    tfrmt_app_out = tfrmt_app_out,
                    mode = loaded_list$mode)

      # view data
      output$data_view <- renderDT({

        datatable(data_out(),
                  rownames = FALSE,
                  fillContainer = TRUE,
                  options = list(paging = FALSE,
                                 scrollY = "500px",
                                 dom = "t")
        )

      })

    }
  )
}
