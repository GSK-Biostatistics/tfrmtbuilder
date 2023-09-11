#' Main server function
#' @noRd
tfrmtbuilder_server <- function(id) {

  moduleServer(
    id,
    function(input, output, session) {

      # ui for home
      home_out <- home_server("home")

      # ui for loading
      settings_orig <- load_server("load", reactive(input$mockmode))

      # if user adjust the inputs, direct them to Data Mapping tab (in Edit tab)
      observe({
        settings_orig$data()
        settings_orig$tfrmt()
        settings_orig$mode()

        updateTabsetPanel(
          session = session,
          "tabs",
          selected = "Data Mapping - TEST"
        )

      })

      # tfrmt data mapping - returns an updated tfrmt/data to be fed into the other modules
     settings <- datamapping_server("overview", settings_orig$data, settings_orig$tfrmt, settings_orig$mode)

      # body plan creation
      bp_out <- body_plan_server("body_plan", reactive(settings()$data), reactive(settings()$tfrmt), settings_orig$mode)
      # row group plan creation
      rg_out <- row_grp_plan_server("row_grp_plan", reactive(settings()$data), reactive(settings()$tfrmt), settings_orig$mode)
      # footnote plan creation
      fn_out <- footnote_plan_server("footnote_plan", reactive(settings()$data), reactive(settings()$tfrmt))
      # col style plan creation
      cs_out <- col_style_plan_server("col_style_plan", reactive(settings()$data), reactive(settings()$tfrmt))
      # col plan creation
      cp_out <- col_plan_simple_server("col_plan",  reactive(settings()$data), reactive(settings()$tfrmt), settings_orig$mode)
      # big N creation
      bn_out <- big_n_server("big_n", reactive(settings()$data), reactive(settings()$tfrmt), settings_orig$mode)
      # titles
      ti_out <- titles_server("titles", reactive(settings()$tfrmt))

      # final tfrmt to combine results of all modules
      tfrmt_app_out <- reactiveVal(NULL)

      observeEvent(settings_orig$data(), # when data changes, reset
                   tfrmt_app_out(NULL))

      # generate the updated tfrmt
      observe({
        req(settings())
        req(bp_out())
        req(rg_out())
        req(fn_out())
        req(cs_out())
        req(cp_out())
        req(bn_out())
        req(ti_out())

        tfrmt_app <-  settings()$tfrmt
        tfrmt_app$title <- ti_out()$title
        tfrmt_app$subtitle <- ti_out()$subtitle
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
        if (!settings()$mode=="mock_no_data"){
          # original data loaded in
          settings()$data
        } else {
          req(tfrmt_app_out())
          # regenerate mock data from new tfrmt
          make_mock_data(tfrmt_app_out())
        }
      })

      # table viewer module
      table_view_server("tbl_view",
                        tab_selected = reactive(input$tabs),
                        data = reactive(settings()$data) ,
                        tfrmt_app_out = tfrmt_app_out,
                        settings = settings)

      # export module
      export_server("export",
                    data =  reactive(settings()$data) ,
                    tfrmt_app_out = tfrmt_app_out,
                    mode = settings_orig$mode)

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
