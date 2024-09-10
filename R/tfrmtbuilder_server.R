#' Main server function
#' @noRd
tfrmtbuilder_server <- function(id, tfrmt, data) {

  moduleServer(
    id,
    function(input, output, session) {

      # ui for home
      home_out <- home_server("home")

      # ui for loading
      settings_orig <- load_server("load", reactive(tfrmt), reactive(data), reactive(input$mockmode))

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
      # page plan creation
      pp_out <- page_plan_server("page_plan", reactive(settings()$data), reactive(settings()$tfrmt), settings_orig$mode)
      # titles
      ti_out <- titles_server("titles", reactive(settings()$tfrmt))


      tfrmt_app_out <- reactive({
        req(settings())
        req(bp_out())
        req(rg_out())
        req(fn_out())
        req(cs_out())
        req(cp_out())
        req(bn_out())
        req(pp_out())
        req(ti_out())

        tfrmt_app <-  settings()$tfrmt
        tfrmt_app$title <- ti_out()$title
        tfrmt_app$subtitle <- ti_out()$subtitle
        tfrmt_app$body_plan <- bp_out()
        tfrmt_app$row_grp_plan <- rg_out()
        tfrmt_app$col_style_plan <- cs_out()
        tfrmt_app$col_plan <- cp_out()
        tfrmt_app$page_plan <- pp_out()

        if (length(fn_out()$struct_list)>0){
          tfrmt_app$footnote_plan <- fn_out()
        } else {
          tfrmt_app$footnote_plan <- NULL
        }

        if (!is_empty(bn_out())){
          tfrmt_app$big_n <- bn_out()
        }

        tfrmt_app

      })


      # data to display
      data_out <- reactive({
        if (!settings()$mode=="mock_no_data"){
          # original data loaded in
          settings_orig$data()
        } else {
          req(tfrmt_app_out())
          # regenerate mock data from new tfrmt
          make_mock_data(tfrmt_app_out())
        }
      })


      # table viewer module
      table_outer_server("tbl_view",
                         cur_tab = reactive(input$all_tabs=="Edit"),
                         subtab = reactive(input$tabs),
                         data = reactive(settings()$data) ,
                         tfrmt_app_out = tfrmt_app_out,
                         mode = reactive(settings()$mode)
                         )

      # export module
      export_server("export",
                    data =  reactive(settings()$data) ,
                    tfrmt_app_out = tfrmt_app_out,
                    mode = reactive(settings()$mode),
                    cur_tab = reactive(input$all_tabs=="Export"))

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
