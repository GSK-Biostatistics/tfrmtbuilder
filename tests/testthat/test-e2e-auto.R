test_that("End to end with mock/auto mode (no data)", {

  skip_if_not(interactive())

  tfrmtbuilder_app <- tfrmtbuilder(run = FALSE)
  app <- shinytest2::AppDriver$new(tfrmtbuilder_app)

  # click export tab
  app$set_inputs(`tb-all_tabs` = "Export")

  # return json tfrmt
  app$expect_download("tb-export-json_save")
  app$expect_download("tb-export-tbl_save_png")

  app$stop()
})

