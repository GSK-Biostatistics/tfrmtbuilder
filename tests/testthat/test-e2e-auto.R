#shinytest2::record_test()

test_that("End to end with mock/auto mode (no data)", {

  skip_on_cran()

  app <- shinytest2::AppDriver$new()

  # click export tab
  app$set_inputs(`tb-all_tabs` = "Export")

  # return json tfrmt
  app$expect_download("tb-export-json_save", name = "tfrmt_out.json")
  app$expect_download("tb-export-tbl_save_png", name = "tfrmt_out.png")
})

