test_that("End to end with mock/auto mode (no data)", {

  skip_if_not(interactive())

  app_dir <- rprojroot::find_testthat_root_file("e2e")
  app <- shinytest2::AppDriver$new(app_dir)

  # click export tab
  app$set_inputs(`tb-all_tabs` = "Export")

  app$get_download("tb-export-json_save", filename = "tfrmt_save.json")
  expect_snapshot_file("tfrmt_save.json")

  app$get_download("tb-export-tbl_save_png", filename = "tfrmt_save.png")
  expect_true(file.exists("tfrmt_save.png"))

  file.remove("tfrmt_save.json")
  file.remove("tfrmt_save.png")

  app$stop()
})

