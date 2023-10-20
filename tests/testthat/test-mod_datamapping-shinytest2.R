
# note: cannot test the "mock_no_data" mode due to bug (selectize + shinytest2)
# https://github.com/rstudio/shinytest2/issues/232

test_that("Mock w/ data mode",{

  skip_if_not(interactive())

  app_dir <- rprojroot::find_testthat_root_file("module_examples/mod_datamapping")
  app <-  shinytest2::AppDriver$new(app_dir)


  # No changes from user ----------------------------------------------------
  values <- app$get_values()$export$vals
  ex_tfrmt <- tfrmt(
    group = c("rowlbl1", "rowlbl2"),
    label = "grp",
    value = "value",
    param = "param",
    column = "column",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )

  expect_equal(values$tfrmt, ex_tfrmt, ignore_attr = TRUE)
  expect_equal(values$data, tfrmt::data_demog %>% select(-value))
  expect_equal(values$mode, "mock_with_data")
  expect_equal(values$original, TRUE)


# Change selections -------------------------------------------------------

  app$set_inputs(`mappings-groups-item-1` = "rowlbl2")
  app$set_inputs(`mappings-groups-item-2` = "rowlbl1")

  # no change prior to save button
  values <- app$get_values()$export$vals
  expect_equal(values$tfrmt, ex_tfrmt, ignore_attr = TRUE)

  # press save
  app$click("mappings-save")
  values <- app$get_values()$export$vals

  ex_tfrmt <- tfrmt(
    group = c("rowlbl2", "rowlbl1"),
    label = "grp",
    value = "value",
    param = "param",
    column = "column",
    body_plan = body_plan(
      frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
    )
  )

  expect_equal(values$tfrmt, ex_tfrmt, ignore_attr = TRUE)
  expect_equal(values$data, tfrmt::data_demog %>% select(-value))
  expect_equal(values$mode, "mock_with_data")
  expect_equal(values$original, FALSE)


# Add selections (sorting cols) -------------------------------------------

  app$click("mappings-sorting_cols-addinput")
  app$click("mappings-sorting_cols-addinput")

  input_ids <- app$get_values()$input %>% names() %>% .[str_detect(., "^mappings-sorting_cols-item")]
  input_ids <- setdiff(input_ids, "mappings-sorting_cols-item-0")

  new_val <- setNames("ord1", input_ids[1])
  app$set_inputs(!!input_ids[1] := "ord1")

  new_val <- setNames("ord2", input_ids[2])
  app$set_inputs(!!input_ids[2] := "ord2")

  expect_equal(app$get_value(input=!!input_ids[1]), "ord1")
  expect_equal(app$get_value(input=!!input_ids[2]), "ord2")

  # no change prior to save button
  values <- app$get_values()$export$vals
  expect_equal(values$tfrmt, ex_tfrmt, ignore_attr = TRUE)

  # press save
  app$click("mappings-save")
  values <- app$get_values()$export$vals

  expect_equal(values$tfrmt, ex_tfrmt %>% layer_tfrmt(tfrmt(sorting_cols = c("ord1","ord2"))), ignore_attr = TRUE)
  expect_equal(values$data, tfrmt::data_demog %>% select(-value))
  expect_equal(values$mode, "mock_with_data")
  expect_equal(values$original, FALSE)

  app$stop()

})
