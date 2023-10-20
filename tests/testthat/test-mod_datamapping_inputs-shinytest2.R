
test_that("Data-driven dropdowns produce expected results",{

  skip_if_not(interactive())

  app_dir <- rprojroot::find_testthat_root_file("module_examples/mod_datamapping_inputs")
  app <-  shinytest2::AppDriver$new(app_dir)

  # No changes from user ----------------------------------------------------

  expect_equal(app$get_value(input="mapping-item-1"), "grp")
  export_vals <- app$get_values()$export

  expect_equal(export_vals$settings, "grp")
  expect_equal(export_vals$valid, TRUE)
  expect_equal(export_vals$initial_state, TRUE)


# Change selection --------------------------------------------------------

  app$set_inputs(`mapping-item-1` = "rowlbl1", wait_ = FALSE)

  expect_equal(app$get_value(input="mapping-item-1"), "rowlbl1")
  export_vals <- app$get_values()$export

  expect_equal(export_vals$settings, "rowlbl1")
  expect_equal(export_vals$valid, TRUE)
  expect_equal(export_vals$initial_state, FALSE)


# Deselect ----------------------------------------------------------------

  app$set_inputs(`mapping-item-1` = NULL, wait_ = FALSE)

  expect_equal(app$get_value(input="mapping-item-1"), NULL)
  export_vals <- app$get_values()$export

  expect_equal(export_vals$settings, NULL)
  expect_equal(export_vals$valid, FALSE)
  expect_equal(export_vals$initial_state, FALSE)

  app$stop()

})



test_that("Data-driven dropdowns can be added/removed",{

  skip_if_not(interactive())

  app_dir <- rprojroot::find_testthat_root_file("module_examples/mod_datamapping_inputs")
  app <-  shinytest2::AppDriver$new(app_dir)

  # get input IDs
  input_ids <- app$get_values()$input %>% names() %>% .[str_detect(., "^mapping-item")]


# Add a new input ---------------------------------------------------------

  app$click("mapping-addinput")
  input_ids_new <- app$get_values()$input %>% names() %>% .[str_detect(., "^mapping-item")]
  input_id_new <- setdiff(input_ids_new, input_ids)
  input_ids <- input_ids_new

  new_val <- setNames("rowlbl2", input_id_new)
  app$set_inputs(!!input_id_new := "rowlbl2")

  expect_equal(app$get_value(input=!!input_id_new), "rowlbl2")
  export_vals <- app$get_values()$export

  expect_equal(export_vals$settings, c("grp","rowlbl2"))
  expect_equal(export_vals$valid, TRUE)
  expect_equal(export_vals$initial_state, FALSE)

  # Add another input -------------------------------------------------------

  app$click("mapping-addinput")
  input_ids_new <- app$get_values()$input %>% names() %>% .[str_detect(., "^mapping-item")]
  input_id_new <- setdiff(input_ids_new, input_ids)
  input_ids <- input_ids_new

  new_val <- setNames("rowlbl1", input_id_new)
  app$set_inputs(!!input_id_new := "rowlbl1")

  expect_equal(app$get_value(input=!!input_id_new), "rowlbl1")
  export_vals <- app$get_values()$export

  expect_equal(export_vals$settings, c("grp","rowlbl2", "rowlbl1"))
  expect_equal(export_vals$valid, TRUE)
  expect_equal(export_vals$initial_state, FALSE)


# Remove input ------------------------------------------------------------

  app$click("mapping-dropinput")
  export_vals <- app$get_values()$export
  expect_equal(export_vals$settings, c("grp", "rowlbl2"))

  # again
  app$click("mapping-dropinput")
  export_vals <- app$get_values()$export
  expect_equal(export_vals$settings, "grp")

  # make sure original number of groups cannot be removed
  app$click("mapping-dropinput")
  export_vals <- app$get_values()$export
  expect_equal(export_vals$settings, "grp")


# Add input again after the removal ---------------------------------------

  app$click("mapping-addinput")
  input_ids_new <- app$get_values()$input %>% names() %>% .[str_detect(., "^mapping-item")]
  input_id_new <- setdiff(input_ids_new, input_ids)
  input_ids <- input_ids_new

  expect_equal(app$get_value(input=!!input_id_new), NULL)

  new_val <- setNames("rowlbl1", input_id_new)
  app$set_inputs(!!input_id_new := "rowlbl1")

  expect_equal(app$get_value(input=!!input_id_new), "rowlbl1")
  export_vals <- app$get_values()$export

  expect_equal(export_vals$settings, c("grp","rowlbl1"))
  expect_equal(export_vals$valid, TRUE)
  expect_equal(export_vals$initial_state, FALSE)

  app$stop()

})

