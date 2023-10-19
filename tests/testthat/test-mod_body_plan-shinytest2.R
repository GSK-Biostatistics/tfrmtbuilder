

test_that("Body plan as expected with no user changes",{

  skip_if_not(interactive())

  app_dir <- rprojroot::find_testthat_root_file("module_examples/mod_body_plan")
  app <-  shinytest2::AppDriver$new(app_dir)

  # No changes from user ----------------------------------------------------
  body_plan_out <- app$get_values()$export$vals

  expect_equal(body_plan_out,
               body_plan(
                 frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x")),
                 frmt_structure(group_val = ".default", label_val = "n", frmt("xx"))
                 ))


  # Switch order of the frmt_structures (rows in sortable) ------------------
  app$set_inputs(
    `bp-item_list` = c("2", "1"),
    allow_no_input_binding_ = TRUE
  )
  body_plan_actual <- app$get_values()$export$vals
  body_plan_expected <- body_plan(
    frmt_structure(group_val = ".default", label_val = "n", frmt("xx")),
    frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
  )
  expect_equal(body_plan_actual, body_plan_expected)

  # the app will re-generate the table after
  app$set_inputs(
    `bp-item_list` = c("1", "2"),
    allow_no_input_binding_ = TRUE
  )
  body_plan_actual <- app$get_values()$export$vals

  expect_equal(body_plan_actual, body_plan_expected)

  app$stop()



})

test_that("body plan add/delete rows",{

  skip_if_not(interactive())

  app_dir <- rprojroot::find_testthat_root_file("module_examples/mod_body_plan")
  app <-  shinytest2::AppDriver$new(app_dir)

# Add row -----------------------------------------------------------------

  # click add button
  app$click("bp-add")
  # Update output value
  app$wait_for_idle()
  app$set_inputs(`bp-customize_pane-filters-values-rowlbl1` = "Baseline BMI",
                 allow_no_input_binding_ = TRUE)

  app$click("bp-save")
  body_plan_actual <- app$get_values()$export$vals
  body_plan_expected <- body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x")),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xx")),
    frmt_structure(group_val = list(rowlbl1 = "Baseline BMI", grp = ".default"), label_val = ".default", frmt("xx.x"))
  )

  expect_equal(body_plan_actual, body_plan_expected)

  app$stop()


  # delete row -----------------------------------------------------------

  app <-  shinytest2::AppDriver$new(app_dir)

  # select a row
  app$click(selector = "#bp-item-2")

   # click delete button
  app$click("bp-delete")

  body_plan_actual <- app$get_values()$export$vals
  body_plan_expected <- body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
  )

  expect_equal(body_plan_actual, body_plan_expected, ignore_attr = TRUE)

  app$stop()

})
