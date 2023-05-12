test_that("no files provided - mock mode", {

  testServer(load_server,
             args = list(mockmode = reactive(TRUE)), {

               # user inputs
               session$setInputs(tfrmt_source = "None")
               session$setInputs(data_source = "Auto")

               template_tfrmt <- tfrmt(group = "group",
                                       label = "label",
                                       param = "param",
                                       value = "value",
                                       column = "column",
                                       body_plan = body_plan(
                                         frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
                                       ))

               # returns template tfrmt
               expect_equal(
                 tfrmt_out(),
                 template_tfrmt,
                 ignore_attr = TRUE
               )

               # returns NULL data
               expect_equal(
                 data_out(),
                 NULL,
                 ignore_attr = TRUE
               )

               # correct mode
               expect_equal(
                 mode(),
                 "mock_no_data"
               )

               # now change to example data
              session$setInputs(data_source="Example")
              session$setInputs(example_data="demog")

              expect_equal(
                data_out(),
                tfrmt::data_demog
              )
              expect_equal(
                mode(),
                "mock_with_data"
              )
             })
})


test_that("Upload tfrmt", {

  json_example_path <- "https://raw.githubusercontent.com/statasaurus/Phuse2023-ARDs-to-tables/main/data/ard.json"
  ard_json <- tfrmt::json_to_tfrmt(path = json_example_path)

  testServer(load_server,
             args = list(mockmode = reactive(TRUE)), {

               # user inputs
               session$setInputs(tfrmt_source = "Upload")
               session$setInputs(tfrmt_load = list(datapath = json_example_path))

               expect_equal(
                 tfrmt_out(),
                 ard_json
               )

             }
  )

})

test_that("Upload data", {

  ard_example_path <- "https://raw.githubusercontent.com/statasaurus/Phuse2023-ARDs-to-tables/main/data/ard.csv"
  ard <- read.csv(ard_example_path)

  testServer(load_server,
             args = list(mockmode = reactive(TRUE)), {

               # user inputs
               session$setInputs(data_source = "Upload")
               session$setInputs(data_load = list(datapath = ard_example_path))

               expect_equal(
                 data_out(),
                 ard
               )
             }
  )
})
