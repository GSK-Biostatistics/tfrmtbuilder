test_that("no files provided - mock mode", {

  skip_on_cran()

  testServer(load_server,
             args = list(mockmode = reactive(TRUE)), {

               # user inputs
               session$setInputs(tfrmt_source = "None")
               session$setInputs(data_source = "Auto")

               template_tfrmt <- prep_tfrmt_app(tfrmt())

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
              session$setInputs(data_ex="demog")

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

  skip_on_cran()

  # save a json
  tfrmt_n_pct() %>% tfrmt_to_json("test.json")

  json_example_path <- "test.json"
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

  # remove json
  file.remove(json_example_path)

})

test_that("Upload data", {

  skip_on_cran()

  # save a csv
  tfrmt::data_demog %>% filter(rowlbl1 %in% c("Age (y)","Sex")) %>%
    write.csv("ard_demog.csv", row.names = FALSE)

  ard_example_path <- "ard_demog.csv"
  ard <- read.csv(ard_example_path, stringsAsFactors = FALSE)

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

  # remove ard
  file.remove(ard_example_path)

})
