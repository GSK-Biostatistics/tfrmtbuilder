
test_that("title and subtitle modified then reset to default (NULL)",{

  skip_on_cran()

  # incoming tfrmt
  tfrmt_obj <- tfrmt(
    group = "group",
    label = "label",
    param = "param",
    column = "column",
    value = "value",
    title = NULL,
    subtitle = NULL
  )

  testServer(titles_server,
             args = list(tfrmt_orig = reactive(tfrmt_obj)), {

               # user inputs
               session$setInputs(title = "My title")
               session$setInputs(subtitle = "My subtitle")

               # save & return
               session$setInputs(save = 1)
               expect_equal(title_list_out(),
                            list(title = "My title",
                                 subtitle = "My subtitle"))

               # reset & return
               session$setInputs(reset = 1)
               expect_equal(title_list_out(),
                            list(title = NULL,
                                 subtitle = NULL))
             })
})


test_that("title and subtitle modified then reset to default (values",{

  skip_on_cran()

  # incoming tfrmt
  tfrmt_obj <- tfrmt(
    group = "group",
    label = "label",
    param = "param",
    column = "column",
    value = "value",
    title = "Original title",
    subtitle = "Original subtitle"
  )

  testServer(titles_server,
             args = list(tfrmt_orig = reactive(tfrmt_obj)), {

               # user inputs
               session$setInputs(title = "My title")
               session$setInputs(subtitle = "My subtitle")

               # save & return
               session$setInputs(save = 1)
               expect_equal(title_list_out(),
                            list(title = "My title",
                                 subtitle = "My subtitle"))

               # reset & return
               session$setInputs(reset = 1)
               expect_equal(title_list_out(),
                            list(title = "Original title",
                                 subtitle = "Original subtitle"))
             })
})
