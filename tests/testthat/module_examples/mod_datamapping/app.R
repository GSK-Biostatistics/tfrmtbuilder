pkgload::load_all(here::here())

tfrmt_orig <- tfrmt(
  group = c("rowlbl1", "rowlbl2"),
  label = "grp",
  value = "value",
  param = "param",
  column = "column",
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x"))
  )
)
data <- tfrmt::data_demog
mode <- "mock_with_data"

shinyApp(
  fluidPage(
    includeCSS(system.file("www","styles.css", package = "tfrmtbuilder")),
    html_dependency_pretty(),
    useShinyjs(),
    useShinyFeedback(),
    datamapping_ui("mappings")
  ),
  function(input,output,session){

    vals <- datamapping_server("mappings",
                               data = reactive(data),
                               tfrmt_orig = reactive(tfrmt_orig),
                               mode = reactive(mode))


     exportTestValues(vals = {vals()})

  }
)
