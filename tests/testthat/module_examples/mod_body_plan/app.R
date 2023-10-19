pkgload::load_all(here::here())

tfrmt_app <- tfrmt(
  group = c("rowlbl1", "grp"),
  label = "rowlbl2",
  value = "value",
  param = "param",
  column = "column",
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", frmt("xx.x")),
    frmt_structure(group_val = ".default", label_val = "n", frmt("xx"))
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
    body_plan_ui("bp")
  ),
  function(input,output,session){

    vals <- body_plan_server("bp",
                             data = reactive(data),
                             tfrmt_app = reactive(tfrmt_app),
                             mode_load = reactive(mode))

    exportTestValues(vals = {vals()})

  }
)
