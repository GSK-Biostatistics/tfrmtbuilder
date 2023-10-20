pkgload::load_all(here::here())

setting_name <- "groups"
data <- tfrmt::data_demog
settings_in <- "grp"
reset <- 1
multiple <- TRUE

shinyApp(
  fluidPage(
    includeCSS(system.file("www","styles.css", package = "tfrmtbuilder")),
    html_dependency_pretty(),
    useShinyjs(),
    useShinyFeedback(),
    datamapping_inputs_ui("mapping", setting_name = setting_name)
  ),
  function(input,output,session){

    vals <- datamapping_inputs_server("mapping",
                               data = reactive(data),
                               settings_in = reactive(settings_in),
                               reset = reactive(reset),
                               multiple = multiple)

    exportTestValues(
      settings = {vals$settings()},
      valid = {vals$valid()},
      initial_state = {vals$initial_stat()}
    )

  }
)
