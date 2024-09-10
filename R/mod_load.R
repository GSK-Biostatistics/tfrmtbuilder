# Module to load data (and tfrmt in future)

# returns list of reactives: data and tfrmt

load_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
             wellPanel(
               div(style = "height: 650px",
                   h3("Table Metadata", class = "heading_style"),
                   shinyjs::hidden(
                     div(
                     id = ns("tfrmt_opts"),
                     radioGroupButtons(ns("tfrmt_source"), label = NULL, choices = c("None", "Upload", "Example")),
                     fluidRow(
                       column(12,
                              shinyjs::hidden(fileInput(ns("tfrmt_load"), buttonLabel = "Load JSON", label = NULL, accept = c(".json")))
                       )
                     ),
                     fluidRow(
                       shinyjs::hidden(radioGroupButtons(ns("tfrmt_ex"),
                                                         label = NULL,
                                                         choices = c("demog","ae","efficacy")))
                     )
                   )),
                   fluidRow(
                     div(style = "height: 500px; overflow-y:auto; ",
                         shinycssloaders::withSpinner(
                           color = getOption("spinner.color", default = "#254988"),
                           type = 4,
                           verbatimTextOutput(ns("json"))
                         )
                     )
                   )
               )
             )
      ),
      column(8,
             wellPanel(
               div(style = "height: 650px",
                   h3("Data", class = "heading_style"),
                   shinyjs::hidden(
                     div(
                       id = ns("data_opts"),
                   radioGroupButtons(ns("data_source"), label = NULL,
                                     choices = c("Auto", "Upload", "Example"), selected = "Auto"),
                   fluidRow(
                     conditionalPanel("input.data_source=='Upload'",
                                      column(6, fileInput(ns("data_load"), buttonLabel = "Load Data", label = NULL, accept = c(".csv",".sas7bdat",".rds",".xpt"))),
                                      ns = ns)
                   ),
                   fluidRow(
                     conditionalPanel("input.data_source=='Example'",
                                      radioGroupButtons(ns("data_ex"), label = NULL, choices = c("demog","ae","labs","efficacy")),
                                      ns = ns)
                   )
                   )
                   ),
                   fluidRow(
                     div(style = "height: 550px;",
                         shinycssloaders::withSpinner(
                           color = getOption("spinner.color", default = "#254988"),
                           type = 4,
                           DTOutput(ns("data_view"), height = "500px")
                         )
                     )
                   )
               )
             )
      )
    )
  )
}

load_server <- function(id, tfrmt_in = reactive(NULL), data_in = reactive(NULL), mockmode){

    moduleServer(
      id,
      function(input, output, session) {

        ns <- session$ns

        observe({
          shinyjs::toggle("tfrmt_opts", condition = is.null(tfrmt_in()))
          shinyjs::toggle("data_opts", condition = is.null(data_in()))
        })
        observe({
          shinyjs::toggle("tfrmt_load", condition = input$tfrmt_source=="Upload")
          shinyjs::toggle("tfrmt_ex", condition = input$tfrmt_source=="Example")

        })

        # disable/enable selection
        observe({

          if (mockmode()){
            updateRadioGroupButtons(session, "data_source", disabledChoices = NULL)
          } else {
            cur_selected <- input$data_source
            selected <- ifelse(cur_selected=="Auto", "Upload", cur_selected)
            updateRadioGroupButtons(session, "data_source", disabledChoices = "Auto", selected = selected)
          }
        })

        # uploaded data (if applicable)
        loaded_data <- eventReactive(input$data_load,{
          rio::import(input$data_load$datapath)
        })

        # uploaded tfrmt (if applicable)
        loaded_tfrmt <- eventReactive(input$tfrmt_load,{
          json_to_tfrmt(path = input$tfrmt_load$datapath)
        })

        # selected example data (if applicable)
        data_ex <- eventReactive(input$data_ex,{

          str_to_eval <- paste0("tfrmt::data_", input$data_ex)

          if (input$data_ex=="labs") {
            str_to_eval <- paste0(str_to_eval, " %>% filter(group2 %in% unique(group2)[1:3])")
          }
          eval(parse(text = str_to_eval))

        })
        # selected example tfrmt (if applicable)
        tfrmt_ex <- eventReactive(input$tfrmt_ex,{

          tfrmt_file <- paste0("tfrmt_", input$tfrmt_ex, ".json")
          json_to_tfrmt(path = system.file("json_examples", tfrmt_file, package = "tfrmt"))

        })


        # tfrmt to be used in the app
        tfrmt_out <- reactive({
          if (!is.null(tfrmt_in())){
            tfrmt_in()
          } else if (input$tfrmt_source=="None"){
            prep_tfrmt_app(tfrmt())
          }  else if (input$tfrmt_source=="Example"){
            tfrmt_ex()
          } else {
            req(loaded_tfrmt())
            loaded_tfrmt()
          }

        })

        # If currently on data = "Auto" and example tfrmt is selected, use the example data instead
        observeEvent(c(input$tfrmt_ex, input$tfrmt_source), {
          req(input$tfrmt_source=="Example")
          req(!input$data_source=="Upload")
          updateRadioGroupButtons(session, "data_source", selected = "Example")
          updateRadioGroupButtons(session, "data_ex", selected = input$tfrmt_ex)
        })

        # keep track of mode for downstream functionality
        mode <- reactive({
          if (mockmode() == TRUE){
            if (!is.null(data_in()) || !input$data_source=="Auto"){
              "mock_with_data"
            } else {
              "mock_no_data"
            }
          } else if (mockmode() == FALSE){
            "reporting"
          }
        })

        # data to be used in the app
        data_out <- reactive({

          if (!is.null(data_in())){
            data_in()
          } else if (mockmode()==TRUE && input$data_source=="Auto"){
            NULL
          } else if (input$data_source=="Example"){
            data_ex()
          } else {
            loaded_data()
          }
        })

        # data preview
        output$data_view <- renderDT({

          if (is.null(data_out())){
            data_tbl <- make_mock_data(tfrmt_out())
          } else {
            data_tbl <- data_out()
          }
          datatable(data_tbl,
                    rownames = FALSE,
                    fillContainer = TRUE,
                    options = list(paging = FALSE,
                                   scrollY = "500px",
                                   dom = "t")
          )

        })

        # tfrmt preview (as json)
        output$json <- renderText({
          tfrmt_to_json(tfrmt_out())
        })

        return(
          list(
          data = data_out,
          tfrmt = tfrmt_out,
          mode = mode
        )
        )

      }
    )
}
