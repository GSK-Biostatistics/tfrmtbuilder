# Module to load data (and tfrmt in future)

# returns list of reactives: data and tfrmt

load_ui <- function(id){

  ns <- NS(id)

  tagList(
    fluidRow(
      column(4,
             wellPanel(
               div(style = "height: 650px",
                   h3("tfrmt", class = "heading_style"),
                   fluidRow(
                     column(4, radioGroupButtons(ns("tfrmt_source"), label = "Source", choices = c("none", "custom"))),
                     column(6, conditionalPanel( "input.tfrmt_source=='custom'",
                                                 fileInput(ns("tfrmt_load"), "Load json file", accept = c(".json")),
                                                 ns = ns)
                     )
                   ),
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
                   fluidRow(
                     column(2, radioGroupButtons(ns("mode"), "Select mode", choices = c("mock", "reporting"), selected = "mock")),
                     column(3, conditionalPanel("input.mode=='mock'",
                                                radioGroupButtons(ns("data_source"), label = "Source", choices = c("none", "example", "custom")),
                                                ns = ns)),
                     column(3, conditionalPanel("input.mode=='reporting' || input.data_source=='custom'",
                                                fileInput(ns("data_load"), "Load data file", accept = c(".csv",".sas7bdat",".rds")),
                                                ns = ns),
                            conditionalPanel("input.data_source=='example' && input.mode=='mock'",
                                             radioGroupButtons(ns("example_data"), label = "Dataset", choices = c("demog","ae","labs","efficacy")),
                                             ns = ns))
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

load_server <- function(id){

    moduleServer(
      id,
      function(input, output, session) {


        # uploaded data (if applicable)
        loaded_data <- eventReactive(input$data_load,{
          rio::import(input$data_load$datapath)
        })

        # uploaded tfrmt (if applicable)
        loaded_tfrmt <- eventReactive(input$tfrmt_load,{
          tfrmt:::json_to_tfrmt(path = input$tfrmt_load$datapath)
        })

        # selected example data
        example_data <- eventReactive(input$example_data,{

          str_to_eval <- paste0("tfrmt::data_", input$example_data)

          if (input$example_data=="labs") {
            str_to_eval <- paste0(str_to_eval, " %>% filter(group2 %in% unique(group2)[1:3])")
          }
          eval(parse(text = str_to_eval))

        })


        # tfrmt to be used in the app
        tfrmt_out <- reactive({

          if (input$tfrmt_source=="none"){
            prep_tfrmt_app(tfrmt())
          } else {
            req(loaded_tfrmt())
            loaded_tfrmt()
          }

        })

        # keep track of mode for downstream functionality
        mode <- reactive({
          if (input$mode == "mock"){
            if (input$data_source=="none"){
              "mock_no_data"
            } else {
              "mock_with_data"
            }
          } else if (input$mode == "reporting"){
            "reporting"
          }
        })

        # data to be used in the app
        data_out <- reactive({

          if (input$mode=="mock" && input$data_source=="none"){
            NULL
          } else if (input$mode=="mock" && input$data_source=="example"){
            example_data()
          } else {
            loaded_data()
          }
        })

        # data preview
        output$data_view <- renderDT({

          if (is.null(data_out())){
            data_tbl <- tfrmt:::make_mock_data(tfrmt_out())
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
