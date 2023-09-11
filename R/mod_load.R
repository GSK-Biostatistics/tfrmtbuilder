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
                   fluidRow(
                     column(4, radioGroupButtons(ns("tfrmt_source"), label = NULL, choices = c("None", "Upload"))),
                     column(8, conditionalPanel( "input.tfrmt_source=='Upload'",
                                                 fileInput(ns("tfrmt_load"), buttonLabel = "Load JSON", label = NULL, accept = c(".json")),
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
                     column(3, radioGroupButtons(ns("data_source"), label = NULL,
                                                                  choices = c("Auto", "Upload", "Example"), selected = "Auto")),
                     column(3, conditionalPanel("input.data_source=='Upload'",
                                                fileInput(ns("data_load"), buttonLabel = "Load Data", label = NULL, accept = c(".csv",".sas7bdat",".rds")),
                                                ns = ns),
                            conditionalPanel("input.data_source=='Example'",
                                             radioGroupButtons(ns("example_data"), label = NULL, choices = c("demog","ae","labs","efficacy")),
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

load_server <- function(id, mockmode){

    moduleServer(
      id,
      function(input, output, session) {

        ns <- session$ns

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

          if (input$tfrmt_source=="None"){
            prep_tfrmt_app(tfrmt())
          } else {
            req(loaded_tfrmt())
            loaded_tfrmt()
          }

        })

        # keep track of mode for downstream functionality
        mode <- reactive({
          if (mockmode() == TRUE){
            if (input$data_source=="Auto"){
              "mock_no_data"
            } else {
              "mock_with_data"
            }
          } else if (mockmode() == FALSE){
            "reporting"
          }
        })

        # data to be used in the app
        data_out <- reactive({

          if (mockmode()==TRUE && input$data_source=="Auto"){
            NULL
          } else if (input$data_source=="Example"){
            example_data()
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
