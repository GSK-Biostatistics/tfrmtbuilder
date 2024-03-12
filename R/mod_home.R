# Module for home landing page

home_ui <- function(id){

  ns <- NS(id)

  fluidRow(
    column(8, uiOutput(outputId = ns("about")) ),
    column(4,
           fluidRow(uiOutput(outputId = ns("hex")) ),
           fluidRow(uiOutput(outputId = ns("links")))
    )
    )
  }

home_server <- function(id){

  moduleServer(
    id,
    function(input, output, session) {

      addResourcePath("www", system.file('www',
                                         package = 'tfrmtbuilder'))

      output$about <- renderUI({
        HTML("
             <h2 class=heading_style> Welcome to the {tfrmtbuilder} Shiny App </h2>

             <p>
             The {tfrmt} package provides a language for defining display-related
             metadata. This metadata can then be used to automate and easily update output formats.
             {tfrmtbuilder} serves as point-and-click interface to the package, allowing users
             to quickly and easily modify existing or new table templates.
             </p>

             <h4 class=heading_style><i> Features and Workflow </i></h4>

             <p>
             <p> At the top of the app, use the toggle to specify whether you are generating a mock (no data values) or a table with data values. Then proceed through the tabs as follows:
             </p>
             <ol>
             <li> Initialize tab
             <ul>
             <li> Load an existing tfrmt (JSON) or start fresh </li>
             <li> Define your data source:
             <ul>
             <li> Auto [mock only]: Use auto-generated mock data</li>
             <li> Upload: Upload an existing dataset (most file formats)
             <ul>
             <li> <i> Note: If in 'mock' mode, this will serve as a shell with the numeric data value column ignored </i> </li>
             </ul>
             <li> Example [mock only]: Use a pre-loaded example dataset </li>
             </ul>
             </ul>
             <li> Edit tab
             <ul>
             <li> Select ARD column mappings </li>
             <li> Format table via the 'plans' and other functionality </li>
             <li> View the table and underlying data as you modify </li>
             </ul>
             </li>
             <li> Export tab
             <ul>
             <li> Download {tfrmt} metadata (JSON) </li>
             <li> Download {tfrmt} table (HTML, PNG) </li>
             </ul>
             </li>
             </ol>
             </p>

             ")
      })

      output$hex <- renderUI({
        HTML(" <left> <img src= 'www/tfrmtbuilder_hex.png'
                alt=hex width=75% > </left> ")

      })

      output$links <- renderUI({

        HTML("<h4 class=heading_style><i> Useful Links </i></h4>

             <ul>
             <li> <a target=_blank class=home_links href=https://gsk-biostatistics.github.io/tfrmt/>
             {tfrmt} User Guide </a> </li>
             <li> <a target=_blank class=home_links href=https://github.com/GSK-Biostatistics/tfrmtbuilder>
             {tfrmtbuilder} GitHub Repository</a> </li>
             <li> <a target=_blank class=home_links href=https://github.com/GSK-Biostatistics/tfrmt>
             {tfrmt} GitHub Repository</a> </li>
             </ul>
        ")
      })

    }
  )}
