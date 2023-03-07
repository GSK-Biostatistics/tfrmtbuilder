# Module for home landing page

home_ui <- function(id){

  ns <- NS(id)

  fluidRow(
    column(8, uiOutput(outputId = ns("about")) ),
    column(4, uiOutput(outputId = ns("hex")) )
    )
  }

home_server <- function(id){

  moduleServer(
    id,
    function(input, output, session) {
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
             <ul>
             <li> Initialize tab
             <ul>
             <li> Load an existing tfrmt (JSON) or start fresh </li>
             <li> Load an existing dataset (most file formats) or use generated mock data </li>
             </ul>
             <li> Edit tab
             <ul>
             <li> Select ARD column mappings </li>
             <li> Format table via the 'plans' </li>
             <li> View the table as you modify </li>
             </ul>
             </li>
             <li> Export tab
             <ul>
             <li> Download tfrmt metadata (JSON) </li>
             <li> Download tfrmt table (HTML, PNG) </li>
             </ul>
             </li>
             </ul>
             </p>

             <h4 class=heading_style><i> Useful Links </i></h4>

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

      output$hex <- renderUI({
        HTML(" <center> <img src=https://github.com/GSK-Biostatistics/tfrmt/blob/main/man/figures/tfrmt.png?raw=true
                alt=hex width=75% > </center> ")

      })

    }
  )}
