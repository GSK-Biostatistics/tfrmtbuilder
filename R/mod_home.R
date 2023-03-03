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
             <h2 class=heading_style> Welcome to the {tfrmtbuilder} App </h2>

             <p>
             Lorem ipsum dolor sit amet. Qui excepturi reprehenderit ut inventore
             laudantium ut architecto veniam. Eos eligendi eaque ut minima odio At
             magni beatae est minima molestias aut consequatur molestiae id sequi deleniti.
             </p>

             <h4 class=heading_style><i> About </i></h4>

             <p>
             Lorem ipsum dolor sit amet. Qui excepturi reprehenderit ut inventore
             laudantium ut architecto veniam. Eos eligendi eaque ut minima odio At
             magni beatae est minima molestias aut consequatur molestiae id sequi deleniti.
             </p>

             <h4 class=heading_style><i> Info </i></h4>

             <p>
             Lorem ipsum dolor sit amet. Qui excepturi reprehenderit ut inventore
             laudantium ut architecto veniam. Eos eligendi eaque ut minima odio At
             magni beatae est minima molestias aut consequatur molestiae id sequi deleniti.
             </p>

             <h4 class=heading_style><i> Useful Links </i></h4>

             <ul>
             <li> <a target=_blank class=home_links href=https://github.com/GSK-Biostatistics/tfrmtbuilder>
             {tfrmtbuilder} GitHub Repository</a> </li>
             <li> <a target=_blank class=home_links href=https://github.com/GSK-Biostatistics/tfrmt>
             {tfrmt} GitHub Repository</a> </li>
             <li> <a target=_blank class=home_links href=https://gsk-biostatistics.github.io/tfrmt/>
             {tfrmt} User Guide </a> </li>
             </ul>

             ")
      })

      output$hex <- renderUI({
        HTML(" <center> <img src=https://github.com/GSK-Biostatistics/tfrmt/blob/main/man/figures/tfrmt.png?raw=true
                alt=hex width=100% > </center> ")

      })

    }
  )}
