#' encuestas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr highcharter ggplot2 tibble
mod_encuestas_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             plotOutput(ns("iVoto"))
      )
    ),
    fluidRow(
      column(width = 6,
             plotOutput(ns("votacion"))
      ),
      column(width = 6,
             plotOutput(ns("ranking"))
      )
    )
  )
}

#' encuestas Server Function
#'
#' @noRd 
mod_encuestas_server <- function(input, output, session, bd){
  ns <- session$ns
  
  output$iVoto <- renderPlot({
    ggplot(bd()) + geom_line(aes(x = a,y = b))
  })
  
  output$votacion <- renderPlot({
    ggplot(bd()) + geom_bar(aes(x = a,y = b), stat = "identity")
  })
  
  output$ranking <- renderPlot({
    ggplot(bd()) + geom_point(aes(x = a,y = b))
    
  })
  
}

## To be copied in the UI
# mod_encuestas_ui("encuestas_ui_1")

## To be copied in the server
# callModule(mod_encuestas_server, "encuestas_ui_1")

