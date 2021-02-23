#' encuestas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr highcharter ggplot2 tibble ggchicklet
mod_encuestas_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12,
             highchartOutput(ns("iVoto"))
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
  
  output$iVoto <- renderHighchart({
    
    hoy<- tibble(x= max(as.Date(puntos$fechaEncuesta)), y= 50) %>% mutate(tt = "Última<br> encuesta")
    eleccion<- tibble(x= as.Date("2021-06-06"), y= 50) %>% mutate(tt = "Día de la<br> elección")
    bd_pop %>% hPollofPolls(puntos = puntos_pop, hoy = hoy, eleccion = eleccion)
  })
  
  output$votacion <- renderPlot({
    tibble(partido = c("MORENA", "PAN", "PRI"),
           n = c(43, 28, 27)) %>% 
      intVotoBarras()
  })
  
  output$ranking <- renderPlot({
    tibble( y = c("1", "2", "3"),
            candidato = c("MORENA", "PAN", "PRI"),
            dia = rep(77, 3),
            color = c("#751438", "#17418A", "#EB0E0E"),
            p = c(.43, .28, .27),
            fecha = rep(as.Date("2021-06-06"), 3),
            prob = c(43, 28, 27),
            label = c("43%", "28%", "27%"),
            rw = c(1, 2, 3)) %>% 
      probGanarOld(candidato = "MORENA", 5)
    
  })
  
}

## To be copied in the UI
# mod_encuestas_ui("encuestas_ui_1")

## To be copied in the server
# callModule(mod_encuestas_server, "encuestas_ui_1")

