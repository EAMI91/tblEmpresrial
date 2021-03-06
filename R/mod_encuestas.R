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
mod_encuestas_server <- function(input, output, session, bd, entidad){
  ns <- session$ns
  
  bd_tempo <- bd_pop
  
  bd_tempo$entidad <- "Michoacán"
  
  bd_tempo2 <- bd_pop
  bd_tempo2$entidad <- "Nuevo León"
  
  bd_tempo2 <- mutate(bd_tempo2, 
                      candidato=if_else(candidato=="MORENA", "PRI",
                      if_else(candidato=="PRI", "MORENA", candidato)),
                      color=if_else(color=="#751438", "#EB0E0E",
                                    if_else(color=="#EB0E0E", "#751438", 
                                            color)))
  bd_pop_final <-  reactive({

    
    bd_pop_final <- bind_rows(bd_tempo, bd_tempo2) %>% 
      filter(entidad== !!entidad())
    
 })
  
  puntos_tempo <- puntos
  puntos_tempo$entidad <- "Michoacán"
  puntos_tempo2 <- puntos
  puntos_tempo2$entidad <- "Nuevo León"
  
  puntos_tempo2 <- mutate(puntos_tempo2, 
                          partido=if_else(partido=="MORENA", "PRI",
                          if_else(partido=="PRI", "MORENA", partido)),
                          candidato=if_else(candidato=="AMLO", "MEADE",
                          if_else(candidato=="MEADE", "AMLO",candidato)))  
  puntos_final <- reactive({
    
    puntos_final <- bind_rows(puntos_tempo, puntos_tempo2) %>% 
      filter(entidad== !!entidad())
    
  })
  
  
  
  output$iVoto <- renderHighchart({
    
    hoy<- tibble(x= max(as.Date(puntos$fechaEncuesta)), y= 50) %>%
      mutate(tt = "Última<br> encuesta")
    eleccion<- tibble(x= as.Date("2021-06-06"), y= 50) %>% 
      mutate(tt = "Día de la<br> elección")
    bd_pop_final() %>% hPollofPolls(puntos = puntos_final(), 
                                    hoy = hoy, eleccion = eleccion)
  })
  
  
  base1 <- tibble(partido = c("MORENA", "PAN", "PRI"),
                  n = c(.43, .28, .27), 
                  entidad="Michoacán") %>% mutate(n2 = paste0(n*100, "%")) 
  base2 <- tibble(partido = c("MORENA", "PAN", "PRI"),
                  n = c(.27, .28, .43), 
                  entidad="Nuevo León") %>% mutate(n2 = paste0(n*100, "%")) 
 bd_ambas <- bind_rows(base1, base2) 
  
 bd_barras <-  reactive({
   bd_ambas <- filter(bd_ambas, entidad==!!entidad())
 })
 
 
  output$votacion <- renderPlot({
    
      intVotoBarras(bd_barras())
  })
  
  juntas <-  reactive({
    
  tabla1 <- tibble( y = c("1", "2", "3"),
                    candidato = c("MORENA", "PAN", "PRI"),
                    dia = rep(77, 3),
                    color = c("#751438", "#17418A", "#EB0E0E"),
                    p = c(.43, .28, .27),
                    fecha = rep(as.Date("2021-06-06"), 3),
                    prob = c(43, 28, 27),
                    label = c("43%", "28%", "27%"),
                    rw = c(1, 2, 3), 
                    entidad= "Michoacán")
  tabla2 <- tibble( y = c("1", "2", "3"),
                    candidato = c("PRI", "PAN", "MORENA"),
                    dia = rep(77, 3),
                    color = c("#EB0E0E", "#17418A", "#751438"),
                    p = c(.43, .28, .27),
                    fecha = rep(as.Date("2021-06-06"), 3),
                    prob = c(43, 28, 27),
                    label = c("43%", "28%", "27%"),
                    rw = c(1, 2, 3), 
                    entidad= "Nuevo León")
  
  juntas <- bind_rows(tabla1, tabla2) %>% 
    filter(entidad== !!entidad())
  })
  output$ranking <- renderPlot({
      juntas()  %>% 
      probGanarOld(candidato = "MORENA", 5)
    
  })
  
}

## To be copied in the UI
# mod_encuestas_ui("encuestas_ui_1")

## To be copied in the server
# callModule(mod_encuestas_server, "encuestas_ui_1")

