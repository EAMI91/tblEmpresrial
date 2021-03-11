#' redes_sociales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr highcharter ggplot2 tibble lubridate
mod_redes_sociales_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
   column(width = 12, 
             selectInput(ns("candidato"), label = "Selecciones candidato", choices = c("Candidato 1", "Candidato 2", "Candidato 3"), selected = "Candidato 1")
      ) 

    ),
    fluidRow(
      column(width = 12,class="shadowBox",
             highchartOutput(ns("reach"))
      )
    ), 

    fluidRow(
      column(width = 6, 
             tags$h4(HTML( "Comparativo")),
             class="shadowBox",
             highchartOutput(ns("saldo"))
      ), 
      column(width = 6, 
             tags$h4(HTML( "Contenido de comentarios")),
             class="shadowBox",
             shinycssloaders::withSpinner(
               plotOutput(ns("nube"))
             ))    
 
  ), 
  fluidRow(
    column(width = 6, 
           tags$h4(HTML( "Tweet con más interacciones")),
           shinycssloaders::withSpinner(uiOutput(ns("masFavs")), 
                                        proxy.height = "200px")
    ), 
    column(width = 6, 
           tags$h4(HTML( "Tweet con mayor alcance")),
           shinycssloaders::withSpinner(uiOutput(ns("masmencion")), 
                                        proxy.height = "200px")
    )    
    
  )
  
  )
}
    
#' redes_sociales Server Function
#'
#' @noRd 
mod_redes_sociales_server <- function(input, output, session, entidad){
  ns <- session$ns
  
  
  output$reach <- renderHighchart({
    tempo1 <- tibble( grupo = sample(c("Tweet", "Menciones", "RT", "Likes"),
                                        prob = c(.2,.2 ,.3, .3), size = 100, replace = T),
                         fecha = sample(seq(today()-100, today(), length.out = 11), size = 100, replace = T ), 
                         entidad = sample(c("Michoacán", "Nuevo León"), 
                                          size = 100, replace = T, 
                                          prob = c(.5, .5)),
                         candidato = "Candidato 1")
    tempo2 <- tibble( grupo = sample(c("Tweet", "Menciones", "RT", "Likes"),
                                     prob = c(.2,.2 ,.3, .3), size = 100, replace = T),
                      fecha = sample(seq(today()-100, today(), length.out = 11), size = 100, replace = T ),
                      entidad = sample(c("Michoacán", "Nuevo León"), 
                                       size = 100, replace = T, 
                                       prob = c(.5, .5)),
                      candidato = "Candidato 2")
    
    tempo3 <- tibble( grupo = sample(c("Tweet", "Menciones", "RT", "Likes"),
                                     prob = c(.2,.2 ,.3, .3), size = 100, replace = T),
                      fecha = sample(seq(today()-100, today(), length.out = 11), size = 100, replace = T ), 
                      entidad = sample(c("Michoacán", "Nuevo León"), 
                                       size = 100, replace = T, 
                                       prob = c(.5, .5)),
                      candidato = "Candidato 3")
      proyectos <- bind_rows(tempo1, tempo2)
      
      proyectos <- bind_rows(proyectos, tempo3) %>% 
      filter(entidad==!!entidad()) %>% 
      filter(candidato==!!input$candidato) %>% 
      arrange(fecha) %>% 
      group_by(fecha, grupo) %>%
      summarise(n=n()) 
    
    reach(proyectos)
  })
  
  output$saldo <- renderHighchart({
    tempo1 <- tibble(votos = sample(30:78, size = 172, replace = T),
                     voto = sample(c("Negativo", "Positivo"), size = 172, replace = T, prob = c(.5, .5)), 
                     entidad = sample(c("Michoacán", "Nuevo León"), 
                                      size = 172, replace = T, 
                                      prob = c(.5, .5)),
                     candidato = "Candidato 1") %>%
      mutate(mes = cut(votos,c(17,29,39,49,59,69,79, 89,100),
                       labels = c("Semana 1", "Semana 2", "Semana 3", "Semana 4", "Semana 5", "Semana 6", "Semana 7", "Semana 8"))) 
    
    tempo2 <- tibble(votos = sample(30:78, size = 172, replace = T),
                     voto = sample(c("Negativo", "Positivo"), size = 172, replace = T, prob = c(.5, .5)), 
                     entidad = sample(c("Michoacán", "Nuevo León"), 
                                      size = 172, replace = T, 
                                      prob = c(.5, .5)),
                     candidato = "Candidato 2") %>%
      mutate(mes = cut(votos,c(17,29,39,49,59,69,79, 89,100),
                       labels = c("Semana 1", "Semana 2", "Semana 3", "Semana 4", "Semana 5", "Semana 6", "Semana 7", "Semana 8"))) 
    
    
    tempo3 <- tibble(votos = sample(30:78, size = 172, replace = T),
                     voto = sample(c("Negativo", "Positivo"), size = 172, replace = T, prob = c(.5, .5)), 
                     entidad = sample(c("Michoacán", "Nuevo León"), 
                                      size = 172, replace = T, 
                                      prob = c(.5, .5)),
                     candidato = "Candidato 3") %>%
      mutate(mes = cut(votos,c(17,29,39,49,59,69,79, 89,100),
                       labels = c("Semana 1", "Semana 2", "Semana 3", "Semana 4", "Semana 5", "Semana 6", "Semana 7", "Semana 8"))) 
    
    df <- bind_rows(tempo1, tempo2)
    df <- bind_rows(df, tempo3)%>%
      filter(entidad==!!entidad()) %>% 
      filter(candidato==!!input$candidato) %>% 
      count(mes, voto) %>%
      mutate(n = as.double(n),
             n2= case_when(voto == "Negativo"~ n*-1,
                           voto == "Positivo"~ n))
    
    graficando_saldo(df)
  })
  
  nube <- reactive({
    candidatos <- filter(candidatos, entidad==!!entidad())
    candidatos <- filter(candidatos, candidato==!!input$candidato)
    words <- select(candidatos, text)
    procesando_nube(words)
  })
  
  output$nube <- renderPlot({
    graficando_nube(nube())
    })
  

  
  output$masFavs <- renderUI({
    tagList(
      paratuit %>% 
        filter(entidad==!!entidad()) %>% 
        filter(candidato==!!input$candidato) %>% 
        select(TW_Entities,TW_StatusID) %>% 
        blockquote(TW_Entities = .$TW_Entities,TW_StatusID = .$TW_StatusID) %>% 
        HTML(),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
    
  })
  
  output$masmencion <- renderUI({
    tagList(
      menciones_1 %>% 
        filter(entidad==!!entidad()) %>% 
        filter(candidato==!!input$candidato) %>% 
        select(TW_Entities,TW_StatusID) %>% 
        blockquote(TW_Entities = .$TW_Entities,TW_StatusID = .$TW_StatusID) %>% 
        HTML(),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
    
  })
  
}
 
 
    
## To be copied in the UI
# mod_redes_sociales_ui("redes_sociales_ui_1")
    
## To be copied in the server
# callModule(mod_redes_sociales_server, "redes_sociales_ui_1")
 
