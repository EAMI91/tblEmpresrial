#' noticias UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import highcharter dplyr tibble quanteda plotly tidyr ggfittext ggthemes

mod_noticias_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, class="shadowBox",
        shinycssloaders::withSpinner(highchartOutput(ns("timeNoticias")))
            ),
      column(width =  6, class="shadowBox",
             shinycssloaders::withSpinner(plotOutput(ns("nubePalabras")))
        ),
      column(width =  6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("termometro")))
        ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("temasEleccion")))
      ),
      
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("califGenerada1")))
      )
    )
  )
}
    
#' noticias Server Function
#'
#' @noRd 
mod_noticias_server <- function(input, output, session, df2){
  ns <- session$ns
  

  
  
  bd.cand <- reactiveVal(NULL)
  bd.tema <- reactiveVal(NULL)
  
  observeEvent(df2$opciones, 
               {   
                bd.cand(
                df2$opciones %>% select(idCategoria, variable, categoria) %>%
                pivot_wider(names_from = variable, values_from = categoria) %>% 
                mutate(Candidato = idCandidato, idCandidato = idCategoria) %>%
                select(idCandidato, Candidato) %>% 
                filter(!is.na(Candidato))
                 )
                 
                 bd.tema(
                   df2$opciones %>% select(idCategoria, variable, categoria) %>%
                     pivot_wider(names_from = variable, values_from = categoria) %>% 
                     mutate(Tema_1 = idTema, idTema_1 = idCategoria,
                            Tema_2 = Tema_1, idTema_2 = idTema_1,
                            Tema_3 = Tema_2, idTema_3 = idTema_2) %>%
                     select(idTema_1, Tema_1,idTema_2, Tema_2, idTema_3, Tema_3) %>% 
                     filter(!is.na(Tema_1))
                 )})
  
  output$timeNoticias <- renderHighchart({
    paleta <- c("Negativa"="#710627", "Neutral"="#CF8C40", "Positiva"="#BBC200")
    noticias <- df2$noticias %>% 
      
      mutate(fecha=floor_date(fecha,unit = "day"), 
             fecha=as.Date(fecha)) %>% 
      filter(!is.na(calificacion)) %>% 
      count(fecha, calificacion)
    
    timeline_noticias(noticias)
  })
  
  output$nubePalabras <- renderPlot({
    noticias_nube <- df2$noticias %>% 
      filter(!is.na(calificacion))
    Nube <- procesando_nube_not(noticias_nube)
    graficando_nube_not(Nube)
  })
  
  output$termometro <- renderHighchart({
    paratermo <- df2$noticias %>% 
             mutate(nivel=nrow(.)*1)
    paratermo <- paratermo$nivel
    termo(paratermo)
  })
 
  output$temasEleccion <- renderHighchart({
    
    temas1 <- bd.tema() %>% 
      select(idTema_1, Tema_1)
    
    
    prueba <- df2$noticias%>% 
      left_join(temas1, by = "idTema_1") %>% 
      mutate(
        Tema_1_otro = ifelse(Tema_1 == "Otros", Tema_1, NA)
      )
    
    
    temas_eleccion(prueba,
                   pregunta = Tema_1,
                   otro = Tema_1_otro,
                   x = 0,
                   titulo = "Temas de la elección general")
  })
  

  output$califGenerada1 <- renderHighchart({
    treemap_calificacion_bis(df2$noticias)
  })
  
  # output$califGenerada2 <- renderHighchart({
  #   treemap_calificacion_bis(bd_2(), candida="candidato 2")
  # })
  # output$califGenerada3 <- renderHighchart({
  #   treemap_calificacion_bis(bd_2(), candida="candidato 3")
  # })
  # 
  # output$califGenerada4 <- renderHighchart({
  #   treemap_calificacion_bis(bd_2(), candida="candidato 4")
  # })
}
 
## To be copied in the UI
# mod_noticias_ui("noticias_ui_1")
    
## To be copied in the server
# callModule(mod_noticias_server, "noticias_ui_1")
 
