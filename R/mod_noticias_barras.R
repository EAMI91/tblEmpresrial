#' noticias_barras UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_noticias_barras_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 6, class="shadowBox",
           shinycssloaders::withSpinner(highchartOutput(ns("tipoEventos")))
    ), 
    column(width = 6, class="shadowBox",
           shinycssloaders::withSpinner(highchartOutput(ns("percepcionMedios")))
    ),
    column(width = 6, class="shadowBox",
           shinycssloaders::withSpinner(highchartOutput(ns("mencionGenerada")))
    ),
    column(width = 6, class="shadowBox",
           shinycssloaders::withSpinner(highchartOutput(ns("mencionNoGenerada")))
    )
 
  )
  )
}
    
#' noticias_barras Server Function
#'
#' @noRd 
mod_noticias_barras_server <- function(input, output, session, df2){
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

  
 # %>% 
 #    left_join(bd.tema() %>% select(contains("Tema_1")), by = "idTema_1") %>% 
 #    left_join(bd.tema() %>% select(contains("Tema_2")), by = "idTema_2") %>% 
 #    left_join(bd.tema() %>% select(contains("Tema_3")), by = "idTema_3") 
  
  
  
  
  
  
  
  
  
  bd_2 <- reactive({
    n <- 1000
    BD <- tibble(
      id = 1:n,
      tipoEvento = sample(c("Político", "Electoral", "Acto de\nCampaña"), n, replace = T),
      candidato = sample(c("candidato 1", "candidato 2", "candidato 3", "candidato 4"), n, replace = T),
      percepcion = sample(c("Buena", "Mala", "Regular"), n, replace = T),
      mencionGenerada = sample(c("Boletines\n de prensa", "declaraciones", "filtraciones"), n, replace = T),
      mencionNoGenerada = sample(c("personaje", "columnista", "adversario", "partidario"), n, replace = T),
      calif_generada = sample(c("Mala", "Buena", "Regular"), n, replace = T),
      calif_no_generada = sample(c("Buena", "Mala", "Regular"), n, replace = T), 
      entidad = sample(c("Michoacán", "Nuevo León"), 
                       n, replace = T, 
                       prob = c(.5, .5))
    )
    BD <- filter(BD, entidad==!!entidad())
  })
  
  output$tipoEventos <- renderHighchart({
    barra<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato")
    
      barras_candidatos(barra, col="evento", title="Tipo de evento")
    

  })
  
  output$percepcionMedios <- renderHighchart({
    barra2<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato")
    
    barras_candidatos(barra2, col="calificacion", title="Percepción en medios")
  })
  
  output$mencionGenerada <- renderHighchart({
    barra3<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato") %>% 
      filter(nota!="Nota no generada")
    barras_candidatos(barra3, col="tipoMedio", "Menciones del candidato generadas")
  })
  
  output$mencionNoGenerada <- renderHighchart({
    
    barra4<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato") %>% 
      filter(nota=="Nota no generada")
    
    barras_candidatos(barra4, col="tipoMedio",  "Menciones del candidato no generadas")
  })
}
    
## To be copied in the UI
# mod_noticias_barras_ui("noticias_barras_ui_1")
    
## To be copied in the server
# callModule(mod_noticias_barras_server, "noticias_barras_ui_1")
 
