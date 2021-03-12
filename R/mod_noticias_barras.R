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
mod_noticias_barras_server <- function(input, output, session, entidad){
  ns <- session$ns
  
  
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
    barras_candidatos(bd_2(), c("candidato 1", "candidato 2", "candidato 3"),
                      col="tipoEvento", "Tipos de enventos")
  })
  
  output$percepcionMedios <- renderHighchart({
    
    barras_candidatos(bd_2(), c("candidato 1", "candidato 2", "candidato 3"),
                      col="percepcion", "Percepción en Medios")
  })
  
  output$mencionGenerada <- renderHighchart({
    barras_candidatos(bd_2(), c("candidato 1", "candidato 2", "candidato 3"),
                      col="mencionGenerada", "Menciones del candidato generadas")
  })
  
  output$mencionNoGenerada <- renderHighchart({
    barras_candidatos(bd_2(), c("candidato 1", "candidato 2", "candidato 3"),
                      col="mencionNoGenerada", "Menciones del candidato no generadas")
  })
}
    
## To be copied in the UI
# mod_noticias_barras_ui("noticias_barras_ui_1")
    
## To be copied in the server
# callModule(mod_noticias_barras_server, "noticias_barras_ui_1")
 
