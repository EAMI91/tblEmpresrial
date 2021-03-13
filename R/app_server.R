#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  bd <- eventReactive(input$actualizar,{
    tibble(a = sample(1:5,5), b = sample(6:10,5))
  })
  
  
  entidad <- reactive({
    req(input$entidad)
    input$entidad
  })
  # Módulo de encuestas
  callModule(mod_encuestas_server, "encuestas_ui_1", bd = bd)  
  
  # Módulo de redes sociales
  callModule(mod_redes_general_server, "redes_general_ui_1", entidad=entidad)
  
  #Módulo de noticias
  callModule(mod_noticias_server, "noticias_ui_1")
  
  #Módulo de análisis electoral cualitativo
  callModule(mod_a_electoral_server, "a_electoral_ui_1")
}
