#' noticias_general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_noticias_general_ui <- function(id){
  ns <- NS(id)
  tagList(
    navbarPage(title = "Noticias",
               tabPanel("Temas y calificadores",
                        mod_noticias_ui(ns("noticias_ui_1"))
               ),
               tabPanel("Eventos, percepción y menciones",
                        mod_noticias_barras_ui(ns("noticias_barras_ui_1"))
               )
    )
 
  )
}

#' noticias_general Server Function
#'
#' @noRd 
mod_noticias_general_server <- function(input, output, session, entidad){
  ns <- session$ns
  
  callModule(mod_noticias_server, "noticias_ui_1", entidad=entidad)
  callModule(mod_noticias_barras_server, "noticias_barras_ui_1", entidad=entidad)
 
}
    
## To be copied in the UI
# mod_noticias_general_ui("noticias_general_ui_1")
    
## To be copied in the server
# callModule(mod_noticias_general_server, "noticias_general_ui_1")
 
