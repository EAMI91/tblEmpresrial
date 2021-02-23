#' redes_sociales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_redes_sociales_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' redes_sociales Server Function
#'
#' @noRd 
mod_redes_sociales_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_redes_sociales_ui("redes_sociales_ui_1")
    
## To be copied in the server
# callModule(mod_redes_sociales_server, "redes_sociales_ui_1")
 
