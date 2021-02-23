#' noticias UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_noticias_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' noticias Server Function
#'
#' @noRd 
mod_noticias_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_noticias_ui("noticias_ui_1")
    
## To be copied in the server
# callModule(mod_noticias_server, "noticias_ui_1")
 
