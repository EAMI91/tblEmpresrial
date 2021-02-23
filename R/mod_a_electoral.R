#' a_electoral UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_a_electoral_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' a_electoral Server Function
#'
#' @noRd 
mod_a_electoral_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_a_electoral_ui("a_electoral_ui_1")
    
## To be copied in the server
# callModule(mod_a_electoral_server, "a_electoral_ui_1")
 
