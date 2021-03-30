#' redes_general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_redes_general_ui <- function(id){
  ns <- NS(id)
  tagList(
    navbarPage(title = "Redes sociales",
               tabPanel("Alcance y mediciones",
                        mod_redes_sociales_ui(ns("redes_sociales_ui_1"))
               )
  )
  )
}
    
#' redes_general Server Function
#'
#' @noRd 

mod_redes_general_server <- function(input, output, session,  df2, candidatos){
  ns <- session$ns
  
  callModule(mod_redes_sociales_server, "redes_sociales_ui_1", df2, candidatos)
 
}

## To be copied in the UI
# mod_redes_general_ui("redes_general_ui_1")
    
## To be copied in the server
# callModule(mod_redes_general_server, "redes_general_ui_1")
 
