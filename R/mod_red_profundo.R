#' red_profundo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_red_profundo_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, 
             selectInput(ns("candidato"), label = "Selecciones candidato", choices = c("Candidato 1", "Candidato 2", "Candidato 3"), selected = "Candidato 1")
      ) 
      
    ),
    fluidRow(
      column(width = 6, 
             tags$h4(HTML( "Interacciones con otras cuentas")),
             class="shadowBox",
             shinycssloaders::withSpinner(
               plotOutput(ns("mencion"))
             )),
      column(width = 6, 
             tags$h4(HTML( "Interacciones con hashtags")),
             class="shadowBox",
             shinycssloaders::withSpinner(
               plotOutput(ns("hashtag"))
             ))
    )
 
  )
}
    
#' red_profundo Server Function
#'
#' @noRd 
mod_red_profundo_server <- function(input, output, session, entidad){
  ns <- session$ns
  nube2 <- reactive({
    candidatos <- filter(candidatos, entidad==!!entidad())
    candidatos <- filter(candidatos, candidato==!!input$candidato)
    words <- select(candidatos, text)
    procesando_nube(words)
  })
  output$mencion <- renderPlot({
    # load("~/Documents/Git/tblEmpresrial/data/candidatos.rda")
    # candidatos <- filter(candidatos, entidad==!!entidad())
    # candidatos <- filter(candidatos, candidato==!!input$candidato)
    # words <- select(candidatos, text)
    # Nube <- procesando_nube(words)
    red_menciones <- procesando_red_menciones(nube2())
    graficando_red(red_menciones)
  })
  
  output$hashtag <- renderPlot({
    # load("~/Documents/Git/tblEmpresrial/data/candidatos.rda")
    # candidatos <- filter(candidatos, entidad==!!input$entidad)
    # candidatos <- filter(candidatos, candidato==!!input$candidato)
    # words <- select(candidatos, text)
    # Nube <- procesando_nube(words)
    red_hashtag <- procesando_red_hashtag(nube2())
    graficando_red(red_hashtag)
  })
 
}
    
## To be copied in the UI
# mod_red_profundo_ui("red_profundo_ui_1")
    
## To be copied in the server
# callModule(mod_red_profundo_server, "red_profundo_ui_1")
 
