#' análisis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_análisis_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(ns("analisis"),"Análisis disponibles",choices = c("Cargando..."="")),
    fluidRow(
      slickROutput(ns("pdf"),width='90%',height = 100)
    )
  )
}

#' análisis Server Function
#'
#' @noRd 
mod_análisis_server <- function(input, output, session, tipo, entidad){
  ns <- session$ns
 
  observe({
    fechas <- imgs %>% filter(entidad == !! entidad(), tipo == !!tipo) %>% arrange(desc(fecha)) %>% pull(fecha)
    nombres <-  fechas %>% format("%d %B %y")
    updateSelectInput(session = session,inputId = "analisis",choices = fechas %>% set_names(nombres))
  })
  
  out <- reactive({
    req(input$analisis)
    imgs %>% filter(entidad == !! entidad(), tipo == !!tipo, fecha == !!input$analisis) %>% pull(archivo)
  })
  
  output$pdf <- renderSlickR({
    slickR(
      out(),height = 700,width = "100%",
    )
  })  
}

## To be copied in the UI
# mod_análisis_ui("análisis_ui_1")

## To be copied in the server
# callModule(mod_análisis_server, "análisis_ui_1")

