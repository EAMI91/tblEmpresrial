#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny purrr slickR rtweet
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  notificaciones <- reactive({
    tibble::tribble(~estado, ~texto,~icono,~status,~fecha,
                    "Nuevo León", "Encuesta nueva, Nuevo León", "poll-h","success", today(),#ymd("2021-03-02"),
                    "Michoacán", "Análisis cualitativo electoral, Michoacán", "file-alt","danger", today() #ymd("2021-03-02")
    ) %>% filter(fecha == today(tzone = "America/Mexico_City"))
  })
  
  
  output$notificaciones <- renderMenu({
    dropdownMenu(
      type = "notifications", badgeStatus = "success",
      headerText = 
        if(nrow(notificaciones()) > 1) glue::glue("Tiene {nrow(notificaciones())} nuevas actualizaciones el día de hoy") else if(nrow(notificaciones()) == 1)"Tiene 1 nueva actualización el día de hoy" else "No tiene actualizaciones el día de hoy",
      
      .list = notificaciones() %>% pmap(function(texto, icono, status, ...){
        notificationItem(icon = icon(icono), status = status, 
                         texto)
      })
      
    )
    
  })
  
  entidad <- reactive({
    input$entidad
  })
  # Módulo de candidatos
  observe({
    callModule(mod_candidatos_server, "candidatos_ui_1", entidad = entidad)  
  })
  
  
  # Módulo de encuestas
  callModule(mod_encuestas_server, "encuestas_ui_1")  
  
  # Módulo de redes sociales
  callModule(mod_redes_sociales_server, "redes_sociales_ui_1", entidad = entidad)
  
  #Módulo de noticias
  callModule(mod_noticias_server, "noticias_ui_1")
  
  #Análisis
  
  callModule(mod_análisis_server, "análisis_ui_1",tipo = "encuestas", entidad = entidad)
  callModule(mod_análisis_server, "análisis_ui_2",tipo = "redes", entidad = entidad)
  callModule(mod_análisis_server, "análisis_ui_3",tipo = "noticias", entidad = entidad)
  callModule(mod_análisis_server, "análisis_ui_4",tipo = "electoral", entidad = entidad)
}
