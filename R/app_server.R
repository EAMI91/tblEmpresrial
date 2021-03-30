#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny purrr slickR rtweet RMariaDB
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(db = tibble(user ="admin",
                                                                    password = "1"))
  )
  
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
    req(input$entidad)
    input$entidad
  })
  
  # Módulo de candidatos
  callModule(mod_candidatos_server, "candidatos_ui_1", entidad = entidad)  
  bd <- eventReactive(input$actualizar,{
    tibble(a = sample(1:5,5), b = sample(6:10,5))
    
    
  })
  
  
  # entidad <- reactive({
  #   req(input$entidad)
  #   input$entidad
  # })
  
  df2 <- reactiveValues(entrenamiento = NULL, prueba = NULL, noticias=NULL, opciones=NULL)
  df3 <- reactiveValues(entidadesb=NULL)
  
  
  
  observe({ df3$entidadesb=tbl(pool, entidadesbd) %>% 
    collect()
  })
  
  
  observeEvent(df3$entidadesb, {
    entidades <- df3$entidadesb %>% pull(suscriptores) %>% 
      set_names(df3$entidadesb %>% pull(estado))
    updateSelectizeInput(session,
                         'entidad',
                         choices = entidades,
                         selected = entidades,
                         server = TRUE)
  })
  
  
  observeEvent(input$entidad, {
    
    sus <- df3$entidadesb %>% filter(suscriptores == input$entidad) %>% pull(estado)
    
    df2$entrenamiento <- tbl(pool, entrenamientobd) %>% 
      filter(estado %in% !!sus)%>% 
      collect()
    df2$prueba =tbl(pool, pruebabd) %>% collect() 
    
    df2$noticias = tbl(pool, noticiasbd) %>% 
      filter(estado %in% !!sus)%>% 
      collect() 
    
    df2$opciones = tbl(pool, opcionesbd) %>%
      collect()
    
    
  })
  
  
  
  candidatos <- eventReactive(input$entidad,{
    # candidatos <- bd$temas %>% filter(suscriptor == input$entidad)
    candidatos <- tbl(pool, candidatosbd) %>% collect() %>% 
      filter(suscriptor == input$entidad)
    
    candidatos <- candidatos %>% pull(tema) %>% 
      set_names(candidatos %>% pull(MS_NOMBRE))
  })
  
  
  
  # 
  # df2 <- reactiveValues(
  #   entrenamiento = tbl(pool, entrenamientobd) %>% 
  #     filter(estado %in% !!req(input$entidad))%>% 
  #     collect(),
  #   prueba =tbl(pool, pruebabd) %>% collect() 
  # )
  
  
  
  
  
  
  
  # Módulo de encuestas
  callModule(mod_encuestas_server, "encuestas_ui_1", entidad = entidad)  
  
  # Módulo de redes sociales

  callModule(mod_redes_general_server, "redes_general_ui_1",  df2=df2, candidatos=candidatos)
  
  #Módulo de noticias
  callModule(mod_noticias_general_server, "noticias_general_ui_1", df2=df2)
  
  #Módulo de análisis electoral cualitativo
  callModule(mod_análisis_server, "análisis_ui_1",tipo = "encuestas", entidad = entidad)
  callModule(mod_análisis_server, "análisis_ui_2",tipo = "redes", entidad = entidad)
  callModule(mod_análisis_server, "análisis_ui_3",tipo = "noticias", entidad = entidad)
  callModule(mod_análisis_server, "análisis_ui_4",tipo = "electoral", entidad = entidad)
}
