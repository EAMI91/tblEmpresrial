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
    entidades <- df3$entidadesb %>% pull(estado) 
    updateSelectizeInput(session,
                         'entidad',
                         choices = entidades,
                         selected = entidades,
                         server = TRUE)
  })
  
   observeEvent(input$entidad, {
      df2$entrenamiento = tbl(pool, entrenamientobd) %>% 
        filter(estado %in% !!req(input$entidad))%>% 
        collect()
      df2$prueba =tbl(pool, pruebabd) %>% collect() 
      
      df2$noticias = tbl(pool, noticiasbd) %>% 
        filter(estado %in% !!req(input$entidad))%>% 
        collect() 
      
      df2$opciones = tbl(pool, opcionesbd) %>%
        collect()
      
      
  })
  

  
  
  
  # 
  # df2 <- reactiveValues(
  #   entrenamiento = tbl(pool, entrenamientobd) %>% 
  #     filter(estado %in% !!req(input$entidad))%>% 
  #     collect(),
  #   prueba =tbl(pool, pruebabd) %>% collect() 
  # )
  

  
  

  
  
  # Módulo de encuestas
  callModule(mod_encuestas_server, "encuestas_ui_1", bd = bd)  
  
  # Módulo de redes sociales
  callModule(mod_redes_general_server, "redes_general_ui_1",  df2=df2)
  
  #Módulo de noticias
  callModule(mod_noticias_general_server, "noticias_general_ui_1", df2=df2)

  #Módulo de análisis electoral cualitativo
  callModule(mod_a_electoral_server, "a_electoral_ui_1")
}
