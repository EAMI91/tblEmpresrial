#' noticias UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import highcharter dplyr tibble quanteda plotly tidyr ggfittext ggthemes

mod_noticias_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 12, class="shadowBox",
        shinycssloaders::withSpinner(highchartOutput(ns("timeNoticias")))
            ),
      column(width =  6, class="shadowBox",
             shinycssloaders::withSpinner(plotOutput(ns("nubePalabras")))
        ),
      column(width =  6, class="shadowBox",
             shinycssloaders::withSpinner(plotlyOutput(ns("termometro")))
        ),
      column(width = 12, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("temasEleccion")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("tipoEventos")))
      ), 
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("percepcionMedios")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("mencionGenerada")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("mencionNoGenerada")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("califGenerada1")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("califGenerada2")))
      ), 
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("califGenerada3")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(highchartOutput(ns("califGenerada4")))
      )
    )
  )
}
    
#' noticias Server Function
#'
#' @noRd 
mod_noticias_server <- function(input, output, session, entidad){
  ns <- session$ns
  
  bd <- reactive({
    n <- 1000
    v <- rep("Este es un texto a ser noticia", n)
    x <- c("12/01/2020", "13/01/2020","14/01/2020", "15/01/2020", "16/01/2020", "17/01/2020",
           "12/02/2020", "13/02/2020","14/02/2020", "15/02/2020", "16/02/2020", "17/02/2020",
           "12/01/2021", "13/01/2021","14/01/2021", "15/01/2021", "16/01/2021", "17/01/2021",
           "12/02/2021", "13/02/2021","14/02/2021", "15/02/2021", "16/02/2021", "17/02/2021")
    X <- sample(c("Buena", "Mala", "Regular"), n, replace=T) 
    filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
    text <- readLines(filePath) 
    remove <- c("", " ")
    text <- setdiff(text, remove)
    
    BD <- tibble(
    id = 1:n, title = v, calificacion = X,
    fecha = as.Date(rep(x, len = n)),
    text = rep(text, len = n),               
    temas = sample(c("Deportes", "Cultura", "Sociedad", "Tecnología", "Otros"), n, replace=T),
    entidad = sample(c("Michoacán", "Nuevo León"), 
                     n, replace = T, 
                     prob = c(.5, .5))
    ) %>% 
    mutate(temasOtro = case_when(temas == "Otros" ~ "Otro tema"), Noticias = 1)
    BD <- filter(BD, entidad==!!entidad())
  })
  
  bd_2 <- reactive({
    n <- 1000
    BD <- tibble(
    id = 1:n,
    tipoEvento = sample(c("Político", "Electoral", "Acto de\nCampaña"), n, replace = T),
    candidato = sample(c("candidato 1", "candidato 2", "candidato 3", "candidato 4"), n, replace = T),
    percepcion = sample(c("Buena", "Mala", "Regular"), n, replace = T),
    mencionGenerada = sample(c("Boletines\n de prensa", "declaraciones", "filtraciones"), n, replace = T),
    mencionNoGenerada = sample(c("personaje", "columnista", "adversario", "partidario"), n, replace = T),
    calif_generada = sample(c("Mala", "Buena", "Regular"), n, replace = T),
    calif_no_generada = sample(c("Buena", "Mala", "Regular"), n, replace = T), 
    entidad = sample(c("Michoacán", "Nuevo León"), 
                     n, replace = T, 
                     prob = c(.5, .5))
    )
    BD <- filter(BD, entidad==!!entidad())
    })
  
  output$timeNoticias <- renderHighchart({
    E <- bd() %>% 
      group_by(calificacion, fecha) %>% 
      summarise(across(Noticias, sum))
    
    timeline_noticias(E)
  })
  
  output$nubePalabras <- renderPlot({
    Nube <- procesando_nube_not(bd(), 10)
    graficando_nube_not(Nube, 10)
  })
  
  output$termometro <- renderPlotly({
  termometro_electoral()
  })
 
  output$temasEleccion <- renderHighchart({
    temas_eleccion(bd(),
                   pregunta = temas,
                   otro = temasOtro,
                   x = 0,
                   titulo = "Temas de la elección general")
  })
  
  output$tipoEventos <- renderHighchart({
    barras_candidatos(bd_2(), c("candidato 1", "candidato 2", "candidato 3"),
                      col="tipoEvento", "Tipos de enventos")
    })
  
  output$percepcionMedios <- renderHighchart({
    
    barras_candidatos(bd_2(), c("candidato 1", "candidato 2", "candidato 3"),
                      col="percepcion", "Percepción en Medios")
    })
  
  output$mencionGenerada <- renderHighchart({
    barras_candidatos(bd_2(), c("candidato 1", "candidato 2", "candidato 3"),
                      col="mencionGenerada", "Menciones del candidato generadas")
  })
  
  output$mencionNoGenerada <- renderHighchart({
    barras_candidatos(bd_2(), c("candidato 1", "candidato 2", "candidato 3"),
                      col="mencionNoGenerada", "Menciones del candidato no generadas")
      })
  
  output$califGenerada1 <- renderHighchart({
    treemap_calificacion_bis(bd_2(), candida="candidato 1")
  })
  
  output$califGenerada2 <- renderHighchart({
    treemap_calificacion_bis(bd_2(), candida="candidato 2")
  })
  output$califGenerada3 <- renderHighchart({
    treemap_calificacion_bis(bd_2(), candida="candidato 3")
  })
  
  output$califGenerada4 <- renderHighchart({
    treemap_calificacion_bis(bd_2(), candida="candidato 4")
  })
}
 
## To be copied in the UI
# mod_noticias_ui("noticias_ui_1")
    
## To be copied in the server
# callModule(mod_noticias_server, "noticias_ui_1")
 
