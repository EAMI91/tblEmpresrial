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
             shinycssloaders::withSpinner(plotOutput(ns("tipoEventos")))
      ), 
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(plotOutput(ns("percepcionMedios")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(plotOutput(ns("mencionGenerada")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(plotOutput(ns("mencionNoGenerada")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(plotOutput(ns("califGenerada")))
      ),
      column(width = 6, class="shadowBox",
             shinycssloaders::withSpinner(plotOutput(ns("califNoGenerada")))
      )
    )
  )
}
    
#' noticias Server Function
#'
#' @noRd 
mod_noticias_server <- function(input, output, session){
  ns <- session$ns
  
  bd <- reactive({
    n <- 1000
    v <- rep("Este es un texto a ser noticia", n)
    A <- tibble(id = 1:n, title = v) 
    X <- sample(c("Buena", "Mala", "Regular"), n, replace=T) 
    B <- tibble(id = 1:n, calificacion = X)
    C <- A %>% left_join(B) 
    
    x <- c("12/01/2020", "13/01/2020","14/01/2020", "15/01/2020", "16/01/2020", "17/01/2020",
           "12/02/2020", "13/02/2020","14/02/2020", "15/02/2020", "16/02/2020", "17/02/2020",
           "12/01/2021", "13/01/2021","14/01/2021", "15/01/2021", "16/01/2021", "17/01/2021",
           "12/02/2021", "13/02/2021","14/02/2021", "15/02/2021", "16/02/2021", "17/02/2021")
    
    fechas <- tibble(id = 1:n, fecha = as.Date(rep(x, len = n)))                 
    D <- C %>% left_join(fechas) %>% mutate(Noticias = 1)
    
    filePath <- "http://www.sthda.com/sthda/RDoc/example-files/martin-luther-king-i-have-a-dream-speech.txt"
    text <- readLines(filePath) 
    remove <- c("", " ")
    text <- setdiff(text, remove)
    desc <- tibble(id = 1:n, text = rep(text, len = n))               
    G <- left_join(D, desc)
    a <- tibble( id = 1:n,
                 temas = sample(c("Deportes", "Cultura", "Sociedad", "Tecnología", "Otros"), n, replace=T))
    a <- mutate(a, temasOtro = case_when(temas == "Otros" ~ "Otro tema"))
    
    H <- G %>% left_join(a)
    H
  })
  
  bd_2 <- reactive({
    n <- 1000
    Eventos <- tibble(
    id = 1:n,
    tipoEvento = sample(c("Político", "Electoral", "Acto de\nCampaña"), n, replace = T),
    candidato = sample(c("candidato 1", "candidato 2", "candidato 3", "candidato 4"), n, replace = T)
    )
    percep <- tibble(
    id = 1:n, 
    percepcion = sample(c("Buena", "Mala", "Regular"), n, replace = T),
    )
    BD <- left_join(Eventos, percep)
    mencion <- tibble(
    id = 1:n, 
    mencionGenerada = sample(c("Boletines\n de prensa", "declaraciones", "filtraciones"), n, replace = T),
    )
    BD <- left_join(BD, mencion)
    mencionNo <- tibble(
    id = 1:n, 
    mencionNoGenerada = sample(c("personaje", "columnista", "adversario", "partidario"), n, replace = T),
    )
    BD <- left_join(BD, mencionNo)
    calificacion_generada <- tibble(
    id = 1:n, 
    calif_generada = sample(c("Mala", "Buena", "Regular"), n, replace = T),
    )
    BD <- left_join(BD, calificacion_generada)
    calificacion_no_generada <- tibble(
    id = 1:n, 
    calif_no_generada = sample(c("Buena", "Mala", "Regular"), n, replace = T),
    )
    BD <- left_join(BD, calificacion_no_generada)
    BD
    })
  
  output$timeNoticias <- renderHighchart({
    E <- bd() %>% 
      group_by(calificacion, fecha) %>% 
      summarise(across(Noticias, sum))
    
    timeline_noticias(E)
  })
  
  output$nubePalabras <- renderPlot({
    Nube <- procesando_nube(bd(), 10)
    graficando_nube(Nube, 10)
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
  
  output$tipoEventos <- renderPlot({
  
    tipos_eventos(bd_2(), c("candidato 1", "candidato 2", "candidato 3"))
  })
  
  output$percepcionMedios <- renderPlot({
  
    percepcion_medios(bd_2(),  c("candidato 1", "candidato 2", "candidato 3"))
  })
  
  output$mencionGenerada <- renderPlot({
  
    mencion_generada(bd_2(),  c("candidato 1", "candidato 2", "candidato 3"))
  })
  
  output$mencionNoGenerada <- renderPlot({
    
    mencion_no_generada(bd_2(),  c("candidato 1", "candidato 2", "candidato 3"))
  })
  
  output$califGenerada <- renderPlot({
  
    calificada_generada(bd_2(), cand = "candidato 1")
  })
  
  output$califNoGenerada <- renderPlot({
    
    calificada_no_generada(bd_2(),  cand = "candidato 2")
  })
}
 
## To be copied in the UI
# mod_noticias_ui("noticias_ui_1")
    
## To be copied in the server
# callModule(mod_noticias_server, "noticias_ui_1")
 
