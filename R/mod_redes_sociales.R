#' redes_sociales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import dplyr highcharter ggplot2 tibble lubridate
mod_redes_sociales_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
   column(width = 12, 
             selectInput(ns("candidato"), label = "Selecciones candidato", choices = c("Cargando..."=""))
      ) 

    ),
    fluidRow(
      column(width = 12,class="shadowBox",
             highchartOutput(ns("reach"))
      )
    ), 

    fluidRow(
      column(width = 6, 
             tags$h4(HTML( "Comparativo")),
             class="shadowBox",
             highchartOutput(ns("saldo"))
      ), 
      column(width = 6, 
             tags$h4(HTML( "Contenido de comentarios")),
             class="shadowBox",
             shinycssloaders::withSpinner(
               plotOutput(ns("nube"))
             ))    
 
  ), 
  fluidRow(
    column(width = 6, 
           tags$h4(HTML( "Tweet con más interacciones")),
           shinycssloaders::withSpinner(uiOutput(ns("masFavs")), 
                                        proxy.height = "200px")
    ), 
    column(width = 6, 
           tags$h4(HTML( "Tweet con mayor alcance")),
           shinycssloaders::withSpinner(uiOutput(ns("masmencion")), 
                                        proxy.height = "200px")
    )    
    
  )
  
  )
}
    
#' redes_sociales Server Function
#'
#' @noRd 
mod_redes_sociales_server <- function(input, output, session, df2, candidatos){
  ns <- session$ns
  
  
  observe({ updateSelectInput(session, inputId = "candidato",choices = candidatos())
  })
  
  
  output$reach <- renderHighchart({
    tempo <- df2$entrenamiento %>% 
      filter(tema==input$candidato) %>% 
      mutate(fecha=floor_date(TW_CreatedAt, "day"), 
             fecha=as.Date(fecha), 
             tuits=1) %>%
      select(TW_Entities:TW_InReplyToStatusID, tuits, TW_FavoriteCount:fecha) %>% 
      group_by(fecha) %>%
      summarise(across(tuits:TW_RetweetCount,
                       ~ sum(.x, na.rm = TRUE))) %>% 
      rename("Tweets"="tuits", "Favoritos"="TW_FavoriteCount", 
             "RTs"="TW_RetweetCount") %>% 
      tidyr::gather(grupo, n, "Tweets":"RTs") 
    
    reach(tempo)
  })
  


  output$saldo <- renderHighchart({
     en <- df2$entrenamiento %>% 
       filter(tema==input$candidato) %>% 
       mutate(fecha=floor_date(TW_CreatedAt,unit="day"), 
              fecha=as.Date(fecha)) %>% 
       count(fecha, calificacion) %>% 
       filter(!is.na(calificacion)) %>% 
       mutate(color=if_else(calificacion=="Negativo", "#f03b20", 
                            if_else(calificacion=="Positivo", "#31a354", "#F0F0F0"))) 
    graficando_saldo(en)
     })
  
  nube <- reactive({
    en <- df2$entrenamiento %>% 
      filter(tema==input$candidato) %>% 
      filter(!is.na(calificacion)) %>% 
      select(calificacion, TW_Text) 
    procesando_nube(en)
  })
  
  output$nube <- renderPlot({
    graficando_nube(nube())
    })
  

  
  output$masFavs <- renderUI({
    tagList(
        df2$entrenamiento %>% 
        filter(tema==input$candidato) %>% 
        mutate(alca=TW_RetweetCount+TW_FavoriteCount) %>% 
        filter(alca==max(alca)) %>% 
        filter(TW_CreatedAt ==max(TW_CreatedAt)) %>%   
          # filter(TW_InReplyToStatusID ==0 ) %>% 
        select(TW_Entities,TW_StatusID) %>% 
        blockquote(TW_Entities = .$TW_Entities,TW_StatusID = .$TW_StatusID) %>% 
        HTML(),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
    
  })
  
  output$masmencion <- renderUI({
    tagList(
        df2$entrenamiento %>% 
          filter(tema==input$candidato) %>% 
          filter(TW_FollowersCount ==max(TW_FollowersCount )) %>% 
          # filter(TW_InReplyToStatusID ==0 ) %>%  
          select(TW_Entities,TW_StatusID) %>% 
          blockquote(TW_Entities = .$TW_Entities,TW_StatusID = .$TW_StatusID) %>% 
        HTML(),
      tags$script('twttr.widgets.load(document.getElementById("tweet"));')
    )
    
  })
  
}
 
 
    
## To be copied in the UI
# mod_redes_sociales_ui("redes_sociales_ui_1")
    
## To be copied in the server
# callModule(mod_redes_sociales_server, "redes_sociales_ui_1")
 
