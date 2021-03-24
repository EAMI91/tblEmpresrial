#' noticias_barras UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_noticias_barras_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
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
           shinycssloaders::withSpinner(highchartOutput(ns("treemap1")))
    ),
    column(width = 6, class="shadowBox",
           shinycssloaders::withSpinner(highchartOutput(ns("treemap2")))
    ),
    column(width = 6, class="shadowBox",
           shinycssloaders::withSpinner(highchartOutput(ns("treemap3")))
    ),
    column(width = 6, class="shadowBox",
           shinycssloaders::withSpinner(highchartOutput(ns("treemap4")))
    )
 
  )
  )
}
    
#' noticias_barras Server Function
#'
#' @noRd 
mod_noticias_barras_server <- function(input, output, session, df2){
  ns <- session$ns
  
  

  bd.cand <- reactiveVal(NULL)
  bd.tema <- reactiveVal(NULL)
  bd.partido <- reactiveVal(NULL)
  
  observeEvent(df2$opciones, 
    {   
    bd.cand(
    df2$opciones %>% select(idCategoria, variable, categoria) %>%
      pivot_wider(names_from = variable, values_from = categoria) %>% 
      mutate(Candidato = idCandidato, idCandidato = idCategoria) %>%
      select(idCandidato, Candidato) %>% 
      filter(!is.na(Candidato))
  )
    
    bd.tema(
      df2$opciones %>% select(idCategoria, variable, categoria) %>%
        pivot_wider(names_from = variable, values_from = categoria) %>% 
        mutate(Tema_1 = idTema, idTema_1 = idCategoria,
               Tema_2 = Tema_1, idTema_2 = idTema_1,
               Tema_3 = Tema_2, idTema_3 = idTema_2) %>%
        select(idTema_1, Tema_1,idTema_2, Tema_2, idTema_3, Tema_3) %>% 
        filter(!is.na(Tema_1)) )
    
    bd.partido(
      
      df2$opciones %>% select(idCategoria, variable, categoria) %>%      
        pivot_wider(names_from = variable, values_from = categoria) %>% 
        mutate(Partido = idPartido, idPartido = idCategoria) %>%       
        select(idPartido, Partido) %>% 
        filter(!is.na(Partido))%>% 
        mutate(color=if_else(Partido=="MORENA", "#BF3722", 
                     if_else(Partido=="INDEPENDIENTE", "#9E7BB5",
                     if_else(Partido=="PAN", "#2260BF",
                     if_else(Partido=="PRI", "#23A95D","#E6BD19")))))
      
      )})

  
 # %>% 
 #    left_join(bd.tema() %>% select(contains("Tema_1")), by = "idTema_1") %>% 
 #    left_join(bd.tema() %>% select(contains("Tema_2")), by = "idTema_2") %>% 
 #    left_join(bd.tema() %>% select(contains("Tema_3")), by = "idTema_3") 
  
  
  
  
    output$tipoEventos <- renderHighchart({
    barra<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato") %>% 
      left_join(bd.partido(), by = "idPartido")
    
      barras_candidatos(barra, col="evento", title="Tipo de evento")
    

  })
  
  output$percepcionMedios <- renderHighchart({
    barra2<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato")%>% 
      left_join(bd.partido(), by = "idPartido")
    
    barras_candidatos(barra2, col="calificacion", title="Percepción en medios")
  })
  
  output$mencionGenerada <- renderHighchart({
    barra3<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato") %>% 
      left_join(bd.partido(), by = "idPartido") %>% 
      filter(nota!="Nota no generada")
    barras_candidatos(barra3, col="tipoMedio", "Menciones del candidato generadas")
  })
  
  output$mencionNoGenerada <- renderHighchart({
    
    barra4<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato") %>% 
      left_join(bd.partido(), by = "idPartido") %>% 
      filter(nota=="Nota no generada")
    
    barras_candidatos(barra4, col="tipoMedio",  "Menciones del candidato no generadas")
  })
  
  
  output$treemap1 <- renderHighchart({
    
    treemuno<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato") %>%
      filter(idCandidato==max(idCandidato))
    
    treemap_calificacion_cand(treemuno)
  })
  
  output$treemap2 <- renderHighchart({
    
  treemdos<-   df2$noticias %>% 
    left_join(bd.cand(), by = "idCandidato") %>%
    filter(idCandidato!=max(idCandidato)) %>% 
    filter(idCandidato==max(idCandidato))
  
  treemap_calificacion_cand(treemdos)
})
  
  output$treemap3 <- renderHighchart({
    
    treemtres<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato") %>%
      filter(idCandidato!=max(idCandidato)) %>% 
      filter(idCandidato!=max(idCandidato))%>% 
      filter(idCandidato==max(idCandidato))
    
    treemap_calificacion_cand(treemtres)
  })
  
  output$treemap4 <- renderHighchart({
    
    treemcuatro<-   df2$noticias %>% 
      left_join(bd.cand(), by = "idCandidato") %>%
      filter(idCandidato!=max(idCandidato)) %>% 
      filter(idCandidato!=max(idCandidato))%>% 
      filter(idCandidato!=max(idCandidato))%>% 
      filter(idCandidato==max(idCandidato))
    
    treemap_calificacion_cand(treemcuatro)
  })
  
}
    
## To be copied in the UI
# mod_noticias_barras_ui("noticias_barras_ui_1")
    
## To be copied in the server
# callModule(mod_noticias_barras_server, "noticias_barras_ui_1")
 
