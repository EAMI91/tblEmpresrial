#' candidatos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_candidatos_ui <- function(id){
  ns <- NS(id)
  tagList(
    slickROutput(ns("candidatos"),width='90%',height = 100) 
  )
}

#' candidatos Server Function
#'
#' @noRd 
mod_candidatos_server <- function(input, output, session, entidad){
  ns <- session$ns
  
  bd <- reactive({
    imgs_cand %>% filter(Entidad == !!entidad()) %>% 
      mutate(img = glue::glue("inst/{entidad()}/Gobernador/{foto_perfil}"))#,
             # nacimiento = glue::glue("{`Lugar de nacimiento`} ({format(`Fecha de nacimiento`,'%d %B %y')})"),
             # texto = paste(Candidato,Partido,nacimiento,Estudios,`Cargo actual`,`Perfil político`,sep = "\n"))
  })
  
  output$candidatos <- renderSlickR({
    validate(
      need(nrow(bd())>0,message = "No se ha registrado ningún candidato para esta entidad.")
    )
    imagen <- slickR(
      bd()$img,height = 700,width = "100%",slideId = "candidatos",
    ) + settings(autoplay = T, autoplaySpeed = 5000)
    # texto <- slickR(elementId = "nombre",bd()$texto,slideType = "p",slideId  = "candidatos2") + settings(dots = T, arrows = F)
    # partido <- slickR(bd()$Partido,slideType = "p") + settings(arrows = F)
    # nacimiento <- slickR(bd()$nacimiento,slideType = "p") + settings(arrows = F)
    # estudios <- slickR(bd()$Estudios,slideType = "p") + settings(arrows = F)
    # cargo <- slickR(bd()$`Cargo actual`,slideType = "p") + settings(arrows = F)
    # perfil <- slickR(bd()$`Perfil político`,slideType = "p") + settings(arrows = F)
    
    # lista <- list(imagen,
    #               texto)
                  # nombre,
                  # partido,
                  # nacimiento,
                  # estudios,
                  # cargo,
                  # perfil)
    imagen
    # Reduce(`%synch%`, lista)
    # a <- imagen %synch% nombre
    # b <- partido %synch% nacimiento
    # imagen %stack% nombre %stack% partido
    # (((((imagen %synch% nombre) %synch% partido) %synch% nacimiento) %synch% estudios) %synch% cargo) %synch% perfil
  }) 
}

## To be copied in the UI
# mod_candidatos_ui("candidatos_ui_1")

## To be copied in the server
# callModule(mod_candidatos_server, "candidatos_ui_1")

