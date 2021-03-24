


barras_candidatos <- function(bd,  col, title){
  
  bd <- bd %>%
    filter(!is.na(Candidato)) %>% 
    mutate(n  =  1) %>%
    group_by(!!sym(col), Candidato, color) %>%
    summarise(across(n, sum)) %>% 
   ungroup() %>%
  mutate(totales=sum(n),
         percentage=round((n/totales)*100,2)) %>%
  mutate(label = paste(round(percentage, 1), "%", sep = " "))
  
  graph <- hchart(bd, hcaes(x = !!sym(col), y = percentage, 
                            group = Candidato, color=color), 
         type = "bar") %>%
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text = as.character(title)) %>% 
  hc_xAxis(title = " ") %>% 
  hc_yAxis(title = " ") %>%   
  hc_colors(colors = unique(bd$color[order(bd$Candidato)])) %>% 
  hc_chart(style = list(fontFamily = "Avenir next"
  ))
return(graph)
}


treemap_calificacion_cand <- function(BD){
  
  base1 <- BD %>% 
    # filter(candidato %in% candida)%>%
    select(calificacion) %>% 
    filter(!is.na(calificacion)) %>% 
    unique() %>% 
    mutate(color=if_else(calificacion=="Positiva",
                         "#FFFFFF",                     
                         if_else(calificacion=="Negativa",
                                 "#FFFFFF","#FFFFFF")),
           id=str_to_id(calificacion)) %>% 
    rename("name"="calificacion")
  base2 <- BD %>% 
    # filter(candidato %in% candida)%>%
    count(calificacion, tipoFuente) %>% 
    filter(!is.na(calificacion)) %>% 
    
    mutate(color=if_else(calificacion=="Positiva" & 
                 tipoFuente=="Boletín de prensa",
                 "#006d2c",
                 if_else(calificacion=="Positiva" & 
                 tipoFuente=="Declaraciones",
                 "#31a354",
                 if_else(calificacion=="Positiva" & 
                 tipoFuente=="Filtraciones",
                 "#74c476",
                 if_else(calificacion=="Negativa" & 
                 tipoFuente=="Boletín de prensa",
                 "#a50f15",
                 if_else(calificacion=="Negativa" & 
                 tipoFuente=="Declaraciones",
                 "#de2d26",
                 if_else(calificacion=="Negativa" & 
                 tipoFuente=="Filtraciones",
                 "#fb6a4a", 
                 if_else(calificacion=="Neutra" & 
                 tipoFuente=="Boletín de prensa",
                 "#feb24c",
                 if_else(calificacion=="Neutra" & 
                 tipoFuente=="Declaraciones",
                 "#fed976","#ffffb2")))))))),
           parent=str_to_id(calificacion),
           id = as.character(row_number())) %>% 
           rename("name"= "tipoFuente", "value"="n")
  dde <- list(base1, base2) %>%
    purrr::map(mutate_if, is.factor, as.character) %>% 
    bind_rows() %>% 
    list_parse() %>% 
    purrr::map(function(x) x[!is.na(x)])
  
  gra <- highchart() %>% 
    hc_chart(type = "treemap") %>% 
    hc_add_theme(hc_theme_google()) %>% 
    hc_title(
      text = paste0("Calificación de menciones de ", unique(BD$Candidato))
    ) %>%
    hc_add_series(
      data = dde,
      allowDrillToNode = TRUE,
      levelIsConstant = TRUE,
      textOverflow = "clip",
      dataLabels = list(color = "white"),
      levels = list(
        list(
          level = 1,
          borderWidth = 8,
          dataLabels = list(
            enabled = TRUE,
            verticalAlign = "top",
            align = "left",
            style = list(fontSize = "12px", textOutline = FALSE)
          )
        ),
        list(
          level = 2,
          borderWidth = 0,
          dataLabels = list(enabled = FALSE)
        )
      )
    ) %>% 
    # esto es para que el primer nivel, que no tiene color asigando, 
    # sea transparente.
    hc_colors("trasnparent")
  
  return(gra)
}

