timeline_noticias <- function(bd){
  # Funciones para volver al español
  # hcoptslang <- getOption("highcharter.lang")
  # hcoptslang$weekdays<- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  # hcoptslang$shortMonths <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  # hcoptslang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  # hcoptslang$thousandsSep <- c(",")
  # options(highcharter.lang = hcoptslang)
  
  
  hc <- bd %>% 
    hchart( hcaes(x = fecha, y = n, group = calificacion), type="line"
    )  %>%
    hc_colors(c("#BBC200", "#710627", "#CF8C40")) %>% 
    hc_plotOptions(line= list(lineWidth = 4,
                              marker = list(radius =5),
                              stickyTracking=F)) %>% 
    hc_xAxis(crosshair = T, title = list(text = "Fecha"), type = "datetime",
             lineWidth = 0, tickWidth  = 0, gridLineWidth =0, 
             showLastLabel= F,
             labels = list(step = 2, style = list(fontSize = "16px", color = "#001c44") )) %>%
    hc_yAxis(crosshair = F, title = list(text = "Número de Noticias"), tickAmount = 3, 
             gridLineWidth =.5, showFirstLabel = F,
             labels = list( style = list(fontSize = "12px") )) %>%
    #title
    hc_title(text = "<b>Calificación de Noticias</b>",
             align = "left", style = list(fontSize = "22px", color = "#13384D")) %>% 
    hc_tooltip(
      sort = F,
      shared = T,
      borderWidth= 0,
      split = T,
      pointFormat = "<br> <b>Calificación</b>: {series.name} <br> <p><b> Número de noticias</b>: {point.Noticias} </p>",
      headerFormat = '<span style="font-size: 15px">{point.key}</span><br/>',
      style = list(fontSize = "16px", color = "#41657A"),
      useHTML = F) %>% 
    hc_chart(style = list(fontFamily = "Avenir Next"), backgroundColor = "#FFF")
  return(hc)
}

procesando_nube_not <- function(bd){
  
  words <- select(bd, texto, calificacion) 
  titulo <- "Temas electoraes"
  #corp_tm <- tm::VCorpus(tm::VectorSource(words))
  corp_quanteda <- corpus(words,text_field = "texto")
  Nube <- dfm(corp_quanteda, remove = stopwords("english"),
              remove_punct = TRUE, groups = "calificacion")
  return(Nube)
}

graficando_nube_not <- function(db, z){
  nube <- textplot_wordcloud(db, min_count = z,comparison = TRUE, max_words = 300, adjust = 0, rotation   = 0.1, random_order = FALSE,random_color = FALSE, ordered_color = FALSE,
                             # font = "Avenir Next",
                             labelsize= 1.5,
                             labelcolor =  "#161F29",
                             labeloffset = .001,
                             color = c("#0f4c42", "#cb2833", "#91d400", "#174a80", "#ffc200"))+ theme_minimal()+
    theme(text=element_text(size=16,   family="Avenir Next"),
          panel.border = element_blank())
  
  return(nube)
}

termo <- function(nivel){
  termom <-  highchart() %>% 
    hc_chart(
      type = "gauge",
      plotBackgroundColor = NULL,
      plotBackgroundImage = NULL,
      plotBorderWidth = 0,
      plotShadow = FALSE
    ) %>% 
    hc_title(
      text = "Termómetro electoral estatal"
    ) %>% 
    hc_add_theme(hc_theme_google()) %>% 
    hc_pane(
      startAngle = -150,
      endAngle = 150,
      background = list(list(
        backgroundColor = list(
          linearGradient = list( x1 = 0, y1 = 0, x2 = 0, y2 = 1),
          stops = list(
            list(0, "#FFF"),
            list(1, "#333")
          )
        ),
        borderWidth = 0,
        outerRadius = "109%"
      ), list(
        backgroundColor = list(
          linearGradient = list( x1 = 0, y1 = 0, x2 = 0, y2 = 1),
          stops = list(
            list(0, "#333"),
            list(1, "#FFF")
          )
        ),
        borderWidth = 1,
        outerRadius = "107%"
      ), list(
        # default background
      ), list(
        backgroundColor = "#DDD",
        borderWidth = 0,
        outerRadius = "105%",
        innerRadius = "103%"
      ))
    ) %>% 
    hc_add_series(
      data = nivel, name = "nivel", tooltip = list(valueSuffix = " ")
    ) %>% 
    hc_chart(style = list(fontFamily = "Avenir next"
    )) %>% 
    
    hc_yAxis(
      min = 0,
      max = 100,
      
      minorTickInterval = "auto",
      minorTickWidth = 1,
      minorTickLength = 10,
      minorTickPosition = "inside",
      minorTickColor = "#666",
      
      tickPixelInterval = 30,
      tickWidth = 2,
      tickPosition = "inside",
      tickLength = 10,
      tickColor = "#666",
      
      labels = list(
        step = 2,
        rotation = "auto"
      ),
      title = list(
        text = ""
      ),
      
      plotBands = list(
        list(from =   0, to = 25, color = "#55BF3B"),
        list(from = 25, to = 50, color = "#DDDF0D"),
        list(from = 50, to = 75, color = "#FFA500"),
        list(from = 75, to = 100, color = "#DF5353")
      )
      
    )
  
  return(termom)
}
temas_eleccion <- function(bd, pregunta, otro, x, titulo =""){
  bd_2 <- bd %>% filter({{ pregunta }} %in% c('Otro'))
  
  bd_1 = count(bd, {{ pregunta }}) %>%
    mutate(n  = round(100*n/sum(n),2)) %>%
    arrange(-n)
  
  nTot <- bd_1 %>%
    select(n) %>%
    mutate(sum = sum(n)) %>%
    select(sum)
  
  nTot <- nTot[1,1]$sum
  
  bd_1 <- bd_1 %>%
    filter(!{{ pregunta }} %in% c('Otro'))
  
  bd_1 <- bd_1 %>% mutate(n  = round(n/100, 2)) %>%
    spread(value = n, key = {{ pregunta }})
  
  bd_2 <- count(bd_2, {{ otro }}) %>%
    mutate(porcentaje  = round(100*n/sum(n), 2)) %>%
    filter(porcentaje > x)
  
  bd_2 <- select(bd_2, -porcentaje) 
  
  bd_2 <- bd_2 %>% mutate(n = round(n/nTot,2)) %>%
    spread(value = n, key = {{ otro }})
  
  if(nrow(bd_1) != nrow(bd_2)){
    
    Graph <- bd_1 %>%  gather(x, y) %>% mutate(y = round(y *100)) %>% 
      hchart(hcaes(x = x, y  =y), type = "line") %>%  
      hc_title(text =paste("<b>", titulo,"<b>") , align = "left", style = list(fontSize = "22px", color = "#13384D")) %>% 
      hc_plotOptions(line = list(lineWidth = 7, marker = list(radius = 7))) %>% 
      hc_yAxis(lineWidth =0, title = list(enabled = F),
               tickAmount = 4, showLastLabel = T,
               gridLineWidth  =1,
               gridLineDashStyle = "longdash",
               lineDashStyle = "longdash",
               labels = list(enabled = F,  format=paste0("{value}%"))) %>% 
      hc_xAxis(title = list(enabled = F), 
               lineWidth = 1,
               gridLineWidth =0,
               labels = list(style = list(
                 fontFamily = "Avenir Next",
                 # color = "#0c5776",
                 fontSize = "16px"))) %>% 
      hc_add_theme(hc_theme_google()) %>% 
      hc_colors("#4F5F80") %>% 
      hc_tooltip(headerFormat = "",
                 pointFormat= '<b>{point.y}%</b>',
                 borderWidth= 0, shape = "square", shadow=F) %>% 
      hc_chart(polar = T, style = list(fontFamily = "Avenir Next"))
  }else{
    
    df <- data.frame(bd_1, bd_2)
    titulos <- df %>%  colnames() %>%  str_to_sentence()
    df <- df %>%  set_names(titulos)
    Graph <-  df %>% gather(x, y) %>% mutate(y = round(y *100)) %>% 
      hchart(hcaes(x = x, y  =y), type = "line") %>%  
      hc_chart(polar = T, style = list(fontFamily = "Avenir Next")) %>% 
      hc_title(text = titulo,
               align = "left", style = list(fontSize = "22px", color = "#13384D")) %>% 
      hc_plotOptions(line = list(lineWidth = 5, marker = list(radius = 7))) %>% 
      hc_yAxis(lineWidth =0, title = list(enabled = F),
               # tickAmount = 3, 
               showLastLabel = T,
               gridLineDashStyle = "longdash",
               lineDashStyle = "longdash",
               labels = list(  format=paste0("{value}%"))) %>% 
      hc_xAxis(title = list(enabled = F), labels = list(style = list(color = "#0c5776", fontFamily  = "Avenir Next", fontSize = "18px"))) %>% 
      hc_colors("#81cbfe")%>% 
      hc_add_theme(hc_theme_google()) %>% 
      hc_tooltip(headerFormat = "",
                 pointFormat= '<b>{point.y}%</b>',
                 borderWidth= 0, shape = "square", shadow=F)
    
  }
  
  return(Graph)
}



mencion_generada <- function(bd, candidatos){
  
  bd <- bd %>%
    filter(candidato %in% candidatos)%>% 
    mutate(n  =  1) %>%
    group_by(mencionGenerada, candidato) %>%
    summarise(across(n, sum)) %>% 
    mutate(percentage = 100*n/sum(n)) %>% 
    mutate(label = paste(round(percentage, 1), "%", sep = " "))
  
  Graph <- ggplot(bd, aes(x = mencionGenerada, y = percentage,
                          fill = as.factor(candidato), label = label)) +
    geom_col(width = 0.9, position = position_dodge(0.95)) + 
    geom_bar_text(position = "dodge", grow = F, reflow = T, place = "top") +
    theme_hc() + scale_fill_calc() + 
    labs(title = "Menciones del candidato generadas", caption = "", x = "", y = "") +
    theme(
      axis.ticks.y=element_blank(), 
      axis.title.y = element_blank(),
      axis.title.x = element_text(color = "#8b878d"),
      text = element_text(family = "Avenir Next", size = 12),
      plot.title = element_text(size = 15,
                                colour =  "#13384D",
                                hjust = 0, face = "bold"),
      axis.text.y = element_blank(),
      axis.text.x = element_text(family = "Avenir Next", size = 12),
      legend.title=element_blank()
    )
  
  return(Graph)
}

barras_calificada <- function(bd, cand, col, calif, title){
  
  bd <- bd %>%
    filter(candidato == cand) %>% 
    mutate(n = 1) %>% 
    group_by({{ col }}, {{ calif }}) %>% 
    summarise(across(n, sum)) %>% 
    mutate(col = stringr::str_to_sentence({{ col }}),
           calif = factor({{ calif }}, c("Mala", "Regular", "Buena")))
  
  cand_1 <- as.character(cand)
  
  Graph <- ggplot(bd, aes(y = n, x = col ,
                          fill = as.factor(calif), label = n)) +
    ggchicklet::geom_chicklet(position = ggplot2::position_dodge(),
                              radius = grid::unit(4, "pt"), width = .9, alpha =.8)+
    geom_bar_text(position = "dodge", grow = F, reflow = F, 
                  # place = "top",
                  color = "#FFFFFF")+
    theme_minimal() + scale_fill_manual(values = c("Buena"= "#89A100",
                                                   "Mala" = "#710627", 
                                                   "Regular" = "#E08931")) +
    labs(title = paste(as.character(title), cand_1, sep = " "),
         caption = "", x = "Frecuencia", y = "") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(color = "#8b878d"),
      text = element_text(family = "Avenir Next", size = 12),
      plot.title = element_text(size = 14, colour =  "#13384D", hjust = 0, face = "bold"),
      axis.text.y = element_text(family = "Avenir Next", size = 12),
      legend.title = element_blank()
    )+ coord_flip()
  
  return(Graph)
}

treemap_calificacion <- function(BD, candida){    
  base1 <- BD %>% 
    filter(candidato %in% candida)%>% 
    select(mencionGenerada) %>% 
    unique() %>% 
    mutate(color=if_else(mencionGenerada=="Boletines\n de prensa", "#FFFFFF", 
                         if_else(mencionGenerada=="declaraciones","#FFFFFF","#FFFFFF")),
           id = str_to_id(mencionGenerada)
    )%>%
    rename("name"="mencionGenerada")
  base2 <- BD %>% 
    filter(candidato %in% candida)%>% 
    count(percepcion, mencionGenerada) %>% 
    mutate(color=if_else(percepcion=="Buena",
                         "#0f4c42",                     
                         if_else(percepcion=="Mala",
                                 "#cb2833","#808080")),
           parent=str_to_id(mencionGenerada),
           id = as.character(row_number())) %>% 
    rename("name"= "percepcion", "value"="n")
  dde <- list(base1, base2) %>%
    purrr::map(mutate_if, is.factor, as.character) %>% 
    bind_rows() %>% 
    list_parse() %>% 
    purrr::map(function(x) x[!is.na(x)])
  
  gra <- highchart() %>% 
    hc_chart(type = "treemap") %>% 
    hc_title(
      text = paste0("Calificación de menciones ", candida)
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
    hc_colors("trasnparent") %>% 
    hc_chart(style = list(fontFamily = "Avenir next"
    ))
  
  
  return(gra)
}

treemap_calificacion_bis <- function(BD){
  
  
  
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
    mutate(tipoFuente=iconv(tipoFuente,"UTF-8","WINDOWS-1252")) %>% 
    
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
                 if_else(calificacion=="Neutral" & 
                 tipoFuente=="Boletín de prensa",
                 "#feb24c",
                 if_else(calificacion=="Neutral" & 
                 tipoFuente=="declaraciones",
                 "#fed976","#ffffb2")))))))),
           parent=str_to_id(calificacion),
           id = as.character(row_number())) %>% 
    rename("name"= "tipoFuente", "value"="n")
  dde <- list(base1, base2) %>%
    purrr::map(mutate_if, is.factor, as.character) %>% 
    bind_rows() %>% 
    list_parse() %>% 
    purrr::map(function(x) x[!is.na(x)])
  
  gra <-  highchart() %>% 
    hc_chart(type = "treemap") %>% 
    hc_title(
      text = "Califación de menciones de la elección"
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


