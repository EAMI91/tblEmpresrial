timeline_noticias <- function(bd){
  # Funciones para volver al español
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$weekdays<- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  hcoptslang$shortMonths <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  hcoptslang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  hcoptslang$thousandsSep <- c(",")
  options(highcharter.lang = hcoptslang)
  
  hc <- bd %>% 
    hchart(
      'spline', hcaes(x = fecha, y = Noticias, group = calificacion)
    )  %>%
    hc_colors(c("#008F39", "#EE0000", "#787676")) %>% 
    hc_plotOptions(line= list(lineWidth = 3,
                              marker = list(radius =0),
                              stickyTracking=F)) %>% 
    hc_xAxis(crosshair = T, title = list(text = "Fecha"), type = "datetime",
             lineWidth = 0, tickWidth  = 0, gridLineWidth =0, 
             showLastLabel= F,
             labels = list(step = 3, style = list(fontSize = "16px", color = "#001c44") )) %>%
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
      pointFormat = "<br> <b>Calificación </b>: {series.name} <br> <p><b> Número de noticias </b>: {point.Noticias} </p>",
      headerFormat = '<span style="font-size: 15px">{point.key}</span><br/>',
      style = list(fontSize = "16px", color = "#41657A"),
      useHTML = F) 
  return(hc)
}


procesando_nube <- function(bd, z){
  words <- select(bd, text, calificacion) %>% na.omit() 
  titulo <- "Temas electoraes"
  corp_quanteda <- corpus(words)
  Nube <- dfm(corp_quanteda, remove = stopwords("english"),
              remove_punct = TRUE, groups = "calificacion")%>%
    dfm_trim(min_termfreq = z)
  return(Nube)
}

graficando_nube <- function(db, z){
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

termometro_electoral <- function(){
  termometro <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = 70,
    title = list(text = "Termómetro electoral estatal", font = list(size = 20)),
    gauge = list(
      axis = list(range = list(NULL, 100), tickwidth = 1, tickcolor = "#4F4F4F"),
      bar = list(color = "#4F4F4F", width = 0.5, thickness = 0.15),
      bgcolor = "white",
      borderwidth = 2.5,
      bordercolor = "gray",
      steps = list(
        list(range = c(0, 20), color = "#FF0134"),
        list(range = c(20, 40), color = "#FF9E50"),
        list(range = c(40, 60), color = "#FFC750"),
        list(range = c(60, 80), color = "#8FCA5D"),
        list(range = c(80, 100), color = "#00CF66")
      ),
      threshold = list(
        line = list(color = "red", width = 4),
        thickness = 1.0,
        value = 70))
  ) 
  
  termometro <- termometro %>%
    layout(
      margin = list(l=15,r=30),
      paper_bgcolor = "white",
      font = list(color = "dark", family = "Arial"))
  
  return(termometro)
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
      hc_chart(polar = T) %>% 
      hc_title(text = titulo, style = list(fontFamily = "Avenir Next")) %>% 
      hc_plotOptions(line = list(lineWidth = 4, marker = list(radius = 7))) %>% 
      hc_yAxis(lineWidth =0, title = list(enabled = F),
               tickAmount = 4, showLastLabel = T,
               gridLineDashStyle = "longdash",
               lineDashStyle = "longdash",
               labels = list(  format=paste0("{value}%"))) %>% 
      hc_xAxis(title = list(enabled = F), labels = list(style = list(color = "#0c5776"))) %>% 
      hc_colors("#81cbfe") %>% 
      hc_tooltip(headerFormat = "",
                 pointFormat= '<b>{point.y}%</b>',
                 borderWidth= 0, shape = "square", shadow=F)
  }else{
    
    df <- data.frame(bd_1, bd_2)
    titulos <- df %>%  colnames() %>%  str_to_sentence()
    df <- df %>%  set_names(titulos)
    Graph <-  df %>% gather(x, y) %>% mutate(y = round(y *100)) %>% 
      hchart(hcaes(x = x, y  =y), type = "line") %>%  
      hc_chart(polar = T, style = list(fontFamily = "Avenir Next")) %>% 
      hc_title(text = titulo) %>% 
      hc_plotOptions(line = list(lineWidth = 4, marker = list(radius = 7))) %>% 
      hc_yAxis(lineWidth =0, title = list(enabled = F),
               tickAmount = 4, showLastLabel = T,
               gridLineDashStyle = "longdash",
               lineDashStyle = "longdash",
               labels = list(  format=paste0("{value}%"))) %>% 
      hc_xAxis(title = list(enabled = F), labels = list(style = list(color = "#0c5776"))) %>% 
      hc_colors("#81cbfe")%>% 
      hc_tooltip(headerFormat = "",
                 pointFormat= '<b>{point.y}%</b>',
                 borderWidth= 0, shape = "square", shadow=F)
    
  }
  
  return(Graph)
}

tipos_eventos <- function(bd, candidatos){
  
  bd <- bd %>%
    filter(candidato %in% candidatos)%>% 
    mutate(n  =  1) %>%
    group_by(tipoEvento, candidato) %>%
    summarise(across(n, sum)) %>% 
    mutate(percentage = 100*n/sum(n)) %>% 
    mutate(label = paste(round(percentage, 1), "%", sep = " "))
  
  Graph <- ggplot(bd, aes(x = tipoEvento, y = percentage,
                          fill = as.factor(candidato), label = label)) +
    geom_col(width = 0.9, position = position_dodge(0.95)) + 
    geom_bar_text(position = "dodge", grow = F, reflow = T, place = "top") +
    theme_hc() + scale_fill_calc() + 
    labs(title = "Tipos de eventos", caption = "", x = "", y = "") +
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

percepcion_medios <- function(bd, candidatos){
  
  bd <- bd %>%
    filter(candidato %in% candidatos)%>% 
    mutate(n  =  1) %>%
    group_by(percepcion, candidato) %>%
    summarise(across(n, sum)) %>% 
    mutate(percentage = 100*n/sum(n)) %>% 
    mutate(label = paste(round(percentage, 1), "%", sep = " "))
  
  Graph <- ggplot(bd, aes(x = percepcion, y = percentage,
                          fill = as.factor(candidato), label = label)) +
    geom_col(width = 0.9, position = position_dodge(0.95)) + 
    geom_bar_text(position = "dodge", grow = F, reflow = T, place = "top") +
    theme_hc() + scale_fill_calc() + 
    labs(title = "Percepción en Medios", caption = "", x = "", y = "") +
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

mencion_no_generada <- function(bd, candidatos){
  
  bd <- bd %>%
    filter(candidato %in% candidatos)%>% 
    mutate(n  =  1) %>%
    group_by(mencionNoGenerada, candidato) %>%
    summarise(across(n, sum)) %>% 
    mutate(percentage = 100*n/sum(n)) %>% 
    mutate(label = paste(round(percentage, 1), "%", sep = ""))
  
  Graph <- ggplot(bd, aes(x = mencionNoGenerada, y = percentage,
                          fill = as.factor(candidato), label = label)) +
    geom_col(width = 0.9, position = position_dodge(0.95)) + 
    geom_bar_text(position = "dodge", grow = F, reflow = T, place = "top") +
    theme_hc() + scale_fill_calc() + 
    labs(title = "Menciones del candidato no generadas", caption = "", x = "", y = "") +
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

calificada_generada <- function(bd, cand){
  
  bd <- bd %>%
    filter(candidato == cand) %>% 
    mutate(n = 1) %>% 
    group_by(mencionGenerada, calif_generada) %>% 
    summarise(across(n, sum))
  
  cand_1 <- as.character(cand)
  
  Graph <- ggplot(bd, aes(x = n, y = mencionGenerada,
                          fill = as.factor(calif_generada), label = n)) +
    geom_col(width = 0.9, position = position_dodge(0.95)) +
    geom_bar_text(position = "dodge", grow = F, reflow = T, place = "right") +
    theme_minimal() + scale_fill_manual(values = c("#33CA7F", "#FC9F5B", "#ECE4B7")) +
    labs(title = paste("Calificación de menciones generadas de", cand_1, sep = " "),
         caption = "", x = "Frecuencia", y = "") +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(color = "#8b878d"),
      text = element_text(family = "Avenir Next", size = 12),
      plot.title = element_text(size = 14, colour =  "#13384D", hjust = 0, face = "bold"),
      axis.text.y = element_text(family = "Avenir Next", size = 12),
      legend.title = element_blank()
    )
  
  return(Graph)
}

calificada_no_generada <- function(bd, cand){
  
  bd <- bd %>%
    filter(candidato == cand) %>% 
    mutate(n = 1) %>% 
    group_by(mencionNoGenerada, calif_no_generada) %>% 
    summarise(across(n, sum)) 
  
  cand_1 <- as.character(cand)
  
  Graph <- ggplot(bd, aes(x = n, y = mencionNoGenerada,
                          fill = as.factor(calif_no_generada), label = n)) +
    geom_col(width = 0.9, position = position_dodge(0.95)) +
    geom_bar_text(position = "dodge", grow = F, reflow = T, place = "right") +
    theme_minimal() + scale_fill_manual(values = c("#33CA7F", "#FC9F5B", "#ECE4B7")) +
    labs(title = paste("Calificación de menciones no generadas de", cand_1, sep = " "),
         caption = "", x = "Frecuencia", y = "") +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(color = "#8b878d"),
      text = element_text(family = "Avenir Next", size = 12),
      plot.title = element_text(size = 14, colour =  "#13384D", hjust = 0, face = "bold"),
      axis.text.y = element_text(family = "Avenir Next", size = 12),
      legend.title=element_blank()
    )
  
  return(Graph)
}