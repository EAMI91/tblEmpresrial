vSeries<-function(grupos){
  num <-seq_len(grupos)+1
  letra<- letters[num]
  
  area <- paste("this.series[",num,"].update({
      id: '",letra,"'
    }, false);",collapse = " ", sep = "")
  num2 <- num + grupos
  linea <- paste("this.series[",num2,"].update({
      linkedTo: '",letra,"'
    }, false);", collapse = " ", sep = "")
  num3 <- num2 + grupos
  
  puntos <- paste("this.series[",num3,"].update({
      linkedTo: '",letra,"'
    }, false);", collapse = " ", sep = "")
  
  fijo <-paste("function () {
        this.series[0].update({
      id: 'hoyColumnSeries'
    }, false);
    this.series[1].update({
      id: 'eleccionColumnSeries'
    }, false);",area,linea,puntos,"}",sep = "" )
  fijo <-gsub(x =  fijo,pattern = ", false);}", replacement = ");}",fixed = T)
  return(fijo)
}

hPollofPolls <- function(DB, puntos, hoy, eleccion) {
  # Funciones para volver al español
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$weekdays<- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
  hcoptslang$shortMonths <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  hcoptslang$months <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  hcoptslang$thousandsSep <- c(",")
  options(highcharter.lang = hcoptslang)
  
  # Gráfica
  candidatos <- DB %>%  count(candidato) %>%  nrow()
  Graph <- highchart() %>%
    hc_chart(style = list(fontColor = "#1C313D", fontFamily= "Avenir Next"), zoomType = "x",
             events = list(load = JS(vSeries(grupos = candidatos)))
    ) %>%
    hc_add_series(data = hoy,showInLegend = F,hcaes(x= x, y = y), type = "column", color ="#8BA4B0") %>% 
    hc_add_series(data = eleccion,showInLegend = F, hcaes(x= x, y = y), type = "column", color ="#BF374E") %>% 
    hc_add_series(DB, "arearange", 
                  hcaes(x = fecha,  low =ic_025, high = ic_975,
                        group = candidato),
                  enableMouseTracking = T, fillOpacity = 0.15) %>% 
    hc_add_series(DB , "line",
                  hcaes(x = fecha, y = media, group = candidato, grouping = FALSE)) %>% 
    hc_add_series(puntos, hcaes(x = fecha, y = as.numeric(resultado), group = partido), type = "scatter") %>%
    hc_yAxis(tickAmount = 4, min =0,
             title = list(text = "Estimación",
                          style = list( fontSize = "16px", color = "#41657A")),
             labels = list(format = "{value}%") ,
             style = list(fontSize = "18px",color = "#13384D")) %>%
    hc_xAxis(crosshair = T, type = "datetime",
             showLastLabel = F,
             tickLength =0,
             # tickInterval= 1000 * 60 * 60 * 24 * 365,
             labels = list(format = '{value:%B}', step = 2, style = list(fontSize = "18px",color = "#13384D")),
             title = list(text = "Fecha", style = list( fontSize = "16px", color = "#41657A"))) %>%
    hc_plotOptions(arearange = list(marker = list(radius = 0),lineWidth = 0, tooltip = list(enabled = F, pointFormat = '', headerFormat = '')),
                   column = list(pointWidth = "1", legend = list(enabled = F),opacity =.8,
                                 dataLabels = list(enabled = T, format = "{point.tt}"),
                                 tooltip = list(enabled = F, pointFormat = "", headerFormat = "")),
                   line = list(marker = list(radius = 0)),
                   scatter = list(marker = list(symbol = "circle", radius = 2) ,
                                  tooltip = list(pointFormat = '{point.casaEncuestadora} <br>{point.partido}: {point.resultado}% <br><span style="font-size: 12px">Levantamiento:<br> <span style="font-size: 12px">{point.fechaInicio} - {point.fechaFin} </span><br/>', headerFormat = ''))) %>%
    hc_tooltip(sort = T,
               shared = T,
               borderWidth= 0,
               split = T,
               pointFormat = "<br> <b>{series.name}:</b> {point.mediatt}% <span style='font-size: 10px'> [{point.ic_025tt}% - {point.ic_975tt}%]</span>",
               headerFormat = '<span style="font-size: 15px">{point.key}</span><br/>',
               style = list(fontSize = "16px", color = "#41657A"),
               useHTML = F) %>%
    hc_title(text = "<b>Intención de voto estimada por fecha</b>",
             align = "left", style = list(fontSize = "22px", color = "#13384D")) %>% 
    # hc_colors(colors = c("#720026", "#035BAD", "#D10A0A","black","green")) 
    hc_colors(DB %>% group_by(candidato, color) %>%  summarise() %>%  arrange(candidato) %>%  pull(color))
  
  return(Graph)
}


intVotoBarras <- function(bd){
  bd %>% 
    ggplot(aes(x = partido, y  = n, fill = partido, label = n2))+
    geom_chicklet(radius = grid::unit(12, "pt"), width = .25)+
    labs(title = "Intención de voto", y = "", x = "")+
    coord_flip()+theme_minimal()+
    theme(panel.grid= element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 22,
                                    colour =  "#13384D",
                                    hjust = 0, face="bold"),
          text = element_text(family = "Avenir Next", size = 20))+
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    geom_text( hjust = 1.3, color = "#FFFFFF", size = 8)+
    scale_fill_manual(values = c("INDEPENDIENTE 1" = "#925AAD",
                                 "MC" = "#ED6B40",
                                 "MORENA" = "#751438",
                                 "PAN"  = "#17418A",
                                 "PES" = "#54218A",
                                 "PRD" = "#FAB855",
                                 "PRI" = "#EB0E0E",
                                 "PT" = "#D63131",
                                 "INDEPENDIENTE 2" ="#2F9C37"))
}

probGanarOld <- function(bd, candidato, nCand){
  
  pCand <- bd %>% 
    filter(candidato == {{candidato}}) %>% 
    pull("prob") 
  
  c <- bd %>% 
    filter(candidato == {{candidato}})
  
  bd <- bd %>% filter(candidato != {{candidato}}) %>% head(4)
  bd <- union(c, bd)
  
  # browser()
  g <- ggplot(bd) +
    # Marcas
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=0, ymax=25), alpha = 0.05, fill = "#C5C3C4")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=25, ymax=50), alpha = 0.10, fill = "#C5C3C4")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=50, ymax=75), alpha = 0.15, fill = "#C5C3C4")+
    geom_rect(aes(xmin=0, xmax=nCand+1, ymin=75, ymax=100), alpha = 0.20, fill = "#C5C3C4")+
    # Indicadores
    geom_rect(aes(xmin=rw, xmax=rw+.8, ymin=0, ymax=prob, fill = candidato, color = color),
              size=.3,color="white") +
    coord_polar(theta = "y")+
    geom_text(aes(x=-nCand, y=0, label=scales::percent(pCand/100)), size=8) +
    scale_fill_manual(values = c("INDEPENDIENTE 1" = "#925AAD",
                                 "MC" = "#ED6B40",
                                 "MORENA" = "#751438",
                                 "PAN"  = "#17418A",
                                 "PES" = "#54218A",
                                 "PRD" = "#FAB855",
                                 "PRI" = "#EB0E0E",
                                 "PT" = "#D63131",
                                 "INDEPENDIENTE 2" ="#2F9C37"))+
    labs(title = "Probabilidad de triunfo")+
    xlim(c(-nCand, nCand+1))+
    ylim(c(0,100))+
    theme_minimal()+
    theme(
      text = element_text(family = "Avenir Next", size = 20),
      plot.title = element_text(size = 22,
                                colour =  "#13384D",
                                hjust = 0, face="bold"),
      axis.text.y = element_blank(),
      axis.line.x = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.title = element_blank(),
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 40/.pt),
      axis.title = element_blank()
    )
  return(g)
}