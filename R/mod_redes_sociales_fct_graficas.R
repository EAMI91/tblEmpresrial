library(magrittr)
library(dplyr)
library(lubridate)
library(highcharter)


reach <- function(bd){
  hchart(bd, hcaes(x = fecha, y = n, group = grupo), type = "line") %>%
    hc_plotOptions(line= list(lineWidth = 4,
                              marker = list(radius =0),
                              stickyTracking=F)) %>%
    hc_xAxis(crosshair = T, title = list(text = "Fecha",  style = list(color = "#FFF")), type = "datetime",
             lineWidth = 0, tickWidth  = 0, gridLineWidth =0,
             showLastLabel= F,
             labels = list(step = 3, style = list(fontSize = "16px", color = "#FFF") )) %>%
    hc_yAxis(crosshair = F, title = list(text = "Total", style = list(color = "#FFF")), tickAmount = 3, max = 10, min =0,
             dashStyle = "dot",
             gridLineWidth =.5, showFirstLabel = F, gridLineColor = "",
             labels = list( style = list(fontSize = "12px") )) %>%
    #tooltip
    hc_tooltip(
      borderWidth= 0,
      outside = T,
      textOutline= "3px contrast",
      shadow=F,
      shared = T,
      split = F,
      headerFormat= '<span style="font-size: 10px">{point.key}</span><br/>'
      # pointFormat = '<span style="color:{point.color}">●</span> <b> {point.candidato}<b><br> p. clave: {point.palabra}<br> {point.n} tuits <br> {point.rt} retuits<br> {point.favs} favoritos<br>'
    ) %>%
    hc_colors(colors = c("#2C6170", "#FF6B6B", "#FFE66D", "steelblue"))  %>% 
    hc_chart(style = list(fontFamily = "Avenir next"
    ))%>%
    hc_add_theme(hc_theme_google())%>%
    #title
    hc_title(text = "Alcance en redes sociales",  style = list(fontWeight = "bold", fontSize = "15px")) 
  
}



graficando_saldo <- function(df){
  hchart(df, hcaes(y = n2, group = voto, x = mes), type = "bar") %>%
    hc_colors(colors = c("#f03b20", "#31a354")) %>%
    hc_plotOptions(bar = list(stacking = T, borderRadius = 5,
                              dataLabels= list(enabled =F,
                                               align= "center",
                                               inside= F,
                                               # rotation = 5,
                                               verticalAlign= 'top',
                                               # x = -5,
                                               crop= F,
                                               overflow= "none",
                                               style=list(color="BLACK", fontSize = "25px", textOutline= "3px contrast"),
                                               format = paste0("{point.n:,.0f} ")
                                               
                              )
    ))%>%
    
    hc_yAxis(labels= list(formatter = JS("function(){ return Math.abs(this.value); }"))) %>%
    hc_add_theme(hc_theme_google())%>%
    #title
    hc_title(text = "Balance de comentarios",  style = list(fontWeight = "bold", fontSize = "15px")) 
  
}




