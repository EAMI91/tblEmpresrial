# library(magrittr)
# library(dplyr)
# library(lubridate)
# library(highcharter)
# library(quanteda)
# library(quanteda.textplots)
# library(here)

reach <- function(bd){
  hchart(bd, hcaes(x = fecha, y = n, group = grupo), type = "line") %>%
    hc_plotOptions(line= list(lineWidth = 4,
                              marker = list(radius =6),
                              stickyTracking=F)) %>%
    hc_xAxis(crosshair = T, title = list(text = F), type = "datetime",
             lineWidth = 0, tickWidth  = 0, gridLineWidth =0,
             showLastLabel= T,
             labels = list(format = '{value:%b %d}', step = 1, 
                           style = list(fontSize = "18px",
                                        color = "#13384D"))) %>%
    hc_yAxis(labels= list(formatter = JS("function(){ return Math.abs(this.value); }"))) %>% 
    #title
    hc_colors(colors = c("#2C6170", "#FF6B6B", "#FFE66D")) %>%
    #tooltip
    # hc_tooltip(
    #   borderWidth= 0,
    #   outside = T,
    #   textOutline= "3px contrast",
    #   shadow=F,
    #   shared = T,
    #   split = F,
    #   headerFormat= '<span style="font-size: 10px">{point.key}</span><br/>'
    # ) %>%
    
  hc_chart(style = list(fontFamily = "Avenir next"
  ))%>%
    hc_add_theme(hc_theme_google())%>%
    #title
    hc_title(text = "Alcance en redes sociales",  style = list(fontWeight = "bold", fontSize = "15px"))
  
}


 #reach(proyectos)




graficando_saldo <- function(df){
  hchart(df, hcaes(x = fecha,y = n, group = calificacion), type = "column", stacking = "normal") %>%
    hc_colors(colors = c("#f03b20",  "grey", "#31a354")) %>%
    hc_yAxis(labels= list(formatter = JS("function(){ return Math.abs(this.value); }"))) %>%
    hc_xAxis(crosshair = T, title = list(text = F), type = "datetime",
             lineWidth = 0, tickWidth  = 0, gridLineWidth =0,
             showLastLabel= T,
             labels = list(format = '{value:%b %d}', step = 1, 
                           style = list(fontSize = "18px",
                                        color = "#13384D"))) %>%
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
    
    hc_add_theme(hc_theme_google())%>%
    #title
    hc_title(text = "Balance de comentarios",  style = list(fontWeight = "bold", fontSize = "15px")) 
}


# graficando_saldo(df)


#### Nube-----





procesando_nube <- function(DB){
  corp_quanteda <- corpus(DB, text_field = "TW_Text")
  Nube <- dfm(corp_quanteda, remove = stopwords("spanish"), 
              remove_punct = TRUE, groups = "calificacion")

}

graficando_nube <- function(DB){
  nube <- textplot_wordcloud(DB, comparison = TRUE, 
                             max_words = 100, adjust = 0, 
                   rotation   = 0, random_order = FALSE,
                   random_color = FALSE,fixed_aspect=TRUE,
                   ordered_color = FALSE,
                   color = c("#f03b20", "#31a354")) + theme_minimal()
  return(nube)
}




blockquote <- function(TW_Entities, TW_StatusID, null_on_error = F){
  oembed_url <- glue::glue("https://publish.twitter.com/oembed?url=https://twitter.com/{TW_Entities}/status/{TW_StatusID}&omit_script=1&dnt=1&theme=light&lang=es")
  bq <- purrr::possibly(httr::GET, list(status_code = 999))(URLencode(oembed_url))
  if (bq$status_code >= 400) {
    if (null_on_error) return(NULL)
    glue('<blockquote style="font-size: 90%">Lo sentimos, no fue posible mostrar el tuit de {TW_Entities}¯\\_(ツ)_/¯</blockquote>')
  } else {
    httr::content(bq, "parsed")$html
  }
}

