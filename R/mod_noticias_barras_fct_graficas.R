
barras_candidatos <- function(bd, candidatos, col, title){
  
  bd <- bd %>%
    mutate(n  =  1) %>%
    group_by(!!sym(col), candidato) %>%
    summarise(across(n, sum)) %>% 
    mutate(percentage =round(100*n/sum(n))) %>% 
    mutate(label = paste(round(percentage, 1), "%", sep = " "))
  
  Graph <- hchart(bd, hcaes(x = !!sym(col), y = percentage, group = candidato), 
                  type = "bar") %>%
    hc_add_theme(hc_theme_google()) %>% 
    hc_title(text = as.character(title)) %>% 
    hc_xAxis(title = "") %>% 
    hc_colors(c("#174a80", "#00A896",
                "#0f4c42", "#cb2833"))%>%
    hc_chart(style = list(fontFamily = "Avenir next"
    ))
  
  
  
  
  return(Graph)
}