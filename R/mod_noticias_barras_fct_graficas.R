


barras_candidatos <- function(bd,  col, title){
  
  bd <- bd %>%
mutate(n  =  1) %>%
  group_by(!!sym(col), Candidato) %>%
  summarise(across(n, sum)) %>% 
  mutate(percentage =round(100*n/sum(n))) %>% 
  mutate(label = paste(round(percentage, 1), "%", sep = " ")) %>% 
  filter(!is.na(Candidato)) 
  
  graph <- hchart(bd, hcaes(x = !!sym(col), y = percentage, group = Candidato), 
         type = "bar") %>%
  hc_add_theme(hc_theme_google()) %>% 
  hc_title(text = as.character(title)) %>% 
  hc_xAxis(title = "") %>% 
  hc_colors(c("#cb2833", "#174a80", "#00A896",
              "#0f4c42"))%>%
  hc_chart(style = list(fontFamily = "Avenir next"
  ))
return(graph)
}
