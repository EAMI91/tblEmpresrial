### Redes-----

procesando_red_menciones <- function(DB){
  tag_dfm <- dfm_select(DB, pattern = ("@*"))
  toptag <- names(topfeatures(tag_dfm, 20))
  tag_fcm <- fcm(tag_dfm)
  topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
}

graficando_red <- function(DB){
  textplot_network(DB,
                   min_freq = 0.1, edge_alpha = 0.5, 
                   edge_size = 5)
  
}


procesando_red_hashtag <- function(DB){
  tag_dfm <- dfm_select(DB, pattern = ("#*"))
  toptag <- names(topfeatures(tag_dfm, 20))
  tag_fcm <- fcm(tag_dfm)
  topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
}