## code to prepare `nvo_leon_menciones` dataset goes here

candidato1 <- rtweet::search_tweets("@claraluzflores", n = 3200, token = token) 
tempo1 <- candidato1 %>% 
  mutate(candidato="Candidato 1", 
         fecha=lubridate::floor_date(created_at, "day")) %>% 
  arrange(-followers_count) %>% 
  filter(fecha ==max(fecha),is_retweet=="FALSE") %>% 
  filter(followers_count==max(followers_count), 
         is.na(reply_to_user_id)) %>% 

  filter(created_at==max(created_at)) %>% 
  select(TW_Entities = screen_name, TW_StatusID = status_id, candidato)  

candidato2 <- rtweet::search_tweets("@adriandelagarza", n = 3200, token = token) 
tempo2 <- candidato2 %>% 
  mutate(candidato="Candidato 2", 
         fecha=lubridate::floor_date(created_at, "day")) %>% 
  arrange(-followers_count) %>% 
  filter(fecha ==max(fecha),is_retweet=="FALSE") %>% 
  filter(followers_count==max(followers_count), 
         is.na(reply_to_user_id)) %>% 
  filter(created_at==max(created_at)) %>% 
  select(TW_Entities = screen_name, TW_StatusID = status_id, candidato) 

candidato3 <- rtweet::search_tweets("@ferlarrazabalnl", n = 3200, token = token) 
tempo3 <- candidato3 %>% 
  mutate(candidato="Candidato 3", 
         fecha=lubridate::floor_date(created_at, "day")) %>% 
  arrange(-followers_count) %>% 
  filter(fecha ==max(fecha),is_retweet=="FALSE") %>% 
  filter(followers_count==max(followers_count), 
         is.na(reply_to_user_id)) %>% 
  filter(created_at==max(created_at)) %>% 
  select(TW_Entities = screen_name, TW_StatusID = status_id, candidato)  

tempo1 <- bind_rows(tempo1, tempo2)
nvo_leon_menciones <- bind_rows(tempo1, tempo3)
nvo_leon_menciones <- mutate(nvo_leon_menciones, entidad="Nuevo León")

usethis::use_data(nvo_leon_menciones, overwrite = TRUE)
