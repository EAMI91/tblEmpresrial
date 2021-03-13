## code to prepare `paratuit` dataset goes here
load(here::here("data", "FSM.rda"))
load(here::here("data", "candidato2.rda"))
load(here::here("data", "candidato3.rda"))
ca1 <- FSM %>% 
  mutate(intera=favorite_count+retweet_count) %>% 
  mutate(candidato="Candidato 1", 
         fecha=lubridate::floor_date(created_at, "day")) %>% 
  filter(fecha ==max(fecha),is_retweet=="FALSE") %>% 
  arrange(-intera) %>% 
  filter(intera==max(intera)) %>% 
  select(TW_Entities = screen_name, TW_StatusID = status_id, candidato)  

ca2 <- candidato2 %>% 
  mutate(intera=favorite_count+retweet_count) %>% 
  mutate(candidato="Candidato 2", 
         fecha=lubridate::floor_date(created_at, "day")) %>% 
  filter(fecha ==max(fecha), is_retweet=="FALSE") %>% 
  arrange(-intera) %>% 
  filter(intera==max(intera)) %>% 
  select(TW_Entities = screen_name, TW_StatusID = status_id, candidato)

ca3<- candidato3 %>% 
  mutate(intera=favorite_count+retweet_count) %>% 
  mutate(candidato="Candidato 3",
         fecha=lubridate::floor_date(created_at, "day")) %>% 
  filter(fecha ==max(fecha), is_retweet=="FALSE") %>% 
  arrange(-intera) %>% 
  filter(intera==max(intera)) %>% 
  select(TW_Entities = screen_name, TW_StatusID = status_id, candidato)

ca1 <- bind_rows(ca1, ca2)
ca1 <- bind_rows(ca1, ca3)
ca1 <- mutate(ca1, entidad="Michoacán")
load(here::here("data", "nvo_leon.rda"))
nvo_leon <- mutate(nvo_leon, entidad="Nuevo León")
paratuit <- bind_rows(ca1, nvo_leon)
usethis::use_data(paratuit, overwrite = TRUE)
