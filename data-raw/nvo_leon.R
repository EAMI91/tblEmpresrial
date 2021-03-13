## code to prepare `nvo_leon` dataset goes here

token <- rtweet::create_token(
  app = "R_Mining_chemi",
  consumer_key = "egBPz5BVerF974Nsk4rYuozne",
  consumer_secret = "aXO7swCGnH4biykQNAt5gochFsbMFnm6bOwilPIMRqLKJ2nt3Z", 
  access_token = "101707868-zV8saQyrQU5TyQ8M4z5ZPNEo6KvS7svxMekJRJ6n",
  access_secret = "CMlK4cX499SpaPyg3gwOxxVNF6TRsAM5YeAY3AOaN7yWp",
  set_renv = F
)

candidato1 <- rtweet::get_timeline("claraluzflores", n = 3200, token = token)
candidato2 <- rtweet::get_timeline("adriandelagarza", n = 3200, token = token)
candidato3 <- rtweet::get_timeline("ferlarrazabalnl", n = 3200, token = token)


ca1 <- candidato1 %>% 
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
  filter(fecha ==max(fecha),is_retweet=="FALSE") %>% 
  arrange(-intera) %>% 
  filter(intera==max(intera)) %>% 
  select(TW_Entities = screen_name, TW_StatusID = status_id, candidato)

ca3 <- candidato3 %>% 
  mutate(intera=favorite_count+retweet_count) %>% 
  mutate(candidato="Candidato 3", 
         fecha=lubridate::floor_date(created_at, "day")) %>% 
  filter(fecha ==max(fecha),is_retweet=="FALSE") %>% 
  arrange(-intera) %>% 
  filter(intera==max(intera)) %>% 
  select(TW_Entities = screen_name, TW_StatusID = status_id, candidato)

ca1 <- bind_rows(ca1, ca2)
nvo_leon <- bind_rows(ca1, ca3)

usethis::use_data(nvo_leon, overwrite = TRUE)
