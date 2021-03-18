## code to prepare `menciones_1` dataset goes here
token <- rtweet::create_token(
  app = "R_Mining_chemi",
  consumer_key = "egBPz5BVerF974Nsk4rYuozne",
  consumer_secret = "aXO7swCGnH4biykQNAt5gochFsbMFnm6bOwilPIMRqLKJ2nt3Z", 
  access_token = "101707868-zV8saQyrQU5TyQ8M4z5ZPNEo6KvS7svxMekJRJ6n",
  access_secret = "CMlK4cX499SpaPyg3gwOxxVNF6TRsAM5YeAY3AOaN7yWp",
  set_renv = F
)



candidato1 <- rtweet::search_tweets("@raulmoronO", n = 3200, token = token) 
  tempo1 <- candidato1 %>% 
       mutate(candidato="Candidato 1", 
       fecha=lubridate::floor_date(created_at, "day")) %>% 
       arrange(-followers_count) %>% 
       filter(followers_count==max(followers_count), 
              is.na(reply_to_user_id)) %>% 
      filter(fecha ==max(fecha),is_retweet=="FALSE") %>% 
      filter(created_at==max(created_at)) %>% 
      select(TW_Entities = screen_name, TW_StatusID = status_id, candidato)  
  
candidato2 <- rtweet::search_tweets("@CarlosHerreraSi", n = 3200, token = token) 
  tempo2 <- candidato2 %>% 
    mutate(candidato="Candidato 2", 
           fecha=lubridate::floor_date(created_at, "day")) %>% 
    arrange(-followers_count) %>% 
    filter(followers_count==max(followers_count), 
           is.na(reply_to_user_id)) %>% 
    filter(fecha ==max(fecha),is_retweet=="FALSE") %>% 
    filter(created_at==max(created_at)) %>% 
    select(TW_Entities = screen_name, TW_StatusID = status_id, candidato) 

  candidato3 <- rtweet::search_tweets("@TonoIxtlahuac", n = 3200, token = token) 
  tempo3 <- candidato3 %>% 
    mutate(candidato="Candidato 3", 
           fecha=lubridate::floor_date(created_at, "day")) %>% 
    arrange(-followers_count) %>% 
    filter(followers_count==max(followers_count), 
           is.na(reply_to_user_id)) %>% 
    filter(fecha ==max(fecha),is_retweet=="FALSE") %>% 
    filter(created_at==max(created_at)) %>% 
    select(TW_Entities = screen_name, TW_StatusID = status_id, candidato)  
  
  
  
  
  tempo1 <- bind_rows(tempo1, tempo2)
  menciones_mich <- bind_rows(tempo1, tempo3)
  menciones_mich <- mutate(menciones_mich, entidad="Michoacán")
  
  load(here::here("data", "nvo_leon_menciones.rda"))
  
  menciones_1 <- bind_rows(menciones_mich, nvo_leon_menciones)
  
  
  
  usethis::use_data(menciones_1, overwrite = TRUE)
