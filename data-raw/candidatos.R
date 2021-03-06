## code to prepare `candidatos` dataset goes here

load(here::here("data", "FSM.rda"))
load(here::here("data", "candidato2.rda"))
load(here::here("data", "candidato3.rda"))

FSM <- select(FSM, text) %>% 
       mutate(candidato="Candidato 1")

candidato2 <- select(candidato2, text) %>% 
  mutate(candidato="Candidato 2")

candidato3 <- select(candidato3, text) %>% 
  mutate(candidato="Candidato 3")

candidatos <- bind_rows(FSM, candidato2)
candidatos <- bind_rows(candidatos, candidato3)
candidatos$entidad <- sample(c("Michoacán", "Nuevo León"), 
                             size = nrow(candidatos), replace = T, prob = c(.5, .5))

usethis::use_data(candidatos, overwrite = TRUE)
