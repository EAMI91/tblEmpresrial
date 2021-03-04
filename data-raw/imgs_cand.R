## code to prepare `imgs_cand` dataset goes here

library(readr)
imgs_cand <- read_csv("data-raw/aspirantes_prueba_mich_qro_nl.csv") %>% mutate(`Fecha de nacimiento` = lubridate::dmy(`Fecha de nacimiento`))
usethis::use_data(imgs_cand, overwrite = TRUE)
