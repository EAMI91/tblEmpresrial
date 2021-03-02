## code to prepare `candidatos` dataset goes here
library(readr)
candidatos <- read_csv("data-raw/aspirantes_prueba_mich_qro_nl.csv")%>% mutate(`Fecha de nacimiento` = lubridate::dmy(`Fecha de nacimiento`))
usethis::use_data(candidatos, overwrite = TRUE)
