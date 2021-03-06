## code to prepare `puntos_pop` dataset goes here
library(readr)
puntos <- read_csv("vignettes/puntos_pollofpolls.csv")
usethis::use_data(puntos, overwrite = TRUE)
