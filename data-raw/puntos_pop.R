## code to prepare `puntos_pop` dataset goes here
library(readr)
puntos_pop <- read_csv("vignettes/puntos_pollofpolls.csv")
usethis::use_data(puntos_pop, overwrite = TRUE)
