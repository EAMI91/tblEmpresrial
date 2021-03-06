## code to prepare `bd_pop` dataset goes here
library(readr)
bd_pop <- read_csv("vignettes/bd_pollofpolls.csv")

usethis::use_data(bd_pop, overwrite = TRUE)
