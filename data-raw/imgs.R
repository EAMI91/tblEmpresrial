## code to prepare `imgs` dataset goes here
library(pdftools)
library(here)
library(purrr)
library(dplyr)
library(tidyr)
# entidades <- tibble::tribble(~nombre, ~min,
#                 "Michoacán","MICH",
#                 "Nuevo León", "NL"
# )

entidades <- list(
  c("Michoacán","MICH"),
  c("Nuevo León","NL")
)
analisis <- c("electoral","encuestas","noticias","redes")

comb <- cross2(entidades,analisis)

archivos <- comb %>% map(~{
  setwd(glue::glue("~/Documents/Git/tblEmpresrial/inst/{.x[[1]][[1]]}/{.x[[2]][[1]]}"))
  imgs <- pdftools::pdf_convert(here(glue::glue('inst/{.x[[1]][[2]]}_{.x[[2]][[1]]}.pdf')),format = 'png',verbose = FALSE,dpi = 300)
  pdfs <- paste("inst",.x[[1]][[1]],.x[[2]][[1]],imgs,sep = "/")  
})

archivos <- archivos %>% do.call(base::c,.) %>% tibble %>% set_names("archivo") %>% 
  separate(archivo, into = c("carpeta","entidad","tipo","imagen"),sep = "/",remove = F) %>% 
  separate(imagen, into = c("a","b","id"),sep = "_") %>% mutate(id = as.numeric(gsub(".png","",x = id,fixed = T))) %>% select(archivo,entidad,tipo,id)
library(readr)
fechas <- read_csv(here("data-raw","fechas.csv"))
# fechas <- tibble::tribble(~id, ~entidad, ~tipo, ~fecha,
#                          1, "Michoacán", "electoral", ymd("2021-02-02"),
#                          2, "Michoacán", "electoral", ymd("2021-03-02"),
#                          1, "Michoacán", "encuestas", ymd("2021-02-02"),
#                          2, "Michoacán", "encuestas", ymd("2021-03-02"),
#                          1, "Michoacán", "noticias", ymd("2021-02-02"),
#                          2, "Michoacán", "noticias", ymd("2021-03-02"),
#                          1, "Michoacán", "redes", ymd("2021-02-02"),
#                          2, "Michoacán", "redes", ymd("2021-03-02"),
#                          1, "Nuevo León", "electoral", ymd("2021-02-02"),
#                          2, "Nuevo León", "electoral", ymd("2021-03-02"),
#                          1, "Nuevo León", "encuestas", ymd("2021-02-02"),
#                          2, "Nuevo León", "encuestas", ymd("2021-03-02"),
#                          1, "Nuevo León", "noticias", ymd("2021-02-02"),
#                          2, "Nuevo León", "noticias", ymd("2021-03-02"),
#                          1, "Nuevo León", "redes", ymd("2021-02-02"),
#                          2, "Nuevo León", "redes", ymd("2021-03-02")
#                          )
# 

# write_excel_csv(here("data-raw","fechas.csv"),x = fechas)
imgs <- fechas %>% left_join(archivos) 
usethis::use_data(imgs, overwrite = TRUE)
setwd("~/Documents/Git/tblEmpresrial")
