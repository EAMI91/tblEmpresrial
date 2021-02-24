## code to prepare `imgs` dataset goes here
library(pdftools)
imgs <- pdftools::pdf_convert('inst/Pres_Cdatos.pdf',format = 'png',verbose = FALSE)
imgs <- paste("inst",imgs,sep = "/")
usethis::use_data(imgs, overwrite = TRUE)
