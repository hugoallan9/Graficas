archivo <- file.choose("C:/Users/INE/Documents/")
separador = ";"
info <- read.csv2(archivo, sep = separador)

names(info) <- c("Corr", "Titulo", "Descripción", "TGrafica", "desagraga", "Fuente", "Nota")


