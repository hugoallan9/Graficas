#**************************************ALGUNOS DATA FRAMES PARA LAS PRUEBAS**************************************
prueba1 <- read.csv2("2_07.csv", sep = ",")
prueba1[,2]<- as.numeric(levels(prueba1[,2]))[prueba1[,2]]

prueba2 <- read.csv2("prueba2.csv", sep=",")
prueba2[,2]<- as.numeric(prueba2[,2])

t01_01 <- read.csv2("Ejemplos CSV/1_01.csv", sep= ";")
t01_01[,2] <- as.numeric(t01_01[,2])