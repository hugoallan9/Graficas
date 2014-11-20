#**************************************ALGUNOS DATA FRAMES PARA LAS PRUEBAS**************************************
prueba1 <- read.csv2("2_07.csv", sep = ",")
prueba1[,2]<- as.numeric(levels(prueba1[,2]))[prueba1[,2]]

prueba2 <- read.csv2("prueba2.csv", sep=",")
prueba2[,2]<- as.numeric(prueba2[,2])

t01_01 <- read.csv2("Ejemplos CSV/1_01.csv", sep= ";")
t01_01[,2] <- as.numeric(t01_01[,2])

t01_02 <- read.csv2("Ejemplos CSV/1_02.csv", sep= ";")
t01_02[,2] <- as.numeric(t01_02[,2])

t01_04 <- read.csv2("Ejemplos CSV/1_04.csv", sep= ";")
t01_04[,2] <- as.numeric(levels(t01_04[,2]))[t01_04[,2]]

dir <- paste(getwd(),"Ejemplos CSV", sep = "/")
filenames <- list.files(path = dir, pattern = ".csv", full.names = TRUE)
numfiles <- length(filenames)

All <- lapply(filenames,function(i){
  read.csv(i, sep = ";")
})
filenames <- gsub(dir,"",filenames)
filenames <- gsub("/","", filenames)
names(All) <- gsub(".csv","",filenames)