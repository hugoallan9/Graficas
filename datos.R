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

dir <- paste(getwd(),"data prueba", sep = "/")
filenames <- list.files(path = dir, pattern = ".csv", full.names = TRUE)
numfiles <- length(filenames)
for(i in 1:numfiles)
{
  shell(cmd=paste("iconv -f ISO-8859-1 -t UTF-8 <\"",filenames[[i]],"\">", paste(getwd(),"Temporal", basename(filenames[[i]]),sep="/"), sep = ""), mustWork=TRUE, intern=F, translate=TRUE)
}
dir <- paste(getwd(),"Temporal", sep = "/")
filenames <- list.files(path = dir, pattern = ".csv", full.names = TRUE)

All <- lapply(filenames,function(i){
  read.csv(i, sep = ";")
})
filenames <- gsub(dir,"",filenames)
filenames <- gsub("/","", filenames)
names(All) <- gsub(".csv","",filenames)



fact2Num <- function(tabla)
{
  if(is.factor(tabla$y))
  {
    tabla$y<- as.numeric(levels(tabla$y))[tabla$y]    
  }
  else
  {
    tabla$y<- as.numeric(tabla$y)   
  }
  return(tabla)
}

tablas <- lapply(All,fact2Num)
