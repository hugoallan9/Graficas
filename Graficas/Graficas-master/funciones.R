#***************SCRIPT CON FUNCIONES PARA GRAFICAR*********************#
source("setup.R")
source("datos.R")

graficaCol <- function(data, color1=rgb(212,126,41, maxColorValue = 255), ancho = 0.6, ordenar = TRUE)
{
 theme_set(temaColumnas)
 names(data)<- c("x","y")
 data <- data[ordenarNiveles(data, ordenar),]
 data$x <- factor(data$x, levels = data$x)
 View(data)
 grafica <- ggplot(data, aes(x, y))
 grafica <- grafica + 
   geom_bar(stat = 'identity',fill = calcularRampa(data, color1), width = ancho, position =  "dodge")+
   labs(x="",y= "")+
   scale_y_continuous(breaks=NULL, expand= c(0.0,0.0))
 print(grafica)
 return(grafica)
}

graficaBar <- function(data, color1=rgb(212,126,41, maxColorValue = 255), ancho = 0.6, ordenar = TRUE)
{
  theme_set(temaBarras)
  names(data)<- c("x","y")
  data <- data[rev(ordenarNiveles(data, ordenar)),]
  data$x <- factor(data$x, levels = data$x)
  View(data)
  grafica <- ggplot(data, aes(x, y))
  grafica <- grafica + 
    geom_bar(stat = 'identity',fill = calcularRampa(data, color1), width = ancho, position =  "dodge")+
    labs(x=NULL,y=NULL)+
    scale_y_continuous(breaks=NULL, expand= c(0.0,0.0))+
    coord_flip()
  print(grafica)
  return(grafica)
}

rotarEtiX <- function(graph)
{
  longitud <- 2.5 +2.5
  graph <- graph + theme(axis.text.x = element_text(angle = 90, vjust =0.5 , hjust= 1))+
    theme(plot.margin = unit(c(longitud,0,-8,0), "mm"))

}

etiquetasBarras <- function(graph)
{
  max <-ggplot_build(graph)$panel$ranges[[1]]$y.range[2] 
  max <- nchar(as.character(max))
  longitud <- 1.2*max +2.5
  print(max)
  graph <- graph +
    geom_text(aes(family = "Open Sans Condensed Light",label= y), size=3, hjust=-0.5, vjust = 0.5)+
    theme(plot.margin = unit(c(longitud,0,0,-7), "mm"))
}

etiquetasHorizontales <- function(graph)
{
  longitud <- 2.5 +2.5
  graph <- graph +
    geom_text(aes(family = "Open Sans Condensed Light",label= prettyNum(y,digits = 1,big.mark = ",")),size=3, hjust=0.5, vjust = -0.5)+
    theme(plot.margin = unit(c(longitud,0,0,-7), "mm"))
}

etiquetasVerticales <- function(graph)
{
  max <-ggplot_build(graph)$panel$ranges[[1]]$y.range[2] 
  max <- nchar(as.character(max))
  longitud <- 1.2*max +2.5
  print(max)
  graph <- graph +
    geom_text(aes(family = "Open Sans Condensed Light",label= prettyNum(y, big.mark = ",")), angle = 90, size=3, hjust=-0.5, vjust = 0.5)+
    theme(plot.margin = unit(c(longitud,0,0,-7), "mm"))
}

exportarLatex <- function(nombre = grafica, graph)
{
  #gy = ggplot_build(graph)$panel$ranges[[1]]$y.range[2]
  #print(gy)
  #graph = graph + scale_y_continuous(expand=c(0,0),limits=c(0,gy))
  #print(graph)
  tikz(nombre, standAlone = FALSE, bareBones = TRUE, width=3.88, height=2.90, sanitize = TRUE)
  temp<- ggplot_gtable(ggplot_build(graph))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid.draw(temp)
  dev.off()
  shell(cmd=paste("iconv -f ISO-8859-1 -t UTF-8 <", nombre,">", paste(dirname(nombre),"/temp",sep="")), mustWork=TRUE, intern=TRUE, translate=TRUE)
  file.copy(from = paste(dirname(nombre), "/temp",sep=""), to=paste(dirname(nombre),"Latex",basename(nombre),sep="/"), overwrite = TRUE)
}

compilar <- function(ruta = paste(getwd(), "Latex/ENEI.tex",sep="/")){
  shell(cmd=paste("cd", dirname(ruta), "&&xelatex  --synctex=1 --interaction=nonstopmode",ruta), mustWork=TRUE, intern=TRUE, translate=TRUE)
  shell.exec(paste(dirname(ruta), "ENEI.pdf", sep="/"))
}

preview <- function(graph)
{
  nombre = tempfile(pattern="Preview", tmpdir= paste(normalizePath(getwd()),"Temporal", sep="\\"))
  tikz(paste(nombre,".tex", sep= ""), standAlone = TRUE, bareBones = FALSE, width = 3.88, height= 2.74, sanitize= TRUE)
  temp<- ggplot_gtable(ggplot_build(graph))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  grid.draw(temp)
  dev.off()
  nombreTex = paste(nombre,".tex",sep="")
  shell(cmd=paste("iconv -f ISO-8859-1 -t UTF-8 <",nombreTex ,">", paste(dirname(nombre),"/temp",sep="")), mustWork=TRUE, intern=TRUE, translate=TRUE)
  file.copy(from = paste(dirname(nombre), "/temp",sep=""), to=paste(dirname(nombre),basename(nombreTex),sep="/"), overwrite = TRUE)
  shell(cmd=paste("xelatex   --synctex=1 --interaction=nonstopmode", "--output-directory",dirname(nombre),paste(nombre,".tex", sep="")))
  shell.exec(paste(nombre,".pdf", sep=""))
}


calcularRampa <- function(data, color1)
{
  rampa = NULL
  for(elemento in data$x)
  {
    if(elemento %in% ignorado)
    {
      rampa = c(rampa,gris)
    }
    else
    {
      rampa = c(rampa,color1)
    }
  }
  return(rampa)
}

ordenarNiveles <- function(data, ordenar = TRUE)
{
  nuevoOrden <- NULL
  ignNombre <- NULL
  ign = 0
  orden <- NULL
  print(ordenar)
  if(ordenar)
  {
    orden <- order(-data$y)
    
  }
  else
  {
    for(i in (1:length(data$x)))
      orden <- c(orden,i)
  }
    for(elemento in orden)
  {
    if( data[elemento,]$x %in% ignorado)
    {
      ign = 1
      pos <- elemento
    }
    else{
      nuevoOrden <- c(nuevoOrden, elemento)
    }
  }
  if(ign == 1)
  {
    nuevoOrden <- c(nuevoOrden, pos)
  }
  print(nuevoOrden)
  return(nuevoOrden)
}
