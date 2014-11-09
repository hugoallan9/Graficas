#***************SCRIPT CON FUNCIONES PARA GRAFICAR*********************#
source("setup.R")
source("datos.R")

graficaCol <- function(data, color1=rgb(212,126,41, maxColorValue = 255), ancho = 0.6)
{
 theme_set(temaColumnas)
 names(data)<- c("x","y")
 grafica <- ggplot(data, aes(x, y, label = y))
 grafica <- grafica + 
   geom_bar(stat = 'identity',fill = color1, width = ancho, position =  "dodge")+
   labs(x="",y= "")+
   scale_y_continuous(breaks=NULL, expand= c(0.0,0.0))
 print(grafica)
 return(grafica)
}

etiquetasHorizontales <- function(graph)
{
  graph <- graph +
    geom_text(aes(family = "Open Sans Condensed Light",label= y),size=3, hjust=0.5, vjust = -0.5)
}

etiquetasVerticales <- function(graph)
{
  graph <- graph +
    geom_text(aes(family = "Open Sans Condensed Light",label= y), angle = 90, size=3, hjust=0, vjust = 0.5)
}

exportarLatex <- function(nombre = grafica, graph)
{
  gy = ggplot_build(graph)$panel$ranges[[1]]$y.range[2]
  graph = graph + ylim(0,gy)
  print(graph)
  tikz(nombre, standAlone = FALSE, bareBones = TRUE, width=3.88, height=2.71, sanitize = TRUE)
  temp<- ggplot_gtable(ggplot_build(graph))
  temp$layout$clip[temp$layout$name=="panel"] <- "off"
  
  grid.draw(temp)
  dev.off()
  shell(cmd=paste("iconv -f ISO-8859-1 -t UTF-8 <", nombre,">", paste(dirname(nombre),"/temp",sep="")), mustWork=TRUE, intern=TRUE, translate=TRUE)
  file.copy(from = paste(dirname(nombre), "/temp",sep=""), to=paste(dirname(nombre),"/",basename(nombre),sep=""), overwrite = TRUE)
}

compilar <- function(ruta = "C:/Users/INE/Documents/graficas/Latex/ENEI.tex"){
  shell(cmd=paste("cd", dirname(ruta), "&&xelatex  --synctex=1 --interaction=nonstopmode",ruta), mustWork=TRUE, intern=TRUE, translate=TRUE)
  shell.exec(paste(dirname(ruta), "ENEI.pdf", sep="/"))
}


