#***************SCRIPT CON FUNCIONES PARA GRAFICAR*********************#
source("setup.R")
source("datos.R")




graficaCol <- function(data, color1=color, ancho = 0.6, ordenar = TRUE)
{
 theme_set(temaColumnas)
 names(data)<- c("x","y")
 data <- data[ordenarNiveles(data, ordenar),]
 data$x <- factor(data$x, levels = data$x)
 View(data)
 grafica <- ggplot(data, aes(x, y))
 grafica <- grafica + 
   geom_bar(stat = 'identity',fill = calcularRampa(data, color1), width = ancho, position =  "dodge")+
   labs(x="",y="")+
   scale_y_continuous(breaks=NULL, expand= c(0.0,0.0))
 print(grafica)
 return(grafica)
}

graficaBar <- function(data, color1=color, ancho = 0.6, ordenar = TRUE)
{
  theme_set(temaBarras)
  names(data)<- c("x","y")
  data <- data[rev(ordenarNiveles(data, ordenar)),]
  data$x <- factor(data$x, levels = data$x)
  View(data)
  grafica <- ggplot(data, aes(x, y))
  grafica <- grafica + 
    geom_bar(stat = 'identity',fill = calcularRampa(data, color1), width = ancho, position =  "dodge")+
    labs(x="",y="")+
    scale_y_continuous(breaks=NULL, expand= c(0.0,0.0))+
    coord_flip()
  print(grafica)
  return(grafica)
}

graficaLinea <- function(data,color1 = color, ancho = 1.7)
{
 theme_set(temaColumnas)
 longitud <- 2.5
 names(data)<- c("x","y")
 minimo <- min(data$y)
 maximo <- max(data$y)
 cota  <- 0.3*(maximo-minimo)
 minimo <- minimo -cota
 print(minimo)
 grafica <- ggplot(data, aes(x,y))
 grafica <- grafica + geom_line(colour = color1, size = ancho)+
   labs(x=NULL,y=NULL)+
   scale_y_continuous(limits = c(minimo,NA))+
   theme(plot.margin = unit(c(longitud,0,0,izquierda), "mm"))
 grafica <- etiquetasLineas(grafica, calcularPosiciones(grafica))
 return(grafica)
}

calcularPosiciones <- function(graph)
{
  #SIMBOLOGIA
  # 1 HACIA ARRIBA
  #-1 HACIA ABAJO
  #0.5 A LA DERECHA
  #-0.5 A LA IZQUIERDA
  data <- ggplot_build(graph)$data[[1]]
  print(data)
  posiciones <- NULL
  if(data$y[[1]] < data$y[[2]])
  {
    posiciones <- c(posiciones, -1)
  }
  else{
    posiciones <- c(posiciones, 1)  
  }
  
  
  for(i in 2:(length(data$y)-1))
  {
    if(data$y[[i-1]] == data$y[[i]])
    {
      if(data$y[[i+1]] > data$y[[i]])
      {
        posiciones <- c(posiciones, -1)
      }else if(data$y[[i+1]] == data$y[[i]])
      {
        posiciones <- c(posiciones, 1)
      }else if(data$y[[i+1]] < data$y[[i]]){
        posiciones <- c(posiciones, 1)
      }
    }else if(data$y[[i-1]] > data$y[[i]])
    {
      if(data$y[[i]] > data$y[[i+1]])
      {
        posiciones <- c(posiciones, 0.5)
      }
      else{
        posiciones <- c(posiciones, -1)
      }
    }else
    {
      if(data$y[[i]] < data$y[[i+1]])
      {
        posiciones <- c(posiciones, -0.5)
      }
      else
      {
        posiciones <- c(posiciones, 1)
      }
    }
  }
  if(data$y[[length(data$y)]] == data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, 1)
  }else if(data$y[[length(data$y)]] < data$y[[length(data$y)-1]])
  {
    posiciones <- c(posiciones, -1)
  }else
  {
    posiciones <- c(posiciones, 1)
  }
  print(posiciones)
  return(posiciones)
}


etiquetasLineas <- function(graph, posiciones)
{
  d <- ggplot_build(graph)$data[[1]]
  for(i in 1:length(posiciones))
  {
    dato <- d$y[[i]]
    cat(c("El dato es: ", dato, "\n"))
    print(i)
    d$etiqueta <-ifelse(is.na(as.numeric(completarEtiquetas(dato,i))), "", formatC(as.numeric(completarEtiquetas(dato,i)), format = "f", digits = 1, big.mark=",")) 
    print(d)
    if(posiciones[[i]] == 1)
    {
      graph <- graph + geom_text(data = d, aes(label=etiqueta,family="Open Sans Condensed Light"),size=3.2,hjust = 0.5, vjust = -0.5)
    }else if(posiciones[[i]] == -1)
    {
      graph <- graph + geom_text(data = d,aes(label=formatC(etiqueta, format = "f", digits = 1, big.mark=","),family="Open Sans Condensed Light"),size=3.2,hjust = 0.5, vjust = 1.5)
    }else if(posiciones[[i]] == 0.5)
    {
      graph <- graph + geom_text(data =d,aes(label=formatC(etiqueta, format = "f", digits = 1, big.mark=","),family="Open Sans Condensed Light"),size=3.2,hjust = 0, vjust = -0.5)
    }
    else
    {
      graph <- graph + geom_text(data = d,aes(label=formatC(etiqueta, format = "f", digits = 1, big.mark=","),family="Open Sans Condensed Light"),size=3.2,hjust = 1.2, vjust = 0.5)
    }
  }
  print(graph)
  return(graph)
}

completarEtiquetas <- function(dato,posicion, tam = 5)
{
  #cat(c("la posicion es: ", posicion))
  #cat(c("el dato es: ", dato))
  etiquetas <- NULL
  for(i in 1:tam)
  {
    if(i == posicion)
    {
      print("Entre al if")
      etiquetas <- c(etiquetas, dato)
    }
    else
    {
      print("Entre al else")
      etiquetas <- c(etiquetas,"")  
    }
  }
  return(etiquetas)
}

rotarEtiX <- function(graph)
{
  longitud <- 2.5 +2.5
  graph <- graph + theme(axis.text.x = element_text(angle = 90, vjust =0.5 , hjust= 1))+
    theme(plot.margin = unit(c(longitud,0,abajo,izquierda), "mm"))

}

etiquetasBarras <- function(graph)
{
  max <-ggplot_build(graph)$panel$ranges[[1]]$x.range[2] 
  print(max)
  max <- nchar(as.character(max))
  longitud <- 1.2*max +3.6
  print(max)
  graph <- graph +
    geom_text(aes(family = "Open Sans Condensed Light",label= y), size=3, hjust=-0.5, vjust = 0.5)+
    theme(plot.margin = unit(c(0,longitud,0,0), "mm"))
}

etiquetasHorizontales <- function(graph)
{
  longitud <- 2.5 +2.5
  graph <- graph +
    geom_text(aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f",digits = 1,big.mark = ",")),size=3, hjust=0.5, vjust = -0.5)+
    theme(plot.margin = unit(c(longitud,0,0,-5), "mm"))
}

etiquetasVerticales <- function(graph)
{
  max <-ggplot_build(graph)$panel$ranges[[1]]$y.range[2] 
  max <- nchar(as.character(formatC(max,format = "f", digits = 1, big.mark=",")))
  longitud <- 1.2*max +3.6
  print(max)
  graph <- graph +
    geom_text(aes(family = "Open Sans Condensed Light",label= formatC(y,format = "f", digits =1, big.mark = ",")), angle = 90, size=3, hjust=-0.5, vjust = 0.5)+
    theme(plot.margin = unit(c(longitud,0,0,-5), "mm"))
}

exportarLatex <- function(nombre = grafica, graph)
{
  #gy = ggplot_build(graph)$panel$ranges[[1]]$y.range[2]
  #print(gy)
  #graph = graph + scale_y_continuous(expand=c(0,0),limits=c(0,gy))
  #print(graph)
  tikz(nombre, standAlone = FALSE, bareBones = TRUE, width=3.88, height=2.71, sanitize = TRUE)
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

existeTraslape <- function(graph,ancho = 0.6)
{
  ejeX <- 99.1 *0.0393700787
  etiquetas <- ggplot_build(graph)$panel$ranges[[1]]$x.labels
  tam <- list()
  for(i in 1:length(etiquetas))
  {
    tam[i] <- getLatexStrWidth(etiquetas[[i]], cex = ancho)   
  }
  lapply(tam, pt2mm)
  nuBarras <- length(etiquetas)
  semiEspacio <- 99.1/(2*nuBarras)
  
}

pt2mm <- function(unidad)
{
  return (unidad*pts2mm)
}
