#*************************SCRIPT PARA FUNCIONES PRELIMINARES*******************#

#**************************FUNCIONES PREVIAS***********************************#
cargarPaquete <- function(package){
  if(package %in% installed.packages()[,"Package"])
  {
    cat(c("El paquete ", package, " ha sido cargado con éxito\n\n"))
    require(package, character.only =  TRUE)
  }
  else
  {
    install.packages(as.character(package))
  }
}

cargarPaquete("ggplot2")
cargarPaquete("tikzDevice")
cargarPaquete("foreign")
cargarPaquete("plyr")
cargarPaquete("dataframes2xls")
cargarPaquete("tikzDevice")
cargarPaquete("grid")
cargarPaquete("extrafont")
cargarPaquete("gridExtra")
cargarPaquete("tools")

#**************************CREACION DE TEMAS***********************************#



gris <- rgb(200,200,200, maxColorValue = 255)
grisBase <- rgb(152,152,152, maxColorValue = 255)
fontSize = 10
temaINE <- theme_gray(base_size = fontSize, base_family = "Open Sans Condensed Light")+ theme(
  text= element_text(family = "Open Sans Condensed Light", face = "plain", colour='black', size = fontSize),
  axis.text.x = element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = 10, hjust = 0.5, vjust =0.5, angle = 0, lineheight = 0.9),
  axis.text.y = element_text(family = "Open Sans Condensed Light", colour = "black", face = "plain", size = 10, hjust = 0.5, vjust =0.5, angle = 0, lineheight = 0.9),
  panel.background = element_rect(fill = NA),
  panel.grid = element_line(colour = NA),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_line(colour = NA),
  panel.grid.major.y =  element_line(colour = NA),
  axis.line = element_line(colour = grisBase),
  plot.margin = rep(unit(0,"null"),4),
  axis.ticks = element_line(colour = NA),
  axis.ticks.x = element_line( size=NULL, color=NA ),
  axis.ticks.y = element_line(size = NULL, color=NA),
  plot.background = element_rect(fill = NA)
)

temaBarras <- temaINE
temaBarras <- temaBarras + theme(
  axis.line.y = element_line(colour = grisBase),
  axis.line.x = element_line(colour = NA),
  axis.text.y = element_text(hjust = 1, vjust = 0.5)
  ) 

temaColumnas <- temaINE
temaColumnas <- temaBarras + theme(
  axis.line.x = element_line(colour = grisBase),
  axis.line.y = element_line(colour = NA),
  axis.text.y = element_text(colour = NA)
  )



#*******************************CONFIGURACION PARA TIKZDEVICE*********************#
options(tikzDefaultEngine = "xetex")
#options(tikzXelatex = "/usr/local/texlive/2014/bin/x86_64-linux/xelatex")
options(tikzXelatexPackages = c("\\usepackage{tikz}\n",
                                "\\usepackage[active,tightpage,xetex]{preview}\n",
                                "\\usepackage{fontspec,xunicode}\n",
                                "\\PreviewEnvironment{pgfpicture}\n",
                                "\\setlength\\PreviewBorder{0pt}\n",
                                "\\setmainfont{Open Sans Condensed Light}\n",
                                "\\newfontfamily\\Bold{Open Sans Condensed Bold}",
                                "\\newfontfamily\\Sans{Open Sans}",
                                "\\newfontfamily\\Italic{Open Sans Condensed Light Italic}"))

options(tikzUnicodeMetricPackages = c("\\usepackage[T1]{fontenc}\n","\\usetikzlibrary{calc}\n",
                                      "\\usepackage{fontspec,xunicode}\n", 
                                      "\\setmainfont{Open Sans Condensed Light}"))

options(tikzDocumentDeclaration= "\\documentclass[12pt]{book}")


#***********************ALGUNAS VARIABLES UTILES*******************#
repu <- c("Total República, Total republica, Total república, Total Republica")
ignorado <- c("Ignorado", "ignorado", "IGNORADO", "otros", "Otro", "Otros", "OTROS", "Ignorada", "ignorada")
pts2mm <- 0.352777778
color <- rgb(219,174,109, maxColorValue = 255)
color2 <- rgb(0.603921568627451,0.6274509803921569,0.4862745098039216)
izquierdo <- -5
abajo <- -8

