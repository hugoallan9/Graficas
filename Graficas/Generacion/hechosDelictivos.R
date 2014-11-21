source("funciones.R")

g1 <- graficaLinea(tablas$"1_01")
exportarLatex("prueba009",g1)
#preview(g1)

g2 <- graficaCol(tablas$"1_02")
g2 <- etiquetasHorizontales(g2)
g2 <- rotarEtiX(g2)
exportarLatex("prueba109",g2)
#preview(g2)

compilar()
