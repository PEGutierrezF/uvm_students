




library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(FD)

#Taxa Matrix
sp.matrix <- read_excel('2023 Philip Hampson/data.xlsx', sheet = 'abundance_matrix')
head(sp.matrix,6)

#Traits Matrix
tr.matrix <- read_excel('2023 Philip Hampson/data.xlsx', sheet = 'Traits')
head(tr.matrix,6)

#convertir los numeros enteros en factores (para que R no entienda las categorias como numeros)
tr.matrix.factor <- as.data.frame(apply(tr.matrix[,], 2, as.factor))
tr.matrix.factor

#Anadir nombres a las filas
rownames(tr.matrix.factor) <- rownames(tr.matrix)

tr.dist<-gowdis(tr.matrix.factor)
head(tr.dist)


FD.output <- dbFD(tr.dist, sp.matrix)
FD.output$FRic
