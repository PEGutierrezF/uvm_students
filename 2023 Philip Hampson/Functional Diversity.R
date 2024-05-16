




library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(FD)

##### Spp Matrix #####
sp.matrix <- read.csv("functional analysis/Bug_abundance_sorted.csv",  row.names=1)
head(sp.matrix)
#Traits Matrix
tr.matrix <- read_xlsx("functional analysis/traits.xlsx")
head(tr.matrix)
#Treatment Matrix
treatment.matrix <- read.csv("functional analysis/treatment.csv",  row.names=1)
head(treatment.matrix) 


#convertir los numeros enteros en factores (para que R no entienda las categorias como numeros)
tr.matrix.factor <- as.data.frame(apply(tr.matrix[,], 2, as.factor))


#Anadir nombres a las filas
rownames(tr.matrix.factor) <- rownames(tr.matrix)

#Crear una matriz de distancias usando la metrica de desemejanza de Gower (u otra)
#(usualmente usamos la distancia euclidea para rasgos continuos)
tr.dist <- gowdis(tr.matrix.factor)
head(tr.dist)

FD.output <- dbFD(tr.dist, sp.matrix)
FD.output$FRic

