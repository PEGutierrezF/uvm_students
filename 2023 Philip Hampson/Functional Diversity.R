




library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(FD)

##### Spp Matrix #####
sp.matrix <- read.csv("spp.csv", row.names= 1)
head(sp.matrix)

#Traits Matrix
tr.matrix<-read.csv("traits.csv", row.names=1)
head(tr.matrix)


#Treatment Matrix
treatment.matrix <- read.csv("treatment.csv", row.names=1)
head(treatment.matrix) 


#convertir los numeros enteros en factores (para que R no entienda las categorias como numeros)
tr.matrix.factor<-as.data.frame(apply(tr.matrix[,], 2, as.factor))
tr.matrix.factor

#Anadir nombres a las filas
rownames(tr.matrix.factor) <- rownames(tr.matrix)

#Crear una matriz de distancias usando la metrica de desemejanza de Gower (u otra)
#(usualmente usamos la distancia euclidea para rasgos continuos)
tr.dist<-gowdis(tr.matrix.factor)
head(tr.dist)

FD.output<-dbFD(tr.dist, sp.matrix)
FD.output$FRic





