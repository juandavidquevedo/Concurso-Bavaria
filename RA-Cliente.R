####### Importar las bases de datos #####
####### Importar las bases de datos #####
setwd("C:\\Users\\jdqg0\\Desktop\\Bavaria")
#paquetes
rm(list=ls())
library(tidyr)
library(arules)
library(dplyr)
library(readr)
library(rvest)


#1. Informacion de los clientes 

infoClientesEstr <- read_delim("Input1_clientes_estructura.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#2. Informacion de los pedidos 
infoClientesVent <- read_delim("Input2_clientes_venta.csv", ";", escape_double = FALSE, trim_ws = TRUE)

#3. Salida(Clientes que piden)
pidenClientes <- read_delim("Input3_clientes_test.csv", ";", escape_double = FALSE, trim_ws = TRUE)

##
infoClientes <- read_csv("juliana/infoClientes.csv", col_names = FALSE)


a <- names(infoClientesVent)
names(infoClientes)=a

####### Filtrar y organizar los datos #####
#Dejar solo los clientes que necesitamos 
IdCliente <- c()
IdCliente <- pidenClientes$Cliente
IdCliente <- as.vector(IdCliente)

Matriz<-unite(infoClientes, AñoMes,c(1:2),  sep = ";", remove = TRUE)

FClientes <- Matriz %>% filter(Cliente %in% IdCliente)

####Reglas de asociacion por cliente 

tabla<-c()
for (j in 1:length(pidenClientes$Cliente)) {
#j=1
client10 <- FClientes %>% filter(Cliente %in% pidenClientes$Cliente[j])

##Hacer una base donde sea IDCliente-Marca1--Marca2--....
#Calcular cuanto es el que más se repite
cant<- as.data.frame(table(client10$AñoMes))
cantmax <- order(cant$Freq, decreasing = TRUE)[1]
FrecMax <- cant$Freq[cantmax]
#Preparar datos
SoloId <- NULL
SoloId$AñoMes <- client10$AñoMes
SoloId <- as.data.frame(SoloId)
SoloId$Marca2 <- client10$Marca2
SoloId <- SoloId[order(SoloId$AñoMes),]
nombres <- c()
for (i in 1:FrecMax){
  nombres <- c(nombres, i)
}
nombres <- as.character(nombres)

Matriz <- aggregate(SoloId$Marca2, list(SoloId$AñoMes), paste, collapse= "," )
Matriz <- separate(Matriz,2,nombres,sep = ",")
#Escribe la matriz que se necesita para reglas de asociacion 
write.table(Matriz, file="MatrizReglasAsociacion.csv", row.names=FALSE, col.names=FALSE, sep=",")


######## INICIO ALGORITMO ########

#1. Cargar la matriz para reglas de asociacion 
MatrizReglasAsociacion <- read_csv("MatrizReglasAsociacion.csv", 
                                   col_names = FALSE)
#2. Hacer las transacciones 
transaction <- read.transactions(file = "MatrizReglasAsociacion.csv",
                                 header = FALSE,
                                 format = "basket", 
                                 sep = ",",
                                 cols = 1,  
                                 rm.duplicates = TRUE)
#inspect(transaction)




#4.Hallar las frecuencias  
#Frecuencia ABSOLUTA 
frecuencia_items <- itemFrequency(x = transaction, type = "absolute")
ordfrecuencia <- c(frecuencia_items %>% sort(decreasing = TRUE))
nombres<- names(ordfrecuencia)
#ordfrecuencia %>% head(10)

#Frecuencia RELATIVA == SOPORTE 
frecuencia_item <- itemFrequency(x = transaction, type = "relative")
frecuencia_item <- as.vector(frecuencia_item)
ordfrecuencia_item <- c(frecuencia_item %>% sort(decreasing = TRUE))
names(ordfrecuencia_item)<- nombres
#ordfrecuencia_item %>% head(10)
for (k in c(1:length(names(ordfrecuencia_item)))) {
  #k=6
  if(names(ordfrecuencia_item)[k]=="Marca_A"){
    pidenClientes$Marca1[j]=ordfrecuencia_item[k]
  }
  if(names(ordfrecuencia_item)[k]=="Marca_B"){
    pidenClientes$Marca2[j]=ordfrecuencia_item[k]
  }
  if(names(ordfrecuencia_item)[k]=="Marca_C"){
    pidenClientes$Marca3[j]=ordfrecuencia_item[k]
  }
  
  if(names(ordfrecuencia_item)[k]=="Marca_D"){
    pidenClientes$Marca_Inno1[j]=ordfrecuencia_item[k]
  }

  if(names(ordfrecuencia_item)[k]=="Marca_E"){
    pidenClientes$Marca_Inno2[j]=ordfrecuencia_item[k]
  }
}
print(j)
print(j)
print(j)
print(j)
print(j)

print(j)
print(j)
print(j)
print(j)
print(j)
print(j)
print(j)

print(j)
}
head(pidenClientes)


write.table(pidenClientes, file = "rezar.csv", sep = ",")

