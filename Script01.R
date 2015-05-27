##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre: Daniel Freire N


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()

data<-read.table("data.txt",header=TRUE,dec=',',sep='\t')
str(data)

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE
edad<-data[,"Edad"]
min(edad,na.rm=TRUE)
mean(edad,na.rm=TRUE)
max(edad,na.rm=TRUE)

# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()
data_f<-subset(data,subset=data[,"Genero"]=="Femenino")
table(data_f[,"Genero"])

# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.
data_dep<-subset(data, data[,"Dependiente"]=="Si")
edad_dep<-data_dep[,"Edad"]
min(edad_dep)
mean(edad_dep)
max(edad_dep)

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()

tipos<-numeric(ncol(data))
for(i in 1:ncol(data)){
  tipos[i]<-typeof(data[,i])
}
tipos


# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()

clase<-numeric(ncol(data))
for(i in 1:ncol(data)){
  clase[i]<-class(data[,i])
}
clase


# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables

n<-length(clase[clase=="factor"])
j<-0
media<- numeric(ncol(data)-n)
nombres<- numeric(ncol(data)-n)
for(i in 1:ncol(data)){
  if(clase[i]!="factor"){
    j=j+1
    media[j]<-mean(data[,i],na.rm=TRUE)
    nombres[j]<-names(data)[i]
  }
}
media<-as.data.frame(t(media))
names(media)<-nombres
media

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()

porcentaje_datos_perdidos<-numeric(ncol(data))
for(i in 1:ncol(data)){
  porcentaje_datos_perdidos[i]<-round(sum(is.na(data[,i]))*100/nrow(data),2) #round() redondea a cierto nuúmero de decimales
}
porcentaje_datos_perdidos<-as.data.frame(porcentaje_datos_perdidos,names(data))
porcentaje_datos_perdidos


# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

data_edad_40<-subset(data,data[,"Edad"] >40)


# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

data_viv_prop<-subset(data,data[,"Vivienda"] =="Propia")


# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

data_cargas_dos<-subset(data,data[,"Cargas"] > 2)

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

data_deuda_dias<- subset(data,data[,"Deuda"] >= 500 & data[,"Dias_Atraso"] > 8 ) 

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

data_3_5<- subset(data,data[,"Score"] >= 900 & data[,"Edad"] <= 35 & data[,"Numero_TC"]>3 )

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(edad,col = "red")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()

boxplot(edad,col = "green")
