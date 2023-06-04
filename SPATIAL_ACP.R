##
#--------------------------------------------------------------------------
#------- CÓDIGO FUSION DE IMAGENES SATELITALES-----------------------------
#------- Tarea 3: proyecto Vision Artificial 1-----------------------------
#------- Por: María Camila Velandia     mariac.velandiag@udea.edu.co-------
#-------      Estudiante de Maestría en ingeniería ------------------------
#-------      CC 1017252095,  Wpp 311 7318160------------------------------
#------- Curso Básico de Procesamiento de Imágenes y Visión Artificial-----
#------- 08 junio 2023-----------------------------------------------------
#--------------------------------------------------------------------------


#----Se borra todos los objetos almacenados en la memoria del usuario------
rm(list=ls())

#---- Directorio de trabajo, modificar segun su directorio de trabajo.
#---- (Donde tenga guardadas las imagenes satelitales extraidas de Google Earth Engine)
setwd("C:/Users/MARIA CAMILA/OneDrive - Universidad de Antioquia/Visión Artificial 1/Tarea_2")

#--------------------------------------------------------------------------
#---- Cargar librerias necesarias------------------------------------------
#--------------------------------------------------------------------------
  
library(sp)         # Manipulación de datos espaciales
library(rgdal)      # Operaciones con datos Geoespaciales
library(raster)     # Creación de archivos raster
library(dismo)      # Modelos de distribución de especies
library(maptools)   # Combinar datos espaciales
library(usdm)       #
library(ade4)
library(ape)        # Análisis de filogenética y evolución
library(ecospat)    # Métodos en ecología espacial
library(gbm)
    

#---- CARGA DE CAPAS (tantas como se quieran. Deben de estar reescaladas)---

NDBI <- raster("rNDBI.tif")
plot(NDBI)
SLAVI <- raster("rSLAVI.tif")
plot(SLAVI)
HUMEDAD <- raster("rTASS.tif")
plot(HUMEDAD)
#---- FUSION DE TODAS LAS CAPAS, INDICES SATELITALES------------------------
ALL<-stack(NDBI, HUMEDAD,SLAVI) 
plot(ALL)

#----LISIS DE CORRELACIÓN---------------------------------------------------
library(ecospat)
ecospat.cor.plot(ALL[ ,1:3])
vifstep(ALL[[1:3]])
vifcor(ALL, th=.3)

####
#----RASTER PCA------------------------------------------------------------
library(RStoolbox)
pca<-rasterPCA(ALL , nSamples = NULL, nComp = nlayers(ALL), spca = FALSE, maskCheck = TRUE)
pca
summary(pca$model)
knitr::kable(round(pca$model$loadings[, 1:3],3))
plot(pca$map,1, legend=T)
plot(pca$map,2, legend=T)
plot(pca$map,3, legend=F)
pca$model$loadings
#----Convertir a raster-----------------------------------------------------
pca1<-raster(pca$map,1)
pca2<-raster(pca$map,2)
pca3<-raster(pca$map,3)
pcas<-stack(pca1,pca2, pca3)

plot(pcas)

#----Imprimir raster de componentes principales------------------------------
conjunto<-ggRGB(pca$map,1,2, stretch="lin", q=0)
pca12<-as.raster(conjunto)

if(require(gridExtra)){
  plots <- lapply(1:4, function(x) ggR(pca$map, x, geom_raster = TRUE))
  grid.arrange(plots[[1]],plots[[2]], plots[[3]], plots[[4]], ncol=2)}

writeRaster(pca1,"pca1.tif")
writeRaster(pca2,"pca2.tif")
#--------------------------------------------------------------------------
#---------------------------  FIN DEL PROGRAMA ----------------------------
#--------------------------------------------------------------------------
