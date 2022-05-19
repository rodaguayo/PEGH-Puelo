rm(list=ls())
cat("\014")  

library("exactextractr")
library("terra")
library("sf")

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/comunas.shp") #Elevation bands 


## Precipitation baseline
PP_CR2MET  <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_mean.tif")
ET_GLEAM   <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Evapotranspiration/ET_GLEAM_mean.tif")

PP_ET_comuna<- as.data.frame(exact_extract(PP_CR2MET, HRUs , 'mean'))
PP_ET_comuna<- cbind(PP_ET_comuna,as.data.frame(exact_extract(ET_GLEAM, HRUs , 'mean')))
PP_ET_comuna <- round(PP_ET_comuna,0)

colnames(PP_ET_comuna) <- c("PP_mm","ET_mm")
rownames(PP_ET_comuna) <- HRUs$Comuna
PP_ET_comuna$PP_hm3<- PP_ET_comuna$PP*0.001*as.numeric(st_area(HRUs))*(10^-6)
PP_ET_comuna$ET_hm3<- PP_ET_comuna$ET*0.001*as.numeric(st_area(HRUs))*(10^-6)
write.csv(PP_ET_comuna, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/comunas.csv", row.names = TRUE)

