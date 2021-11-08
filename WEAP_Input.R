rm(list=ls())
cat("\014")  

library("terra")
library("readxl")
library("tools")

HRUs<-vect("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Elevation_bands.shp")

## Precipitation baseline
PP_CR2MET_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_d.nc")
PP_CR2MET_d <- as.data.frame(t(terra::extract(PP_CR2MET_d, HRUs ,method = "bilinear", fun=mean, na.rm=TRUE)))
PP_CR2MET_d <- PP_CR2MET_w[-1, ]
colnames(PP_CR2MET_w) <- HRUs$ID
write.csv(PP_CR2MET_w, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/PP_Input.csv")

## Temperature baseline
T2M_CR2MET <- rast("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc")
T2M_CR2MET <- crop(T2M_CR2MET, HRUs)[[-1]]
T2M_CR2MET <- tapp(T2M_CR2MET, strftime(time(T2M_CR2MET),format="%Y-%W"), fun = mean) 

T2M_CR2MET_w <- as.data.frame(t(terra::extract(T2M_CR2MET, HRUs ,method = "bilinear", fun=mean, na.rm=TRUE)))
T2M_CR2MET_w <- T2M_CR2MET_w[-1, ]
colnames(T2M_CR2MET_w) <- HRUs$ID
write.csv(T2M_CR2MET_w, "PP_Input.csv")

## Wind baseline
WD_ERA5L_u <- rast("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc")
WD_ERA5L_v <- rast("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc")



WD_ERA5L 


WD_ERA5L <- crop(WD_ERA5L, HRUs)[[-1]]
WD_ERA5L <- tapp(WD_ERA5L, strftime(time(WD_ERA5L),format="%Y-%W"), fun = mean) 

WD_ERA5L_w <- as.data.frame(t(terra::extract(WD_ERA5L_w, HRUs ,method = "bilinear", fun=mean, na.rm=TRUE)))

#From 10m to 2m
WD_ERA5L_w <- WD_ERA5L_w*((2/10)^0.25)
WD_ERA5L_w <- (WD_ERA5L_w*5.0849)-2.6095


WD_ERA5L_w <- WD_ERA5L_w[-1, ]
colnames(WD_ERA5L_w) <- HRUs$ID
write.csv(WD_ERA5L_w, "WD_Input.csv")

#Relative humidity baseline
RH_ERA5L <- rast("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc")
RH_ERA5L <- crop(RH_ERA5L, HRUs)[[-1]]
RH_ERA5L <- tapp(RH_ERA5L, strftime(time(RH_ERA5L),format="%Y-%W"), fun = mean) 


temp<-stack("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Humedad Relativa/t2m_ERA5.nc")
tempr<-stack("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Humedad Relativa/t2m_rocio_ERA5.nc")

es_rocio<-6.1094*exp((17.625*tempr)/(243.04+tempr))
es<-6.1094*exp((17.625*temp)/(243.04+temp))
HR<-es_rocio/es

HR<-setZ(HR, as.Date(as.numeric(substring(names(temp), 2)), origin = "1970-01-01"))


RH_ERA5L_w <- as.data.frame(t(terra::extract(RH_ERA5L_w, HRUs ,method = "bilinear", fun=mean, na.rm=TRUE)))

RH_ERA5L_w <- RH_ERA5L_w[-1, ]
colnames(RH_ERA5L_w) <- HRUs$ID
write.csv(RH_ERA5L_w, "RH_Input.csv")

#Cloud cover
T2M_CR2MET <- rast("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc")
T2M_CR2MET <- crop(T2M_CR2MET, HRUs)[[-1]]
T2M_CR2MET <- tapp(T2M_CR2MET, strftime(time(T2M_CR2MET),format="%Y-%W"), fun = mean) 

T2M_CR2MET_w <- as.data.frame(t(terra::extract(T2M_CR2MET, HRUs ,method = "bilinear", fun=mean, na.rm=TRUE)))
T2M_CR2MET_w <- T2M_CR2MET_w[-1, ]
colnames(T2M_CR2MET_w) <- HRUs$ID
write.csv(T2M_CR2MET_w, "PP_Input.csv")

#Cloud cover
CC_MODIS <- list.files("E:/Datasets/CLOUD_COVER/", full.names = TRUE)
CC_MODIS <- rast(CC_MODIS)

CC_MODIS_w <- as.data.frame(t(terra::extract(CC_MODIS, HRUs ,method = "bilinear", fun=mean, na.rm=TRUE)))


CC_MODIS_w <- CC_MODIS_w[-1, ]
colnames(T2M_CR2MET_w) <- HRUs$ID
write.csv(T2M_CR2MET_w, "PP_Input.csv")

## OLD


#Climate projections: WEAP 
subcuenca<-shapefile("C:/Users/Rodrigo/Dropbox/Puelo/SIG/Areas/Subcuencas_Puelo2_elep.shp")
intro<-read.csv2("C:/Users/Rodrigo/Documents/WEAP Areas/Puelo_River_v2/Database/Encabezado.csv", header = FALSE)

pp_base<-stack("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Precipitacion/Datos_Grillados_PP/PP_v3_2000_2019.nc")
pp_base<-setZ(pp_base,seq(as.Date("2000/1/1"), as.Date("2019/5/1"), "month"))
pp_base<-pp_base[[which(getZ(pp_base) >= as.Date("2000/4/1") & getZ(pp_base) <= as.Date("2019/3/31"))]]
pp_lista<-list.files("C:/Users/Rodrigo/Dropbox/Puelo/Proyecciones/Resultados/Precipitacion",pattern = "nc4$", full.names = TRUE)

t2m_base<-stack("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Temperatura/Datos_Grillados_TEMP/Temp_v2_t2m_2000_2019.nc")
t2m_base<-setZ(t2m_base,seq(as.Date("2000/3/1"), as.Date("2019/5/1"), "month"))
t2m_base<-t2m_base[[which(getZ(t2m_base) >= as.Date("2000/4/1") & getZ(t2m_base) <= as.Date("2019/3/31"))]]
t2m_lista<-list.files("C:/Users/Rodrigo/Dropbox/Puelo/Proyecciones/Resultados/Temperatura",pattern = "nc4$", full.names = TRUE)

time<-seq(as.Date('2000/4/1'), as.Date('2070-12-31'), by = 'month')
año<-format(as.Date(time, format="%d-%m-%Y"),"%Y")
mes<-rep(c(4,5,6,7,8,9,10,11,12,1,2,3),71)[1:849]

for(j in 1:90) {
  
  pp_future<-stack(pp_lista[j])
  pp_future<-setZ(pp_future,seq(as.Date("2019/1/1"), as.Date("2070/12/1"), "month"))
  pp_future<-pp_future[[which(getZ(pp_future) >= as.Date("2019/4/1"))]]
  pp_future<-stack(pp_base,pp_future)
  pp_future<-disaggregate(pp_future, 5, method='')
  
  t2m_future<-stack(t2m_lista[j])
  t2m_future<-setZ(t2m_future,seq(as.Date("2019/1/1"), as.Date("2070/12/1"), "month"))
  t2m_future<-t2m_future[[which(getZ(t2m_future) >= as.Date("2019/4/1"))]]
  t2m_future<-stack(t2m_base,t2m_future)
  t2m_future<-disaggregate(t2m_future, 5, method='')
  
  pp_weap<-round(t(extract(pp_future,subcuenca,fun=mean,na.rm=TRUE)),0)
  t2m_weap<-round(t(extract(t2m_future,subcuenca,fun=mean,na.rm=TRUE)),2)
  
  pp_weap<-cbind(mes, pp_weap)
  pp_weap<-cbind(a?o, pp_weap)
  colnames(pp_weap)<- colnames(intro)
  pp_weap<-rbind(intro,pp_weap)
  
  t2m_weap<-cbind(mes, t2m_weap)
  t2m_weap<-cbind(a?o, t2m_weap)
  colnames(t2m_weap)<- colnames(intro)
  t2m_weap<-rbind(intro,t2m_weap)
  
  write.table(t2m_weap,paste0(file_path_sans_ext(basename(t2m_lista[j])),".csv"), sep=";", row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(pp_weap,paste0(file_path_sans_ext(basename(pp_lista[j])),".csv"), sep=";", row.names = FALSE, col.names = FALSE, quote = FALSE)
  print(j)
}

#Climate projections: Puelo mean
cuenca<-shapefile("C:/Users/Rodrigo/Dropbox/Puelo/SIG/Areas/Cuenca_Puelo.shp")
cuenca <- spTransform(cuenca, crs("+proj=longlat +datum=WGS84"))

pp_base<-stack("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Precipitacion/Datos_Grillados_PP/PP_v3_2000_2019.nc")
pp_base<-setZ(pp_base,seq(as.Date("2000/1/1"), as.Date("2019/5/1"), "month"))
pp_base<-pp_base[[which(getZ(pp_base) >= as.Date("2000/4/1") & getZ(pp_base) <= as.Date("2019/3/31"))]]
pp_lista<-list.files("C:/Users/Rodrigo/Dropbox/Puelo/Proyecciones/Resultados/Precipitacion",pattern = "nc4$", full.names = TRUE)

t2m_base<-stack("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Temperatura/Datos_Grillados_TEMP/Temp_v2_t2m_2000_2019.nc")
t2m_base<-setZ(t2m_base,seq(as.Date("2000/3/1"), as.Date("2019/5/1"), "month"))
t2m_base<-t2m_base[[which(getZ(t2m_base) >= as.Date("2000/4/1") & getZ(t2m_base) <= as.Date("2019/3/31"))]]
t2m_lista<-list.files("C:/Users/Rodrigo/Dropbox/Puelo/Proyecciones/Resultados/Temperatura",pattern = "nc4$", full.names = TRUE)

pp_results<-matrix(0,90,849)
t2m_results<-matrix(0,90,849)
rownames(pp_results)<-basename(pp_lista)
rownames(t2m_results)<-basename(t2m_lista)

for(j in 1:90) {
  pp_future<-stack(pp_lista[j])
  pp_future<-setZ(pp_future,seq(as.Date("2019/1/1"), as.Date("2070/12/1"), "month"))
  pp_future<-pp_future[[which(getZ(pp_future) >= as.Date("2019/4/1"))]]
  pp_future<-stack(pp_base,pp_future)

  t2m_future<-stack(t2m_lista[j])
  t2m_future<-setZ(t2m_future,seq(as.Date("2019/1/1"), as.Date("2070/12/1"), "month"))
  t2m_future<-t2m_future[[which(getZ(t2m_future) >= as.Date("2019/4/1"))]]
  t2m_future<-stack(t2m_base,t2m_future)

  pp_results[j,]<-round(t(extract(pp_future,cuenca,fun=mean,na.rm=TRUE)),0)
  t2m_results[j,]<-round(t(extract(t2m_future,cuenca,fun=mean,na.rm=TRUE)),2)
  print(j)
}

write.xlsx(pp_results, "pp_2040_2070.xlsx")
write.xlsx(t2m_results, "t2m_2040_2070.xlsx")

  
  


