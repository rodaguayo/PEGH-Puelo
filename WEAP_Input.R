rm(list=ls())
cat("\014")  

library("exactextractr")
library("terra")
library("sf")

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Elevation_bands.shp")

## Precipitation baseline
PP_CR2MET_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_d.nc")
PP_CR2MET_d <- as.data.frame(t(exact_extract(PP_CR2MET_d, HRUs , 'mean')))
rownames(PP_CR2MET_d) <- seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
colnames(PP_CR2MET_d) <- HRUs$ID_WEAP
PP_CR2MET_d <- round(PP_CR2MET_d,0)
write.csv(PP_CR2MET_d, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/PP_Input.csv")


## Temperature baseline
T2M_CR2MET_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Temperature/T2M_CR2MET_d.nc")
T2M_CR2MET_d <- as.data.frame(t(exact_extract(T2M_CR2MET_d, HRUs , "mean")))
rownames(T2M_CR2MET_d) <- seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
colnames(T2M_CR2MET_d) <- HRUs$ID_WEAP
T2M_CR2MET_d <- round(T2M_CR2MET_d,2)
write.csv(T2M_CR2MET_d, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/T2M_Input.csv")


## Wind baseline
WD_ERA5L_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/WD_ERA5L_d.nc")
WD_ERA5L_d <- as.data.frame(t(exact_extract(WD_ERA5L_d, HRUs , "mean")))
rownames(WD_ERA5L_d) <- seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
colnames(WD_ERA5L_d) <- HRUs$ID_WEAP
WD_ERA5L_d <- round(WD_ERA5L_d,2)
write.csv(WD_ERA5L_d, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/WD_Input.csv")


#Relative humidity baseline
HR_ERA5L_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/RH_ERA5L_d.nc")
HR_ERA5L_d <- as.data.frame(t(exact_extract(HR_ERA5L_d, HRUs, "mean")))
rownames(HR_ERA5L_d) <- seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
colnames(HR_ERA5L_d) <- HRUs$ID_WEAP
HR_ERA5L_d <- round(HR_ERA5L_d,1)
write.csv(HR_ERA5L_d, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/HR_Input.csv")


#Cloud cover
CC_MODIS_m <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/CC_MODIS.nc")
CC_MODIS_m <- as.data.frame(t(exact_extract(CC_MODIS_m, HRUs , "mean")))
colnames(CC_MODIS_m) <- HRUs$ID_WEAP
CC_MODIS_m <- round(CC_MODIS_m,2)
write.csv(CC_MODIS_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/CC_Input.csv")


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

  
  


