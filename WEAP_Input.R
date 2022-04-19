rm(list=ls())
cat("\014")  

library("exactextractr")
library("terra")
library("sf")

#Choose one
HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Elevation_bands.shp") #Elevation bands
date<-seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
name<-"Puelo"

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Yelcho.shp") #Elevation bands
date<-seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
name<-"Yelcho"

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Pascua.shp") #Elevation bands
date<-seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
name<-"Pascua"

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Aysen.shp") #Elevation bands
date<-seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
name<-"Aysen"

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Baker.shp") #Elevation bands
date<-seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
name<-"Baker"

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Pcostera.shp") #Elevation bands
date<-seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
name<-"P_costeras"

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Acostera.shp") #Elevation bands
date<-seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
name<-"A_costeras"

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Bcostera.shp") #Elevation bands
date<-seq(from = as.POSIXct("1979-04-01", tz="UTC"), to = as.POSIXct("2020-03-31", tz="UTC"), by = "day")
name<-"B_costeras"

HRUs<-st_read("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_David.shp") #Elevation bands
date2<-seq(from = as.POSIXct("1979-01-01", tz="UTC"), to = as.POSIXct("2020-04-30", tz="UTC"), by = "day")
name<-"David"

## Precipitation baseline
PP_CR2MET_d <- rast("E:/Datasets/CR2MET/CR2MET_pr_v2.0_day_1979_2020_005deg.nc",  subds="pr") # For basins in Central Chile
PP_CR2MET_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_d.nc")
factor<-rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_factor.tif")

factor <- crop(factor, project(vect(HRUs), factor))
PP_CR2MET_d <- crop(PP_CR2MET_d,factor)
PP_CR2MET_d<-PP_CR2MET_d*factor

PP_CR2MET_d <- as.data.frame(t(exact_extract(PP_CR2MET_d, HRUs , 'mean')))
PP_CR2MET_d <- round(PP_CR2MET_d,0)
colnames(PP_CR2MET_d) <- HRUs$ID_WEAP
PP_CR2MET_d <- cbind(date,PP_CR2MET_d)
write.csv(PP_CR2MET_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Inputs/Input_PP_",name,".csv"), row.names = FALSE)


## Temperature baseline (mean)
T2M_CR2MET_d <- rast("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc", subds="t2m") # For basins in Central Chile
T2M_CR2MET_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Temperature/T2M_CR2MET_d.nc")
T2M_CR2MET_d <- as.data.frame(t(exact_extract(T2M_CR2MET_d, HRUs , "mean")))
T2M_CR2MET_d <- round(T2M_CR2MET_d,2)
colnames(T2M_CR2MET_d) <- HRUs$ID_WEAP
T2M_CR2MET_d <- cbind(date, T2M_CR2MET_d)
write.csv(T2M_CR2MET_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Inputs/Input_T2M_",name,".csv"), row.names = FALSE)


## Temperature baseline (max and min)
T2M_CR2MET_d <- rast("E:/Datasets/CR2MET/CR2MET_tmax_v2.0_day_1979_2020_005deg.nc", subds="tmax") # For basins in Central Chile
T2M_CR2MET_d <- as.data.frame(t(exact_extract(T2M_CR2MET_d, HRUs , "mean")))
T2M_CR2MET_d <- round(T2M_CR2MET_d,2)
colnames(T2M_CR2MET_d) <- HRUs$ID_WEAP
T2M_CR2MET_d <- cbind(date2, T2M_CR2MET_d)
write.csv(T2M_CR2MET_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Inputs/Input_T2Mmax_",name,".csv"), row.names = FALSE)

T2M_CR2MET_d <- rast("E:/Datasets/CR2MET/CR2MET_tmin_v2.0_day_1979_2020_005deg.nc", subds="tmin") # For basins in Central Chile
T2M_CR2MET_d <- as.data.frame(t(exact_extract(T2M_CR2MET_d, HRUs , "mean")))
T2M_CR2MET_d <- round(T2M_CR2MET_d,2)
colnames(T2M_CR2MET_d) <- HRUs$ID_WEAP
T2M_CR2MET_d <- cbind(date2, T2M_CR2MET_d)
write.csv(T2M_CR2MET_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Inputs/Input_T2Mmin_",name,".csv"), row.names = FALSE)


## Wind baseline
WD_ERA5L_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/WD_ERA5L_d.nc")
WD_ERA5L_d <- as.data.frame(t(exact_extract(WD_ERA5L_d, HRUs , "mean")))
WD_ERA5L_d <- round(WD_ERA5L_d,2)
colnames(WD_ERA5L_d) <- HRUs$ID_WEAP
WD_ERA5L_d <- cbind(date, WD_ERA5L_d)
write.csv(WD_ERA5L_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Inputs/Input_WD_",name,".csv"), row.names = FALSE)


#Relative humidity baseline
HR_ERA5L_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/RH_ERA5L_d.nc")
HR_ERA5L_d <- as.data.frame(t(exact_extract(HR_ERA5L_d, HRUs, "mean")))
HR_ERA5L_d <- round(HR_ERA5L_d,1)
colnames(HR_ERA5L_d) <- HRUs$ID_WEAP
HR_ERA5L_d <- cbind(date, HR_ERA5L_d)
write.csv(HR_ERA5L_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Inputs/Input_RH_",name,".csv"), row.names = FALSE)


#Cloud cover
library("zoo")

CC_MODIS_m <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/CC_MODIS.nc")
date <- MODIS::extractDate(list.files("E:/Datasets/CLOUD_COVER2/HDF", recursive = FALSE), asDate = TRUE)$inputLayerDates
CC_MODIS_m <- as.data.frame(t(exact_extract(CC_MODIS_m, HRUs , "mean")))
colnames(CC_MODIS_m) <- HRUs$ID_WEAP

#Interpolate daily values
CC_MODIS_m <-zoo(CC_MODIS_m, order.by = date)
CC_MODIS_m <- na.spline(CC_MODIS_m, xout = seq(start(CC_MODIS_m), end(CC_MODIS_m), "day"))
date<- index(CC_MODIS_m)
CC_MODIS_m <- sapply(CC_MODIS_m, FUN = function(x) {smooth.spline(x, spar =0.25)$y})
CC_MODIS_m <- zoo(CC_MODIS_m, order.by = date)
CC_MODIS_m <- as.data.frame(aggregate(CC_MODIS_m, by = strftime(index(CC_MODIS_m),format="%m-%d"), FUN = median))/100
CC_MODIS_m[CC_MODIS_m > 1] <- 1
CC_MODIS_m <- 1- CC_MODIS_m 
CC_MODIS_m <- round(CC_MODIS_m,2)
write.csv(CC_MODIS_m, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Inputs/Input_CC_",name,".csv"), row.names = FALSE)

#Potential evapotranspiration (PET)
PET_ERA5L_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Evapotranspiration/PET_GLEAM.nc")
PET_ERA5L_d <- as.data.frame(t(exact_extract(PET_ERA5L_d, HRUs, "mean")))
PET_ERA5L_d <- round(PET_ERA5L_d,1)
colnames(PET_ERA5L_d) <- HRUs$ID_WEAP
PET_ERA5L_d <- cbind(seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "day"), PET_ERA5L_d)
write.csv(PET_ERA5L_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Inputs/Input_PET_",name,".csv"), row.names = FALSE)

#Evapotranspiration (ET)
ET_ERA5L_d <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Evapotranspiration/ET_GLEAM.nc")
ET_ERA5L_d <- as.data.frame(t(exact_extract(ET_ERA5L_d, HRUs, "mean")))
ET_ERA5L_d <- round(ET_ERA5L_d,1)
colnames(ET_ERA5L_d) <- HRUs$ID_WEAP
ET_ERA5L_d <- cbind(seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "day"), ET_ERA5L_d)
write.csv(ET_ERA5L_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Inputs/Input_ET_",name,".csv"), row.names = FALSE)








