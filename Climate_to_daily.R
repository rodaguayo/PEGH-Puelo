rm(list=ls())
cat("\014")  

library("terra")
library("tools")

HRUs<-vect("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Elevation_bands.shp") #Default for Puelo 
HRUs<-ext(c(-73.6,-70.8,-49.4,-41.1))

## Precipitation baseline
PP_CR2MET <- rast("E:/Datasets/CR2MET/CR2MET_pr_v2.0_day_1979_2020_005deg.nc")
PP_CR2MET <- crop(PP_CR2MET,HRUs)
PP_CR2MET <- subset(PP_CR2MET, which(time(PP_CR2MET) > '1979-03-31' & (time(PP_CR2MET) <= '2020-03-31')))
PP_CR2MET_m <- tapp(PP_CR2MET, strftime(time(PP_CR2MET),format="%Y"), fun = sum)
PP_CR2MET_m <- mean(PP_CR2MET_m[[-c(1,nlyr(PP_CR2MET_m))]]) #Exclude first and last

writeRaster(PP_CR2MET_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_mean.tif", datatype="INT4S", overwrite = TRUE)
writeCDF(PP_CR2MET, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_d.nc",  
         prec="integer", overwrite=TRUE, varname="pr", unit="mm", longname="precipitation", zname="time", compression = 9)


## Temperature baseline
T2M_CR2MET <- rast("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc")
T2M_CR2MET <- crop(T2M_CR2MET, HRUs)
T2M_CR2MET <- subset(T2M_CR2MET, which(time(T2M_CR2MET) > '1979-03-31' & (time(T2M_CR2MET) <= '2020-03-31')))
T2M_CR2MET_m <- tapp(T2M_CR2MET, strftime(time(T2M_CR2MET),format="%Y"), fun = mean)
T2M_CR2MET_m <- mean(T2M_CR2MET_m[[-c(1,nlyr(T2M_CR2MET_m))]]) #Exclude first and last

writeRaster(T2M_CR2MET_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Temperature/T2M_CR2MET_mean.tif", overwrite = TRUE)
writeCDF(T2M_CR2MET, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Temperature/T2M_CR2MET_d.nc",
         overwrite=TRUE, varname="t2m", unit="degC", longname="temperature", zname="time", compression = 9)


## Wind speed baseline
WD_ERA5L_u00 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_00hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u03 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_03hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u06 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_06hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u09 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_09hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u12 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_12hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u15 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_15hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u18 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_18hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u21 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_21hrs.nc", subds="u10"), HRUs)

WD_ERA5L_v00 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_00hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v03 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_03hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v06 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_06hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v09 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_09hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v12 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_12hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v15 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_15hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v18 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_18hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v21 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_21hrs.nc", subds="v10"), HRUs)

factor      <- (2/10)^0.25 #Correction from 10m to 2m
WD_ERA5L_00 <- sqrt(WD_ERA5L_u00^2+WD_ERA5L_v00^2)*factor
WD_ERA5L_03 <- sqrt(WD_ERA5L_u00^2+WD_ERA5L_v00^2)*factor
WD_ERA5L_06 <- sqrt(WD_ERA5L_u00^2+WD_ERA5L_v00^2)*factor
WD_ERA5L_09 <- sqrt(WD_ERA5L_u00^2+WD_ERA5L_v00^2)*factor
WD_ERA5L_12 <- sqrt(WD_ERA5L_u00^2+WD_ERA5L_v00^2)*factor
WD_ERA5L_15 <- sqrt(WD_ERA5L_u00^2+WD_ERA5L_v00^2)*factor
WD_ERA5L_18 <- sqrt(WD_ERA5L_u00^2+WD_ERA5L_v00^2)*factor
WD_ERA5L_21 <- sqrt(WD_ERA5L_u00^2+WD_ERA5L_v00^2)*factor

WD_ERA5L <- c(WD_ERA5L_00, WD_ERA5L_03, WD_ERA5L_06, WD_ERA5L_09, WD_ERA5L_12, WD_ERA5L_15, WD_ERA5L_18, WD_ERA5L_21)
WD_ERA5L_d <- tapp(WD_ERA5L, strftime(time(WD_ERA5L),format="%Y-%m-%d"), fun = mean)
time(WD_ERA5L_d) <- seq(from = as.POSIXct("1979-01-01", tz="UTC"), to = as.POSIXct("2021-09-01", tz="UTC"), by = "day")
WD_ERA5L_d <- subset(WD_ERA5L_d, which(time(WD_ERA5L_d) > '1979-03-31' & (time(WD_ERA5L_d) <= '2020-03-31')))

WD_ERA5L_m <- tapp(WD_ERA5L_d, strftime(time(WD_ERA5L_d),format="%Y"), fun = mean)
WD_ERA5L_m <- mean(WD_ERA5L_m[[-c(1,nlyr(WD_ERA5L_m))]]) #Exclude first and last

writeRaster(WD_ERA5L_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/WD_ERA5L_mean.tif", overwrite = TRUE)
writeCDF(WD_ERA5L_d, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/WD_ERA5L_d.nc",
         overwrite=TRUE, varname="wd", unit="m/s", longname="Wind speed", zname="time", compression = 9)

## Relative humidity baseline
T2M_ERA5L_00 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_00hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_03 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_03hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_06 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_06hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_09 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_09hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_12 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_12hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_15 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_15hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_18 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_18hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_21 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_21hrs.nc", subds="t2m"), HRUs)-273.15

T2Mr_ERA5L_00 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_00hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_03 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_03hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_06 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_06hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_09 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_09hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_12 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_12hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_15 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_15hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_18 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_18hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_21 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_40-60lat_21hrs.nc", subds="d2m"), HRUs)-273.15

## Calculated from the partial pressures of water vapor and saturation.
HR_ERA5L_00 <- exp((17.625*T2Mr_ERA5L_00)/(243.04+T2Mr_ERA5L_00)) / exp((17.625*T2M_ERA5L_00)/(243.04+T2M_ERA5L_00))
RH_ERA5L_03 <- exp((17.625*T2Mr_ERA5L_03)/(243.04+T2Mr_ERA5L_03)) / exp((17.625*T2M_ERA5L_03)/(243.04+T2M_ERA5L_03))
RH_ERA5L_06 <- exp((17.625*T2Mr_ERA5L_06)/(243.04+T2Mr_ERA5L_06)) / exp((17.625*T2M_ERA5L_06)/(243.04+T2M_ERA5L_06))
RH_ERA5L_09 <- exp((17.625*T2Mr_ERA5L_09)/(243.04+T2Mr_ERA5L_09)) / exp((17.625*T2M_ERA5L_09)/(243.04+T2M_ERA5L_09))
RH_ERA5L_12 <- exp((17.625*T2Mr_ERA5L_12)/(243.04+T2Mr_ERA5L_12)) / exp((17.625*T2M_ERA5L_12)/(243.04+T2M_ERA5L_12))
RH_ERA5L_15 <- exp((17.625*T2Mr_ERA5L_15)/(243.04+T2Mr_ERA5L_15)) / exp((17.625*T2M_ERA5L_15)/(243.04+T2M_ERA5L_15))
RH_ERA5L_18 <- exp((17.625*T2Mr_ERA5L_18)/(243.04+T2Mr_ERA5L_18)) / exp((17.625*T2M_ERA5L_18)/(243.04+T2M_ERA5L_18))
RH_ERA5L_21 <- exp((17.625*T2Mr_ERA5L_21)/(243.04+T2Mr_ERA5L_21)) / exp((17.625*T2M_ERA5L_21)/(243.04+T2M_ERA5L_21))

RH_ERA5L   <- c(HR_ERA5L_00, RH_ERA5L_03, RH_ERA5L_06, RH_ERA5L_09, RH_ERA5L_12, RH_ERA5L_15, RH_ERA5L_18, RH_ERA5L_21)
RH_ERA5L_d <- tapp(RH_ERA5L, strftime(time(RH_ERA5L),format="%Y-%m-%d"), fun = mean)*100
time(RH_ERA5L_d) <- seq(from = as.POSIXct("1979-01-01", tz="UTC"), to = as.POSIXct("2021-09-01", tz="UTC"), by = "day")
RH_ERA5L_d <- subset(RH_ERA5L_d, which(time(RH_ERA5L_d) > '1979-03-31' & (time(RH_ERA5L_d) <= '2020-03-31')))

RH_ERA5L_m <- tapp(RH_ERA5L_d, strftime(time(RH_ERA5L_d),format="%Y"), fun = mean)
RH_ERA5L_m <- mean(RH_ERA5L_m[[-c(1,nlyr(RH_ERA5L_m))]]) #Exclude first and last

writeRaster(RH_ERA5L_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/RH_ERA5L_mean.tif", datatype="INT4S", overwrite = TRUE)
writeCDF(RH_ERA5L_d, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/RH_ERA5L_d.nc",prec="integer",
         overwrite=TRUE, varname="rh", unit="%", longname="Relative humidity", zname="time", compression = 9)


## Cloud cover
CC_MODIS <- rast(list.files("E:/Datasets/CLOUD_COVER/", full.names = T))
CC_MODIS <- crop(CC_MODIS, HRUs)*0.01
CC_MODIS[CC_MODIS > 100] <- NA

writeRaster(mean(CC_MODIS), "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/CC_MODIS_mean.tif", datatype="INT4S", overwrite = TRUE)
writeCDF(CC_MODIS, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/CC_MODIS.nc",  
         prec="integer", overwrite=TRUE, varname="cc", unit="%", longname="Cloud cover", zname="time", compression = 9)

