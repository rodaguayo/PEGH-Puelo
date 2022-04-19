rm(list=ls())
cat("\014")  

library("terra")

HRUs<-vect("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Elevation_bands.shp") #Default for Puelo 
HRUs<-ext(c(-73.9,-70.8,-49.4,-40.7))

HRUs<-ext(c(-74,-70,-38,-31)) #For RH and wind speed (David)

## Precipitation baseline
PP_CR2MET <- rast("E:/Datasets/CR2MET/CR2MET_pr_v2.0_day_1979_2020_005deg.nc",  subds="pr")
PP_CR2MET <- subset(PP_CR2MET, which(time(PP_CR2MET) > '1979-03-31' & (time(PP_CR2MET) <= '2020-03-31')))
PP_CR2MET <- crop(PP_CR2MET,HRUs)

factor<-rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_factor.tif")
factor <- crop(factor, PP_CR2MET)
PP_CR2MET_d<-PP_CR2MET*factor

PP_CR2MET_m <- tapp(PP_CR2MET, strftime(time(PP_CR2MET),format="%Y"), fun = sum)
PP_CR2MET_m <- mean(PP_CR2MET_m[[-c(1,nlyr(PP_CR2MET_m))]]) #Exclude first and last

writeRaster(PP_CR2MET_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_mean.tif", datatype="INT4S", overwrite = TRUE)
writeCDF(PP_CR2MET, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_d.nc", 
         overwrite=TRUE, varname="pr", unit="mm", longname="precipitation", zname="time", compression = 9)


## Temperature baseline
T2M_CR2MET_v0 <- rast("E:/Datasets/CR2MET/CR2MET_t2m_v2.0_day_1979_2020_005deg.nc", subds="t2m")
T2M_CR2MET_v0 <- subset(T2M_CR2MET_v0, which(time(T2M_CR2MET_v0) > '1979-03-31' & (time(T2M_CR2MET_v0) <= '2020-03-31')))
T2M_CR2MET_v0 <- crop(T2M_CR2MET_v0, HRUs) # Crop for study area
T2M_CR2MET_v1 <- disagg(T2M_CR2MET_v0, 2)

dem_001        <- rast("C:/Users/rooda/Dropbox/Patagonia/GIS South/dem_patagonia3f.tif")
dem_001[is.na(dem_001)] <- 0 # Downscaling from 0.05 to 0.025 (0.01 in variables, ups)

dem_001<-resample(dem_001, T2M_CR2MET_v1, method="bilinear")
dem_005<-resample(dem_001, T2M_CR2MET_v0, method="bilinear")
dem_005<-resample(dem_005, dem_001, method="near")
T2M_CR2MET_v1<-T2M_CR2MET_v1+(dem_005-dem_001)*0.0065

T2M_CR2MET_v1m <- tapp(T2M_CR2MET_v1, strftime(time(T2M_CR2MET_v1),format="%Y"), fun = mean)
T2M_CR2MET_v1m <- mean(T2M_CR2MET_v1m[[-c(1,nlyr(T2M_CR2MET_v1m))]]) #Exclude first and last

writeRaster(T2M_CR2MET_v1m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Temperature/T2M_CR2MET_mean.tif", overwrite = TRUE)
writeCDF(T2M_CR2MET_v1, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Temperature/T2M_CR2MET_d.nc",
         overwrite=TRUE, varname="t2m", unit="degC", longname="temperature", zname="time", compression = 9)


## Wind speed baseline
WD_ERA5L_u00 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_00hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u03 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_03hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u06 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_06hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u09 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_09hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u12 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_12hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u15 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_15hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u18 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_18hrs.nc", subds="u10"), HRUs)
WD_ERA5L_u21 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_21hrs.nc", subds="u10"), HRUs)

WD_ERA5L_v00 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_00hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v03 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_03hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v06 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_06hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v09 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_09hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v12 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_12hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v15 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_15hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v18 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_18hrs.nc", subds="v10"), HRUs)
WD_ERA5L_v21 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_21hrs.nc", subds="v10"), HRUs)

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
time(WD_ERA5L_d) <- seq(from = as.POSIXct("1979-01-01", tz="UTC"), to = as.POSIXct("2020-12-31", tz="UTC"), by = "day")
WD_ERA5L_d <- subset(WD_ERA5L_d, which(time(WD_ERA5L_d) > '1979-03-31' & (time(WD_ERA5L_d) <= '2020-03-31')))

WD_ERA5L_m <- tapp(WD_ERA5L_d, strftime(time(WD_ERA5L_d),format="%Y"), fun = mean)
WD_ERA5L_m <- mean(WD_ERA5L_m[[-c(1,nlyr(WD_ERA5L_m))]]) #Exclude first and last

writeRaster(WD_ERA5L_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/WD_ERA5L_mean.tif", overwrite = TRUE)
writeCDF(WD_ERA5L_d, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/WD_ERA5L_d.nc",
         overwrite=TRUE, varname="wd", unit="m/s", longname="Wind speed", zname="time", compression = 9)

## Relative humidity baseline
T2M_ERA5L_00 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_00hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_03 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_03hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_06 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_06hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_09 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_09hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_12 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_12hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_15 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_15hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_18 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_18hrs.nc", subds="t2m"), HRUs)-273.15
T2M_ERA5L_21 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_21hrs.nc", subds="t2m"), HRUs)-273.15

T2Mr_ERA5L_00 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_00hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_03 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_03hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_06 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_06hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_09 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_09hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_12 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_12hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_15 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_15hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_18 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_18hrs.nc", subds="d2m"), HRUs)-273.15
T2Mr_ERA5L_21 <- crop(rast("E:/Datasets/ERA5_LAND/RH_Wind_30-50lat_21hrs.nc", subds="d2m"), HRUs)-273.15

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
time(RH_ERA5L_d) <- seq(from = as.POSIXct("1978-12-31", tz="UTC"), to = as.POSIXct("2020-12-31", tz="UTC"), by = "day")
RH_ERA5L_d <- subset(RH_ERA5L_d, which(time(RH_ERA5L_d) > '1979-03-31' & (time(RH_ERA5L_d) <= '2020-03-31')))

RH_ERA5L_m <- tapp(RH_ERA5L_d, strftime(time(RH_ERA5L_d),format="%Y"), fun = mean)
RH_ERA5L_m <- mean(RH_ERA5L_m[[-c(1,nlyr(RH_ERA5L_m))]]) #Exclude first and last

writeRaster(RH_ERA5L_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/RH_ERA5L_mean.tif", datatype="INT4S", overwrite = TRUE)
writeCDF(RH_ERA5L_d, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/RH_ERA5L_d.nc", overwrite=TRUE, 
         varname="rh", unit="%", longname="Relative humidity", zname="time", compression = 9)


## Cloud cover
CC_MODIS   <- rast(list.files("E:/Datasets/CLOUD_COVER2/", full.names = T, recursive = FALSE, pattern = ".tif$"))
time_modis <- MODIS::extractDate(list.files("E:/Datasets/CLOUD_COVER2/HDF", recursive = FALSE), asDate = TRUE)
CC_MODIS <- crop(CC_MODIS, HRUs)
time(CC_MODIS)<-time_modis$inputLayerDates

CC_MODIS_m <- tapp(CC_MODIS, strftime(time(CC_MODIS),format="%Y"), fun = mean, na.rm = TRUE)
CC_MODIS_m <- mean(CC_MODIS_m[[-c(1,nlyr(CC_MODIS_m))]])

writeRaster(CC_MODIS_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/CC_MODIS_mean.tif", datatype="INT4S", overwrite = TRUE)
writeCDF(CC_MODIS, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Others/CC_MODIS.nc", overwrite=TRUE, 
         varname="cc", unit="%", longname="Cloud cover", zname="time", compression = 9)


## Evapotranspiration (ET) and potential evapotranspiration (PET)
library("raster")
PET_GLEAM <- stack(list.files("E:/Datasets/GLEAM/EP", full.names = T), varname="Ep")
crs(PET_GLEAM) <- crs(shapefile("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Elevation_bands.shp") )
PET_GLEAM <- setZ(PET_GLEAM, seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "day"))

ET_GLEAM <- stack(list.files("E:/Datasets/GLEAM/E", full.names = T), varname="E")
crs(ET_GLEAM) <- crs(shapefile("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Elevation_bands.shp") )
ET_GLEAM <- setZ(ET_GLEAM, seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "day"))

for (i in 1980:2020){
  PET_GLEAM_i <- subset(PET_GLEAM, which(getZ(PET_GLEAM) >= as.Date(paste0(i, "-01-01")) & (getZ(PET_GLEAM) <= as.Date(paste0(i, "-12-31")))))
  PET_GLEAM_i <- flip(t(PET_GLEAM_i), 1)
  PET_GLEAM_i <- crop(PET_GLEAM_i, extent(c(-75,-70,-51,-30)))
  writeRaster(PET_GLEAM_i, paste0("PET_GLEAM_",i,".nc"), format = "CDF", overwrite=TRUE, varname="pet", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time(day)")
  
  ET_GLEAM_i <- subset(ET_GLEAM, which(getZ(ET_GLEAM) >= as.Date(paste0(i, "-01-01")) & (getZ(ET_GLEAM) <= as.Date(paste0(i, "-12-31")))))
  ET_GLEAM_i <- flip(t(ET_GLEAM_i), 1)
  ET_GLEAM_i <- crop(ET_GLEAM_i, extent(c(-75,-70,-51,-30)))
  writeRaster(ET_GLEAM_i, paste0("ET_GLEAM_",i,".nc"), format = "CDF", overwrite=TRUE, varname="et", varunit="mm", xname="Longitude",   yname="Latitude", zname="Time(day)")
  print(i)
}

ET_GLEAM   <- rast(list.files("E:/Datasets/GLEAM/E_v2", full.names = T, recursive = FALSE, pattern = ".nc$"))
time(ET_GLEAM) <- seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "day")
ET_GLEAM <- crop(ET_GLEAM, HRUs)
ET_GLEAM_m <- tapp(ET_GLEAM, strftime(time(ET_GLEAM),format="%Y"), fun = sum, na.rm = TRUE)
ET_GLEAM_m <- mean(ET_GLEAM_m[[-c(1,nlyr(ET_GLEAM_m))]])

writeRaster(ET_GLEAM_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Evapotranspiration/ET_GLEAM_mean.tif", overwrite = TRUE)
writeCDF(ET_GLEAM, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Evapotranspiration/ET_GLEAM.nc", overwrite=TRUE, 
         varname="et", unit="mm", longname="Evapotranspiration", zname="time", compression = 9)

PET_GLEAM   <- rast(list.files("E:/Datasets/GLEAM/EP_v2", full.names = T, recursive = FALSE, pattern = ".nc$"))
time(PET_GLEAM) <- seq(as.Date("1980/1/1"), as.Date("2020/12/31"), "day")
PET_GLEAM <- crop(PET_GLEAM, HRUs)
PET_GLEAM_m <- tapp(PET_GLEAM, strftime(time(PET_GLEAM),format="%Y"), fun = sum, na.rm = TRUE)
PET_GLEAM_m <- mean(PET_GLEAM_m[[-c(1,nlyr(PET_GLEAM_m))]])

writeRaster(PET_GLEAM_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Evapotranspiration/PET_GLEAM_mean.tif", overwrite = TRUE)
writeCDF(PET_GLEAM, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Evapotranspiration/PET_GLEAM.nc", overwrite=TRUE, 
         varname="pet", unit="mm", longname="Potential Evapotranspiration", zname="time", compression = 9)

