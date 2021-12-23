rm(list=ls())
cat("\014")  

library("terra")

LAI_dir <-list.files("C:/Users/rooda/Desktop/nvdi1/VI_16Days_500m_v6/NDVI", full.names = TRUE, recursive = FALSE)
LAI_data<-rast(LAI_dir)
LAI_data<-LAI_data*0.0001 #Scale factor

#Extract date from row-names
year<-as.numeric(substr(basename(LAI_dir), 14, 17))
day <-as.numeric(substr(basename(LAI_dir), 19, 21))
time(LAI_data)<-as.Date(day, origin = paste0(year, "-01-01"))


LAI_data_monthly <- tapp(LAI_data, strftime(time(LAI_data),format="%m"), fun = mean, na.rm = TRUE)
LAI_data_yearly  <- mean(tapp(LAI_data, strftime(time(LAI_data),format="%Y"), fun = mean), na.rm = TRUE)

#Kc calculation
land_cover<-rast("C:/Users/rooda/Dropbox/Patagonia/GIS South/land_cover_500m.tif")
dem <- rast("C:/Users/rooda/Dropbox/Patagonia/GIS South/dem_patagonia3f.tif")
land_cover<-resample(land_cover, LAI_data_yearly, method = "near")
dem<-resample(dem, LAI_data_yearly, method = "bilinear")

Kc_min = 0.4
Kc_max = 1.2

Kc_y = Kc_min + (Kc_max-Kc_min)*((LAI_data_yearly-minmax(LAI_data_yearly)[1])/(minmax(LAI_data_yearly)[2]-minmax(LAI_data_yearly)[1]))
Kc_y[land_cover == 17 & dem > 1] <- 1.2

writeRaster(Kc_y, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Kc_mean.tif", overwrite = TRUE)

NDVI_min<-min(minmax(LAI_data_monthly))
NDVI_max<-max(minmax(LAI_data_monthly))

Kc_m = Kc_min + (Kc_max-Kc_min)*((LAI_data_monthly-NDVI_min)/(NDVI_max-NDVI_min))
Kc_m[land_cover == 17 & dem > 1] <- 1.2

writeCDF(Kc_m, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Kc_monthly.nc", overwrite=TRUE, 
         varname="Kc", unit="-", longname="Cropp coefficient", zname="time", compression = 9)