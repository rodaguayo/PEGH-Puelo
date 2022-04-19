rm(list=ls())
cat("\014")  

library("terra")

HRUs<-vect("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Subcuencas_Puelo.shp") #Default for Puelo 
HRUs<-ext(c(-73.9,-70.8,-49.4,-40.7))

#PatatogiaMet v1.0
pp_pmet  <-rast("C:/Users/rooda/Dropbox/Patagonia/Data/Precipitation/PP_PMET_1990_2019_v2.nc")
pp_pmet  <-mean(tapp(pp_pmet, index = format(time(pp_pmet), "%Y"), fun = sum))
pp_pmet  <-crop(pp_pmet, HRUs)


#CR2MET v2.0
pp_cr2met <- rast("E:/Datasets/CR2MET/CR2MET_pr_v2.0_mon_1979_2019_005deg.nc",  subds="pr")
time(pp_cr2met)<-seq(from = as.POSIXct("1979-01-01", tz="UTC"), to = as.POSIXct("2019-12-31", tz="UTC"), by = "month")
pp_cr2met <- subset(pp_cr2met, which(time(pp_cr2met) > '1990-01-01' & (time(pp_cr2met) <= '2019-12-31')))
pp_cr2met <- mean(tapp(pp_cr2met, index = format(time(pp_cr2met), "%Y"), fun = sum))
pp_cr2met  <-crop(pp_cr2met, HRUs)

factor = (pp_pmet/pp_cr2met)
factor[factor< 1]<-1
factor[factor> 5]<-5

writeRaster(factor, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_factor.tif", overwrite = TRUE)
