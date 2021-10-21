rm(list=ls())
cat("\014")  

library("raster")
library("MODIStsp")

#Download TERRA and AQUA files for 2000-2020 period 
MODIStsp()



#Part 1: Stack, crop and mask
cuencas_np<-shapefile("C:/Users/Rodrigo/Documents/ArcGIS/Chile/north_patagonia_basin.shp")

stack_terra<-stack(list.files(path="C:/Users/Rodrigo/Desktop/Snow/Terra_v2", pattern = "tif$", full.names = TRUE))
stack_aqua<-stack(list.files(path="C:/Users/Rodrigo/Desktop/Snow/Aqua_v2", pattern = "tif$", full.names = TRUE))
rclmat <- matrix(c(0, 11, 0, 11, 40, -1, 40, 50, 0, 50, 200,1 ,200, 255,0),ncol=3, byrow=TRUE)

for(i in 1:779) {
  terra_cut<-mask(crop(stack_terra[[i]],cuencas_np),cuencas_np)
  aqua_cut<-mask(crop(stack_aqua[[i]],cuencas_np),cuencas_np)
  
  terra_cut <- reclassify(terra_cut, rclmat)
  aqua_cut <-reclassify(aqua_cut, rclmat)
  
  terra_cut[terra_cut == 0] <- aqua_cut[terra_cut == 0]
  writeRaster(terra_cut, names(stack_terra[[i]]), datatype='INT2S', format = "GTiff",overwrite=TRUE)
  print(i)
}