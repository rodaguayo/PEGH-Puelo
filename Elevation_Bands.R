rm(list=ls())
cat("\014")  

library("raster")
source('ElevationZones.R')

basins<-shapefile("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Subcuencas_Puelo_V2.shp")
nasadem_30m<-raster("C:/Users/rooda/Dropbox/ArcGIS/Chile/dem_patagonia1.tif")

elev_bands<-terra::vect()

for (i in 1:length(basins)) {
  
  elev_zones_i<-elevationZones(x=basins[i,], dem=nasadem_30m, max.zones = 4, min.elevZ = 500, elev.thres = 500)
  zone_i <- terra::rast(elev_zones_i$zonesRaster)
  zone_i <- rbind(terra::as.polygons(zone_i[[1]], values = F),terra::as.polygons(zone_i[[2]], values = F),
                  terra::as.polygons(zone_i[[3]], values = F),terra::as.polygons(zone_i[[4]], values = F))
  
  zone_i$ID <- paste0(basins[i,]$Nombre,"_",elev_zones_i$median.elevation)
  zone_i$median_elev <- elev_zones_i$median.elevation
  zone_i$HRU <- basins[i,]$Nombre
  
  elev_bands<-rbind(elev_bands,zone_i)
  print(i)
  
}

plot(elev_bands)
terra::writeVector(elev_bands, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Elevation_bands.shp", overwrite=TRUE)
