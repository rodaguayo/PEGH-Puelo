rm(list=ls())
cat("\014")  

library("terra")

## Preproccesing dh/dt
dh_dt<-list.files("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Glaciers/dhdt_Hugonnet_raw", full.names = TRUE)
dh_dt_ref<-rast(dh_dt[1])

for (i in 1: length(dh_dt)){
  dh_dt_i<-rast(dh_dt[i])
  dh_dt_i<-project(dh_dt_i, crs(dh_dt_ref), filename = basename(dh_dt)[i], overwrite=TRUE)
  print(i)
}

dh_dt<-list.files("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Glaciers/dhdt_Hugonnet_raw", full.names = TRUE)
dh_dt<- writeRaster(vrt(dh_dt), "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Glaciers/dhdt_Hugonnet.tif")



## Preproccesing thickness
ids<-as.data.frame(vect("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Glaciers/Glaciers_RGI6.shp"))
ids<-subset(ids, Area > 1)$RGIId
thickness<-list.files("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Glaciers/Farinotti_et_al_2019", full.names = TRUE)
thickness_names<- substr(basename(thickness),1,14)

thickness<-thickness[thickness_names %in% ids]  #Subset based on RGI ID (previously masked)
thickness<-lapply(thickness, rast)


for (i in 1: length(thickness)){
  thickness[[i]]<-resample(thickness[[i]], rast(crs = crs(thickness[[1]]), resolution = 25, extent = ext(thickness[[i]])))
  thickness[[i]]<-aggregate(thickness[[i]], fact = 4)
  print(i)
}

thickness <- split(thickness, 1:50)
thickness <- lapply(thickness, src)
thickness <- lapply(thickness, mosaic, fun="max")

thickness_v2 <- src(thickness)
thickness_v2 <- mosaic(thickness_v2, fun="max", overwrite=TRUE)


writeRaster(thickness_v2, "C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Glaciers/thickness_farinotti.tif")








