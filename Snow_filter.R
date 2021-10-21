rm(list=ls())
cat("\014")  

library("raster")

#Part 2: Filter
stack_terrac<-stack(list.files(path="C:/Users/Rodrigo/Desktop/Snow/Terra_v2c", pattern = "tif$", full.names = TRUE))
cuencas_np<-shapefile("C:/Users/Rodrigo/Documents/ArcGIS/Chile/north_patagonia_basin.shp")
dem<-resample(raster("C:/Users/Rodrigo/Documents/ArcGIS/Chile/dem_3arc_p2p.tif"), stack_terrac[[1]])

suma <- function(x) {
  sum(x)
}

for(j in 1:5) {
   stack_j<-mask(crop(stack_terrac,cuencas_np[j,]),cuencas_np[j,])
   dem_j<-mask(crop(dem,cuencas_np[j,]),cuencas_np[j,])
   clouds_j<-t(freq(stack_j,merge=TRUE))
   cloud_tipping<-0.15*sum(clouds_j[2,1:3])
  
   for(i in 2:778) {
  
    nieve_2<-stack_j[[i]]
    DEMsuelo<-dem_j
    DEMnieve<-dem_j
    DEMsuelo1<-dem_j
    DEMnieve1<-dem_j
  
  #Bajo 15% de nubes
  if (clouds_j[i+1,2] <= cloud_tipping  | is.na(clouds_j[i+1,2]) ){
    
    #Linea de nieves
    DEMnieve[nieve_2 != 1] <- NA
    DEMsuelo[nieve_2 != -1] <- NA
    lNieve<-mean(getValues(DEMnieve), na.rm = TRUE)
    lSuelo<-mean(getValues(DEMsuelo), na.rm = TRUE)
    
    DEMnieve1[dem_j <= lNieve] <- NA
    DEMnieve1[dem_j > lNieve] <- 1
    DEMsuelo1[dem_j >= lSuelo] <- NA
    DEMsuelo1[dem_j < lSuelo] <- -1
    
    nieve_2[nieve_2==0 & !is.na(DEMnieve1)] <- DEMnieve1[nieve_2==0 & !is.na(DEMnieve1) ]
    nieve_2[nieve_2==0 & !is.na(DEMsuelo1)] <- DEMsuelo1[nieve_2==0 & !is.na(DEMsuelo1) ]
    
    writeRaster(nieve_2, paste0(cuencas_np$Name[j],'_',names(stack_terrac)[i]), datatype='INT2S', format = "GTiff",overwrite=TRUE)
    print(paste0(cuencas_np$Name[j],'_if_',i))
    
    #si sobre 15% de nubes
  } else {
    
    #filtro temporal
    nieve_2<-stack_j[[i]]
    condicion <- stack_j[[i-1]]==stack_j[[i+1]] & stack_j[[i-1]]!=0 & stack_j[[i+1]]!=0 & nieve_2==0
    nieve_2[condicion]=stack_j[[i-1]][condicion]

    #filtro espacial
    focal <- focal(nieve_2,w=matrix(1, ncol=3, nrow=3), fun=suma)
    focal[nieve_2 != 0] <- NA
    
    nieve_2[focal > 0] <- 1
    nieve_2[focal < 0] <- -1
    
    #Linea de nieves
    DEMnieve[nieve_2 != 1] <- NA
    DEMsuelo[nieve_2 != -1] <- NA
    lNieve<-mean(getValues(DEMnieve), na.rm = TRUE)
    lSuelo<-mean(getValues(DEMsuelo), na.rm = TRUE)
    
    DEMnieve1[dem_j <= lNieve] <- NA
    DEMnieve1[dem_j > lNieve] <- 1
    DEMsuelo1[dem_j >= lSuelo] <- NA
    DEMsuelo1[dem_j < lSuelo] <- -1
    
    nieve_2[nieve_2==0 & !is.na(DEMnieve1)] <- DEMnieve1[nieve_2==0 & !is.na(DEMnieve1) ]
    nieve_2[nieve_2==0 & !is.na(DEMsuelo1)] <- DEMsuelo1[nieve_2==0 & !is.na(DEMsuelo1) ]
    
    writeRaster(nieve_2, paste0(cuencas_np$Name[j],'_',names(stack_terrac)[i]), datatype='INT2S', format = "GTiff",overwrite=TRUE)
    print(paste0(cuencas_np$Name[j],'_else_',i))
    
  }
  }
}

##Parte 3: Final results
stack_2puelo<-stack(list.files(path="C:/Users/Rodrigo/Dropbox/Puelo/Datos/Nieve/Nieve_MODIS/2_Puelo", pattern = "tif$", full.names = TRUE))
write.xlsx(t(freq(stack_2puelo,merge=TRUE)), "C:/Users/Rodrigo/Dropbox/Rstudio/Snow_Puelo.xlsx")
