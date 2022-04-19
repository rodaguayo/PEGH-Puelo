rm(list=ls())
cat("\014")  

source("C:/Users/rooda/Dropbox/Coding/PEGH-Puelo/qdm.R")
library("exactextractr")
library("terra")
library("sf")


#Area to analyze: Choose one
file<-"C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Elevation_bands.shp"
area<-vect(file)
name<-"Puelo"

file<-"C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Yelcho.shp"
area<-vect(file)
name<-"Yelcho"

file<-"C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Pascua.shp"
area<-vect(file)
name<-"Pascua"

file<-"C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Aysen.shp"
area<-vect(file)
name<-"Aysen"

file<-"C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Baker.shp"
area<-vect(file)
name<-"Baker"

file<-"C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/GIS/Cuencas/Cuenca_Bcostera.shp"
area<-vect(file)
name<-"Baker_c"

pp_baseline <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_d.nc")
factor<-rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_factor.tif")
pp_baseline <- subset(pp_baseline, which(time(pp_baseline) >= '1979-12-31'  & time(pp_baseline) <= '2010-12-31'))
area <- project(area, pp_baseline)
pp_baseline <- crop(pp_baseline, area)
pp_baseline <- pp_baseline*resample(factor,pp_baseline)

t2m_baseline <- rast("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Temperature/T2M_CR2MET_d.nc")
t2m_baseline <- subset(t2m_baseline, which(time(t2m_baseline) >= '1979-12-31'  & time(t2m_baseline) <= '2010-12-31'))
t2m_baseline <- crop(t2m_baseline, area)

gcm_list<-list.dirs("E:/Datasets/GCMs/Daily", full.names = TRUE,recursive =FALSE)
ssp_list<-c("historical","ssp126","ssp245", "ssp585")

for(j in 1:5) {
  setwd(gcm_list[j])
  files_delete <- dir(path=gcm_list[j],  pattern="subset", full.names = T)
  file.remove(files_delete)
  
  pp_files<-list.files(path=gcm_list[j], pattern = "pr_", full.names = TRUE, recursive = FALSE)
  t2m_files<-list.files(path=gcm_list[j], pattern = "tas_", full.names = TRUE, recursive = FALSE)
  
  pp_historical_names<-grep(pattern = ssp_list[1], pp_files, value = T)
  t2m_historical_names<-grep(pattern = ssp_list[1], t2m_files, value = T)
  
  pp_historical <- lapply(pp_historical_names, FUN = rast, subds = "pr")
  pp_historical <- rast(pp_historical)
  pp_historical <- subset(pp_historical, which(time(pp_historical) >= '1980-01-01'  & time(pp_historical) <= '2010-12-31'))
  crs(pp_historical) <- "epsg:4326"
  pp_historical <- shift(pp_historical, dx=-180- xmin(pp_historical))
  pp_historical <- resample(pp_historical, pp_baseline, method = "bilinear")*86400
  pp_historical <- round(pp_historical,2)
  
  t2m_historical <- lapply(t2m_historical_names, FUN = rast, subds = "tas")
  t2m_historical <- rast(t2m_historical)
  t2m_historical <- subset(t2m_historical, which(time(t2m_historical) >= '1980-01-01'))
  crs(t2m_historical) <- "epsg:4326"
  t2m_historical <- shift(t2m_historical, dx=-180- xmin(t2m_historical))
  t2m_historical <- resample(t2m_historical, t2m_baseline, method = "bilinear")-273.15
  t2m_historical <- round(pp_historical,2)
  print(paste0("historical_",j))
    
  for(i in 4:4) {
    
    pp_model_name <- grep(pattern = ssp_list[i], pp_files, value = T)
    t2m_model_name<-grep(pattern = ssp_list[i], t2m_files, value = T)
    
    pp_model_ssp <- lapply(pp_model_name, FUN = rast, subds = "pr")
    pp_model_ssp <- rast(pp_model_ssp)
    pp_model_ssp <- subset(pp_model_ssp, which(time(pp_model_ssp) >= '2020-01-01' & time(pp_model_ssp) <= '2060-12-31'))
    crs(pp_model_ssp) <- "epsg:4326"
    pp_model_ssp <- shift(pp_model_ssp, dx=-180- xmin(pp_model_ssp))
    pp_model_ssp <- resample(pp_model_ssp, pp_baseline, method = "bilinear")*86400
    pp_model_ssp <- round(pp_model_ssp,2)
    
    pp_model_ssp_qdm<-values(pp_model_ssp, mat=TRUE)
    for(k in 1:(nrow(pp_historical)*ncol(pp_historical))) {  pp_model_ssp_qdm[k,]<-qdm(o = as.numeric(pp_baseline[k]), p = as.numeric(pp_historical[k]), s = as.numeric(pp_model_ssp[k]), precip = TRUE, pr.threshold = 0.05, n.quantiles = 100)}
    pp_model_ssp_qdm<-setValues(pp_model_ssp, values = pp_model_ssp_qdm)

    t2m_model_ssp <- lapply(t2m_model_name, FUN = rast, subds = "tas")
    t2m_model_ssp <- rast(t2m_model_ssp)
    t2m_model_ssp <- subset(t2m_model_ssp, which(time(t2m_model_ssp) >= '2020-01-01' & time(t2m_model_ssp) <= '2060-12-31'))
    crs(t2m_model_ssp) <- "epsg:4326"
    t2m_model_ssp <- shift(t2m_model_ssp, dx=-180- xmin(t2m_model_ssp))
    t2m_model_ssp <- resample(t2m_model_ssp, t2m_baseline, method = "bilinear")-273.15
    t2m_model_ssp <- round(t2m_model_ssp,2)
    
    t2m_model_ssp_qdm<-values(t2m_model_ssp, mat=TRUE)
    for(k in 1:(nrow(t2m_historical)*ncol(t2m_historical))) {  t2m_model_ssp_qdm[k,]<-qdm(o = as.numeric(t2m_baseline[k]), p = as.numeric(t2m_historical[k]), s = as.numeric(t2m_model_ssp[k]), precip = FALSE, n.quantiles = 100)}
    t2m_model_ssp_qdm<-setValues(t2m_model_ssp, values = t2m_model_ssp_qdm)
    
    writeCDF(pp_model_ssp_qdm, paste0("subset_",basename(pp_model_name)[1]), varname="pr", unit="mm", longname="precipitation", zname="time", compression = 9, overwrite= T)
    writeCDF(t2m_model_ssp_qdm, paste0("subset_",basename(t2m_model_name)[1]), varname="tas", unit="degC", longname="air temperature", zname="time", compression = 9, overwrite= T)
    print(paste0("ssp_",i-1))
    }
}

HRUs<-st_read(file)

for(j in 1:5) {
  
  files<-list.files(path=gcm_list[j], pattern = ".nc", full.names = TRUE, recursive = FALSE)
  pp_files  <-grep(pattern = "subset.+pr_day", files, value = T)
  t2m_files <-grep(pattern = "subset.+tas_day", files, value = T)
  
  for(i in 1:1) {
    
    PP_gcm_d  <- rast(pp_files[i])
    T2M_gcm_d <- rast(t2m_files[i])
    date<-time(PP_gcm_d)
    PP_gcm_d  <- as.data.frame(t(exact_extract(PP_gcm_d, HRUs , 'mean')))
    T2M_gcm_d <- as.data.frame(t(exact_extract(T2M_gcm_d, HRUs , "mean")))
    
    PP_gcm_d  <- round(PP_gcm_d,0)
    T2M_gcm_d <- round(T2M_gcm_d,2)
    colnames(PP_gcm_d)  <- HRUs$ID_WEAP
    colnames(T2M_gcm_d) <- HRUs$ID_WEAP
    PP_gcm_d  <- cbind(date,PP_gcm_d)
    T2M_gcm_d <- cbind(date, T2M_gcm_d)
    
    name_gcm<-substring(pp_files[i], regexpr("day_", pp_files[i])+4, regexpr("i1p1f1", pp_files[i])-5)
    write.csv(PP_gcm_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Projections/Input_PP_",name,"_",name_gcm,".csv"), row.names = FALSE)
    write.csv(T2M_gcm_d, paste0("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/WEAP/Projections/Input_T2M_",name,"_",name_gcm,".csv"), row.names = FALSE)
    print(j)
  }
}


