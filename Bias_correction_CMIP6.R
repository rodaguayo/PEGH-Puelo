rm(list=ls())
cat("\014")  

options(java.parameters = "-Xmx16g")  

library("transformeR")
library("downscaleR")
library("loadeR")
library("visualizeR")
library("loadeR.2nc")
library("tools")

## Precipitation
pp_baseline<-loadGridData("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Precipitation/PP_CR2MET_d.nc",
                          lonLim = c(-72.4, -71.1), latLim = c(-42.5, -41.10), var = "pr", years = 1995:2000)
t2m_baseline<-loadGridData("C:/Users/rooda/Dropbox/Proyectos/Puelo PEGH/Data/Temperature/T2M_CR2MET_d.nc", 
                           lonLim = c(-72.4, -71.1), latLim = c(-42.5, -41.10), var = "t2m", years = 1980:2010)

gcm_list<-list.dirs("E:/Datasets/GCMs/Daily", full.names = TRUE,recursive =FALSE)
ssp_list<-c("historical","ssp126","ssp245", "ssp585")

for(j in 4:4) {
  pp_files<-list.files(path=gcm_list[j], pattern = "subset.+pr.+nc$", full.names = TRUE, recursive = FALSE)
  t2m_files<-list.files(path=gcm_list[j], pattern = "subset.+tas.+nc$", full.names = TRUE, recursive = FALSE)
  
  pp_historical <- grep(pattern = ssp_list[1], pp_files, value = T)
  pp_historical <- loadGridData(pp_historical, var = "pr", years = 1990:1991)
  pp_historical <- interpGrid(pp_historical,  new.coordinates = getGrid(pp_baseline), method = "bilinear", bilin.method = "akima")
  
  t2m_historical <- grep(pattern = ssp_list[1], t2m_files, value = T)
  t2m_historical <- loadGridData(t2m_historical, var = "tas", years = 1980:2010)
  t2m_historical <- interpGrid(t2m_historical,  new.coordinates = getGrid(t2m_baseline), method = "bilinear", bilin.method = "akima")
  
  for(i in 2:4) {
    
    pp_model_name <- grep(pattern = ssp_list[i], pp_files, value = T)
    pp_model_ssp  <- loadGridData(pp_model_name, var = "pr", years = 2021:2022)
    pp_model_ssp  <- interpGrid(pp_model_ssp,  new.coordinates = getGrid(pp_historical), method = "bilinear", bilin.method = "akima")
    pp_model_ssp_qdm  <- biasCorrection(y = pp_baseline, x = pp_historical,  newdata = pp_model_ssp, precipitation = TRUE, method = "qdm")
    
    t2m_model_name<-grep(pattern = ssp_list[i], t2m_files, value = T)
    t2m_model_ssp <- loadGridData(t2m_model_name, var = "tas", years = 2020:2060)
    t2m_model_ssp <- interpGrid(t2m_model_ssp, new.coordinates = getGrid(t2m_historical), method = "bilinear", bilin.method = "akima")
    t2m_model_ssp_qdm <- biasCorrection(y = t2m_baseline, x = t2m_historical, newdata = t2m_model_ssp, precipitation = FALSE, method = "qdm")
    
    setwd(gcm_list[j])
    grid2nc(pp_model_ssp_qdm, paste0(basename(pp_model_name),"_qdm.nc4"))
    grid2nc(t2m_model_ssp_qdm, paste0(basename(t2m_model_name),"_qdm.nc4"))
    print(paste0("model_",j,"_ssp_",i-1))
    }
  }
