rm(list=ls())
cat("\014")  

library("transformeR")
library("downscaleR")
library("loadeR")
library("visualizeR")
library("loadeR.2nc")
library("raster")
library("tools")

#N°1: Precipitation
pp_baseline<-loadGridData("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Precipitacion/Datos_Grillados_PP/PP_v3_2000_2019.nc", var = "Precipitation", years = 2001:2018)
ssp_list<-list.dirs(path="C:/Users/Rodrigo/Dropbox/Rstudio/Patagonia/CMIP5/Precipitation_MS2", full.names = TRUE,recursive =FALSE)
historical<-list.files(path=ssp_list[1], pattern = "nc$", full.names = TRUE, recursive = FALSE)

for(j in 1:5) {
  model_hist <- loadGridData(historical[j], var = "Precipitation")
  
  #For CMIP5 models
  model_hist <- subsetGrid(model_hist, years = 1980:2005)
  
  for(i in 2:4) {
    model_name<-list.files(path=ssp_list[i], pattern = "nc$", full.names = TRUE, recursive = FALSE)[j]
    model_ssp <- loadGridData(model_name, var = "Precipitation")
    model_ssp <- bindGrid(model_ssp, model_hist, dimension = "time")

    #training and simulate period
    model_training <- subsetGrid(model_ssp, years = 2001:2018)
    model_training <- interpGrid(model_training, new.coordinates = getGrid(pp_baseline), method = "bilinear", bilin.method = "akima")

    model_ssp <- subsetGrid(model_ssp, years = 2019:2070)
    model_ssp <- interpGrid(model_ssp, new.coordinates = getGrid(pp_baseline), method = "bilinear", bilin.method = "akima")
    
    model_ssp_eqm<-biasCorrection(y = pp_baseline, x = model_training, newdata = model_ssp, precipitation = TRUE, method = "eqm", extrapolation = "constant")
    model_ssp_qdm<-biasCorrection(y = pp_baseline, x = model_training, newdata = model_ssp, precipitation = TRUE, method = "qdm")
    model_ssp_ptr<-biasCorrection(y = pp_baseline, x = model_training, newdata = model_ssp, precipitation = TRUE, method = "ptr")
    grid2nc(model_ssp_eqm, paste0(file_path_sans_ext(basename(model_name)),"_eqm.nc4"))
    grid2nc(model_ssp_qdm, paste0(file_path_sans_ext(basename(model_name)),"_qdm.nc4"))
    grid2nc(model_ssp_ptr, paste0(file_path_sans_ext(basename(model_name)),"_ptr.nc4"))
    print(paste0("model_",j,"_ssp_",i-1))
      }
  }

#N°2: Temperature
pp_baseline<-loadGridData("C:/Users/Rodrigo/Dropbox/Puelo/Datos/Temperatura/Datos_Grillados_TEMP/Temp_v2_t2m_2000_2019.nc", var = "Temperature_t2m", years = 2001:2018)
ssp_list<-list.dirs(path="C:/Users/Rodrigo/Dropbox/Rstudio/Patagonia/CMIP5/Temperature_MS2", full.names = TRUE,recursive =FALSE)
historical<-list.files(path=ssp_list[1], pattern = "nc$", full.names = TRUE, recursive = FALSE)

for(j in 1:5) {
  model_hist <- loadGridData(historical[j], var = "Temperature")
  
  #For CMIP5 models
  model_hist <- subsetGrid(model_hist, years = 1980:2005)
  
  for(i in 2:4) {
    model_name<-list.files(path=ssp_list[i], pattern = "nc$", full.names = TRUE, recursive = FALSE)[j]
    model_ssp <- loadGridData(model_name, var = "Temperature")
    model_ssp <- bindGrid(model_ssp, model_hist, dimension = "time")
    
    #training and simulate period
    model_training <- subsetGrid(model_ssp, years = 2001:2018)
    model_training <- interpGrid(model_training, new.coordinates = getGrid(pp_baseline), method = "bilinear", bilin.method = "akima")
    model_ssp <- subsetGrid(model_ssp, years = 2019:2070)
    model_ssp <- interpGrid(model_ssp, new.coordinates = getGrid(pp_baseline), method = "bilinear", bilin.method = "akima")
    
    model_ssp_eqm<-biasCorrection(y = pp_baseline, x = model_training, newdata = model_ssp, precipitation = FALSE, method = "eqm", extrapolation = "constant")
    model_ssp_qdm<-biasCorrection(y = pp_baseline, x = model_training, newdata = model_ssp, precipitation = FALSE, method = "qdm")
    model_ssp_var<-biasCorrection(y = pp_baseline, x = model_training, newdata = model_ssp, precipitation = FALSE, method = "variance")
    grid2nc(model_ssp_eqm, paste0(file_path_sans_ext(basename(model_name)),"_eqm.nc4"))
    grid2nc(model_ssp_qdm, paste0(file_path_sans_ext(basename(model_name)),"_qdm.nc4"))
    grid2nc(model_ssp_var, paste0(file_path_sans_ext(basename(model_name)),"_ptr.nc4"))
    print(paste0("model_",j,"_ssp_",i-1))
  }
}