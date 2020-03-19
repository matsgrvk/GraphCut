for(i in 1:length(TAS_MODELS[,1])){
  model <- TAS_MODELS[i,1]
  model_version <- TAS_MODELS[i,2]
  setwd(paste0("workspace/CMIP5_30Ayr/",model,""))
  assign(paste0("nc_",model_version,""),
         nc_open(paste0("tas_30Ayr_",model_version,"_hist_rcp_r1i1p1_197901_200812.nc","")))
  assign(paste0("tas_",model_version,""),
         ncvar_get(get(paste0("nc_",model_version,"")),"tas"))
  nc_close(get(paste0("nc_",model_version,"")))
}

for(i in 1:length(TAS_MODELS[,1])){
  model <- TAS_MODELS[i,1]
  model_version <- TAS_MODELS[i,2]
  setwd(paste0("workspace/CMIP5_30Ayr_2038/",model,""))
  assign(paste0("nc_",model_version,"_2038",""),
         nc_open(paste0("tas_30Ayr_",model_version,"_hist_rcp_r1i1p1_200901_203812.nc","")))
  assign(paste0("tas_",model_version,"_2038",""),
         ncvar_get(get(paste0("nc_",model_version,"_2038","")),"tas"))
  nc_close(get(paste0("nc_",model_version,"_2038","")))
}