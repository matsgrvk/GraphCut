.libPaths("/home/users/mgarvik/GC_M2/R/x86_64-pc-linux-gnu-library/3.5")
list.of.packages <- c("ncdf4","fields","maps","mapdata","RcppXPtrUtils","devtools","paramtest","overlapping","lattice","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
install_github("thaos/gcoWrapR")
library(gcoWrapR)

setwd("/home/users/mgarvik/GC_M2/workspace/")
load("models_list.rdata")
source("load_tas.R")
setwd("/home/users/mgarvik/GC_M2/workspace/")
colorTable<- designer.colors(64, c( "blue","grey90", "red"))

nc_ERAI <- nc_open("tmp_tas_30Ayr_ERA5_197901_200812.nc")
tas_ERAI <- ncvar_get(nc_ERAI,"t2m")
lon <- ncvar_get(nc_ERAI,"lon")
lat <- ncvar_get(nc_ERAI,"lat")
nc_close(nc_ERAI)

model <- TAS_MODELS[,1]
model_version <- TAS_MODELS[,2]
tas_ref = tas_ERAI

for(i in 1:length(model)){
  assign(paste0("bias_",model_version[i],"_2038"),
         get(paste0("tas_",model_version[i],"_2038"))-tas_ref)
}

bias_list_mme <- list()

for (i in 1:length(model)){
  assign(paste0("bias",i), tas_ref)
  assign(paste0("bias",i), get(paste0("bias_",model_version[i],"_2038")))
  bias_list_mme[i] <-list(get(paste0("bias",i)))
}

tas_list_mme <- list()

for (i in 1:length(model)){
  assign(paste0("tas",i), tas_ref)
  assign(paste0("tas",i), get(paste0("tas_",model_version[i],"_2038")))
  tas_list_mme[i] <-list(get(paste0("tas",i)))
}


mat_mean_mme <- matrix(0, nrow(tas_ref), ncol(tas_ref))
tmp_mat_mean <- array(0,c(nrow(tas_ref),ncol(tas_ref),length(bias_list_mme)))

mat_tas_mme <- matrix(0, nrow(tas_ref), ncol(tas_ref))
tmp_mat_tas <- array(0,c(nrow(tas_ref),ncol(tas_ref),length(tas_list_mme)))


for(x in 1:nrow(tas_ref)){
  for(y in 1:ncol(tas_ref)){
    for(i in 1:length(bias_list_mme)){
      tmp_mat_mean[,,i] <- bias_list_mme[[i]]
    }
    mat_mean_mme[x,y] <- mean(tmp_mat_mean[x,y,])
  }
}


for(x in 1:nrow(tas_ref)){
  for(y in 1:ncol(tas_ref)){
    for(i in 1:length(tas_list_mme)){
      tmp_mat_tas[,,i] <- tas_list_mme[[i]]
    }
    mat_tas_mme[x,y] <- mean(tmp_mat_tas[x,y,])
  }
}

save(mat_mean,file="bias_list_mme.rdata")