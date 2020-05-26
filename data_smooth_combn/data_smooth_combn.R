# .libPaths("/home/users/mgarvik/GC_M2/R/x86_64-pc-linux-gnu-library/3.5")
list.of.packages <- c("ncdf4","fields","maps","mapdata","RcppXPtrUtils","devtools","paramtest","overlapping","lattice","reshape2","gtools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
install_github("thaos/gcoWrapR")
library(gcoWrapR)

# setwd("/home/users/mgarvik/GC_M2/workspace/")
load("models_list.rdata")
load("load_tas_cv.rdata")
# setwd("/home/users/mgarvik/GC_M2/workspace/")

model_5mod <- TAS_MODELS[c(7,13,22,26,31),1]  ### Modèles utilisés (dans l'ordre) : CNRM,IPSL,MPI,GISS,NORESM
model_version_5mod <- TAS_MODELS[c(7,13,22,26,31),2]
model_list <- list("cnrm","ipsl","mpi","giss","noresm")

y=permutations(length(model_5mod),length(model_5mod)-1) ### ensemble des combinaisons possibles
x=matrix(rep(1:length(model_5mod),nrow(y)),ncol=5,nrow=nrow(y),byrow = T)

data_smooth_list_combn <- vector("list",nrow(x))
t=0

for(m in 1:nrow(x)){
  ref <- sum(x[m,])-sum(y[m,])  ### permet de choisir le modèle qui n'est pas dans la perm
  
  tas_ref = get(paste0("tas_",model_version_5mod[[ref]]))
  
  nlabs = length(model_5mod)-1
  width = ncol(tas_ref)
  height = nrow(tas_ref)
  
  tas_list <- list()
  bias_list <- list()
  
  for(i in 1:length(model_version_5mod)){
    tas_list[[i]] <- get(paste0("tas_",model_version_5mod[[i]]))
    bias_list[[i]] <- tas_list[[i]]- tas_ref
  }
  
  tas_list[[ref]] = bias_list[[ref]] = NULL
  
  labs <- sapply(tas_list, identity, simplify = "array")
  labs_c <- labs-273.15
  bias <- sapply(bias_list, identity, simplify = "array")
  bias_2 = bias
  bias = c(aperm(bias, c(2, 1, 3)))
  
  # instanciation of class GCoptimizationGridGraph
  gco <- new(GCoptimizationGridGraph, width, height, nlabs)
  # Preparing the DataCost and SmoothCost functions of the MRF in C++
  ptrDataCost <- cppXPtr(
    code = 'float dataFn(int p, int l, Rcpp::List extraData)
  {
    int numPix = extraData["numPix"];
    float weight = extraData["weight"];
    NumericVector data = extraData["data"];
    return(weight * std::abs(data[p + numPix * l]) );
  }',
    includes = c("#include <math.h>", "#include <Rcpp.h>")
  )
  ptrSmoothCost <- cppXPtr(
    code = 'float smoothFn(int p1, int p2, int l1, int l2, Rcpp::List extraData)
  {
    int numPix = extraData["numPix"];
    float weight = extraData["weight"];
    NumericVector data = extraData["data"];
    float cost = std::abs(data[p1 + numPix * l1]-data[p1 + numPix * l2])
    + std::abs(data[p2 + numPix * l1] - data[p2 + numPix * l2]) ;
    return(weight * cost);
  }',
    includes = c("#include <math.h>", "#include <Rcpp.h>")
  )
  # Assigning the DataCost and SmoothCost functions to the MRF graph, gco
  gco$setDataCost(ptrDataCost, list(numPix = width * height, data = bias, weight = 1))
  gco$setSmoothCost(ptrSmoothCost, list(numPix = width * height, data = bias, weight = 1))
  
  mat_min <- matrix(floor(runif(length(tas_ref),min=0,max=3)),nrow(tas_ref),ncol(tas_ref))
  
  for(xmin in 1:nrow(tas_ref)){
    for(ymin in 1:ncol(tas_ref)){
      mat_min[xmin,ymin] = which.min(abs(bias_2[xmin,ymin,]))-1
    }
  }
  
  vec_min <- as.vector(t(mat_min))
  
  for(z in 0:(length(tas_ref)-1)){
    gco$setLabel(z,vec_min[z+1])
  }
  
  # Optimizing the MRF energy with alpha-beta swap
  gco$swap(-1)
  
  t=t+1
  
  
  data_smooth_list_combn[[t]] <- list("Data cost" = gco$giveDataEnergy(),"Smooth cost" = gco$giveSmoothEnergy())
}

# label_attribution_list <- vector("list",length(model_list))
# 
# for(i in 1:length(label_attribution_list)){
#   label_attribution_list[[i]] <- get(paste0("label_attribution_",model_list[[i]]))
# }

#### label_attribution_list[[m]]$results[[p0]]$`Label attribution` avec :
#### m : modèle utilisé en ref (1:5)
#### p0 : resultat en fonction du p0 (1:130)


save(data_smooth_list_combn, file="data_smooth_list_combn.rdata")
