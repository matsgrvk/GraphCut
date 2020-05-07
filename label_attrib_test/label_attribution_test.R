list.of.packages <- c("ncdf4","fields","maps","mapdata","RcppXPtrUtils","devtools","paramtest","overlapping","lattice","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
library(gcoWrapR)

load("models_list.rdata")
load("load_tas_cv.rdata")

model_5mod <- TAS_MODELS[c(7,13,22,26,31),1]  ### Modèles utilisés (dans l'ordre) : CNRM,IPSL,MPI,GISS,NORESM
model_version_5mod <- TAS_MODELS[c(7,13,22,26,31),2]
model_list <- list("cnrm","ipsl","mpi","giss","noresm")

for(m in 1:length(model_5mod)){
  tas_ref = get(paste0("tas_",model_version_5mod[[m]]))
  
  nlabs = length(model_5mod)-1
  width = ncol(tas_ref)
  height = nrow(tas_ref)
  
  tas_list <- list()
  bias_list <- list()
  
  for(i in 1:length(model_version_5mod)){
    tas_list[[i]] <- get(paste0("tas_",model_version_5mod[[i]]))
    bias_list[[i]] <- tas_list[[i]]- tas_ref
  }
  
  tas_list[[m]] = bias_list[[m]] = NULL
  
  labs <- sapply(tas_list, identity, simplify = "array")
  labs_c <- labs-273.15
  bias <- sapply(bias_list, identity, simplify = "array")
  bias_2 = bias
  bias = c(aperm(bias, c(2, 1, 3)))
  
  weight_test <- function(iter,p0){
    # instanciation of class GCoptimizationGridGraph
    gco <- new(GCoptimizationGridGraph, width, height, nlabs)
    # Preparing the DataCost and SmoothCost functions of the MRF in C++
    ptrDataCost <- cppXPtr(
      code = 'float dataFn(int p, int l, Rcpp::List extraData)
  {
    int numPix = extraData["numPix"];
    float weight = extraData["weight"];
    NumericVector data = extraData["data"];
    return(weight * abs(data[p + numPix * l]) );
  }',
      includes = c("#include <math.h>", "#include <Rcpp.h>")
    )
    ptrSmoothCost <- cppXPtr(
      code = 'float smoothFn(int p1, int p2, int l1, int l2, Rcpp::List extraData)
  {
    int numPix = extraData["numPix"];
    float weight = extraData["weight"];
    NumericVector data = extraData["data"];
    float cost = abs(data[p1 + numPix * l1]-data[p1 + numPix * l2])
    + abs(data[p2 + numPix * l1] - data[p2 + numPix * l2]) ;
    return(weight * cost);
  }',
      includes = c("#include <math.h>", "#include <Rcpp.h>")
    )
    # Assigning the DataCost and SmoothCost functions to the MRF graph, gco
    gco$setDataCost(ptrDataCost, list(numPix = width * height, data = bias, weight = p0))
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
    
    mat_gco_label_world <- matrix(NA, nrow=height, ncol=width)
    
    for(j in 1:height){
      for(i in 1:width){
        mat_gco_label_world[j,i] <- gco$whatLabel((i - 1) + width * (j - 1))
      }
    }
    
    t=0
    
    t=t+1
    
    tmp_list_label_attrib <- vector("list",length(model_list))
    
    for(i in 1:length(model_5mod)){
      assign(paste0("weight_spaef_",model_list[[i]]), vector(mode="list",length=130))
      tmp_list_label_attrib[[i]] <- list(get(paste0("weight_spaef_",model_list[[i]])))
    }
    
    
    tmp_list_label_attrib[[t]] <- list("Label attribution" = mat_gco_label_world)
  }
  
  assign(paste0("label_attribution_",model_list[[m]]), grid_search(weight_test, params=list(p0 = c(seq(1,3,1))), n.iter=1,output="list"))
}

label_attribution_list <- vector("list",length(model_list))

for(i in 1:length(label_attribution_list)){
  label_attribution_list[[i]] <- get(paste0("label_attribution_",model_list[[i]]))
}

#### label_attribution_list[[m]]$results[[p0]]$`Label attribution` avec :
#### m : modèle utilisé en ref (1:5)
#### p0 : resultat en fonction du p0 (1:130)


# save(label_attribution_list, file="label_attribution_list_cv.rdata")
