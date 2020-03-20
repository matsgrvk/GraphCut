list.of.packages <- c("ncdf4","fields","maps","mapdata","RcppXPtrUtils","devtools","paramtest","overlapping","lattice","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
install_github("thaos/gcoWrapR")
library(gcoWrapR)

load("models_list.rdata")
load("load_tas_spaef.rdata")

colorTable<- designer.colors(64, c( "blue","grey90", "red"))

model_5mod <- TAS_MODELS[c(7,13,22,26,31),1]  ### Modèles utilisés (dans l'ordre) : CNRM,GISS,IPSL,MPI,NORESM
model_version_5mod <- TAS_MODELS[c(7,13,22,26,31),2]
tas_ref = `tas_NorESM1-M_2038` #### Modèle numéro 4

for(i in 1:length(model_5mod)){
  assign(paste0("bias_",model_version_5mod[i],"_2038"),
         get(paste0("tas_",model_version_5mod[i],"_2038"))-tas_ref)
}


nlabs = length(model_5mod)
width = ncol(tas_ref)
height = nrow(tas_ref)

lab_list <- list()
bias_list <- list()
ref = matrix(0, nrow = height, ncol = width)

for (i in 1:length(model_5mod)){
  assign(paste0("lab",i), ref)
  assign(paste0("lab",i), get(paste0("tas_",model_version_5mod[i],"_2038"))) #model_list[i] = tas_model
  lab_list[i] <-list(get(paste0("lab",i)))
  assign(paste0("bias",i), ref)
  assign(paste0("bias",i), get(paste0("bias_",model_version_5mod[i],"_2038")))
  bias_list[i] <-list(get(paste0("bias",i)))
}

ref=tas_ref

labs <- sapply(lab_list, identity, simplify = "array")
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
gco$setDataCost(ptrDataCost, list(numPix = width * height, data = bias, weight = 0))
gco$setSmoothCost(ptrSmoothCost, list(numPix = width * height, data = bias, weight = 1))

mat_min <- matrix(1,nrow(tas_ref),ncol(tas_ref))

for(xmin in 1:nrow(tas_ERAI)){
  for(ymin in 1:ncol(tas_ERAI)){
    mat_min[xmin,ymin] = which.min(abs(bias_2[xmin,ymin,]))-1 
  }
}

vec_min <- as.vector(t(mat_min))

for(z in 0:(length(ref)-1)){
  gco$setLabel(z,vec_min[z+1])
}

print(vec_min)

# Optimizing the MRF energy with alpha-beta swap
gco$swap(-1)

mat_gco_label_world = tas_GC = matrix(NA, nrow=height, ncol=width)

for(j in 1:height){
  for(i in 1:width){
    mat_gco_label_world[j,i] <- gco$whatLabel((i - 1) + width * (j - 1))
  }
}

print(mat_gco_label_world)

for(y in 1:width){
  for(x in 1:height){
    for(i in 0:(length(model_5mod)-1)){
      if(mat_gco_label_world[x,y] == i){
        tas_GC[x,y] = labs[x,y,(i+1)]
      } 
    }
  }
}

all.equal(tas_GC,tas_ref)