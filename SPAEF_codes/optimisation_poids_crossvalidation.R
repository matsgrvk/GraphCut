.libPaths("/home/users/mgarvik/GC_M2/R/x86_64-pc-linux-gnu-library/3.5")
list.of.packages <- c("ncdf4","fields","maps","mapdata","RcppXPtrUtils","devtools","paramtest","overlapping","lattice","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
install_github("thaos/gcoWrapR")
library(gcoWrapR)

load("models_list.rdata")
load("load_tas_spaef.rdata")

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
  assign(paste0("bias_",model_version[i]),
         get(paste0("tas_",model_version[i]))-tas_ref)
}


nlabs = length(model)
width = ncol(tas_ref)
height = nrow(tas_ref)

lab_list <- list()
bias_list <- list()
lab_list_2038 <- list()
lab_list_2100 <- list()
ref = matrix(0, nrow = height, ncol = width)

for (i in 1:length(model)){
  assign(paste0("lab",i), ref)
  assign(paste0("lab",i), get(paste0("tas_",model_version[i]))) #model_list[i] = tas_model
  lab_list[i] <-list(get(paste0("lab",i)))
  assign(paste0("bias",i), ref)
  assign(paste0("bias",i), get(paste0("bias_",model_version[i])))
  bias_list[i] <-list(get(paste0("bias",i)))
  assign(paste0("lab",i,"_2038"), ref)
  assign(paste0("lab",i,"_2038"), get(paste0("tas_",model_version[i],"_2038")))
  lab_list_2038[i] <-list(get(paste0("lab",i,"_2038")))
  # assign(paste0("lab",i,"_2100"), ref)
  # assign(paste0("lab",i,"_2100"), get(paste0("tas_",model_version[i],"_2100")))
  # lab_list_2100[i] <-list(get(paste0("lab",i,"_2100")))
}

ref=tas_ref

labs <- sapply(lab_list, identity, simplify = "array")
labs_2038 <- sapply(lab_list_2038, identity, simplify = "array")
# labs_2100 <- sapply(lab_list_2100, identity, simplify = "array")
labs_c <- labs-273.15
bias <- sapply(bias_list, identity, simplify = "array")
bias_2 = bias
bias = c(aperm(bias, c(2, 1, 3)))

weight_test <- function(iter, p0, p1) {
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
  
  mat_min <- matrix(0,nrow(tas_ref),ncol(tas_ref))
  
  for(xmin in 1:nrow(tas_ERAI)){
    for(ymin in 1:ncol(tas_ERAI)){
      mat_min[xmin,ymin] = which.min(abs(bias_2[xmin,ymin,]))-1 
    }
  }
  
  vec_min <- as.vector(t(mat_min))
  
  for(z in 0:(length(ref)-1)){
    gco$setLabel(z,vec_min[z+1])
  }
  
  # Optimizing the MRF energy with alpha-beta swap
  gco$swap(-1)
  
  mat_gco_label_world <- matrix(0, nrow=height, ncol=width)
  for(j in 1:height){
    for(i in 1:width){
      mat_gco_label_world[j,i] <- gco$whatLabel((i - 1) + width * (j - 1))
    }
  }
  
  tas_GC <- mat_gco_label_world
  
  for(y in 1:width){
    for(x in 1:height){
      for(i in 0:(length(model)-1)){
        if(mat_gco_label_world[x,y] == i){
          tas_GC[x,y] = labs_2038[x,y,(i+1)]
        } 
      }
    }
  }
  

  bias_GC <- mat_gco_label_world
  
  for(y in 1:width){
    for(x in 1:height){
      for(i in 0:(length(model)-1)){
        if(mat_gco_label_world[x,y] == i){
          bias_GC[x,y] = bias_2[x,y,(i+1)]
        } 
      }
    }
  }
  

  t=0
  
  spaef <- function(models){
    for(model in models){
      for(i in 1:lengths(models)){
        assign(paste(model[i],"_tas",sep=""), melt(get(paste("tas_",model[i],sep=""))))
        ref_tas <- melt(tas_ERAI)
        
        df_temp <- melt(get(paste("tas_",model[i],sep="")))
        df_temp$type <- "Model" 
        
        assign(paste(model[i],"_tas",sep=""), df_temp)
        df_temp <- assign(paste(model[i],"_tas",sep=""), df_temp)
        ref_tas$type <- 'Reference'
        
        # and combine into your new data frame vegLengths
        tasValues <- rbind(get(paste(model[i],"_tas",sep="")), ref_tas)
        
        bw <- 2 * IQR(c(get(paste("tas_",model[i],sep="")),tas_ERAI))/
          length(c(get(paste("tas_",model[i],sep="")),tas_ERAI))^(1/3)
        
        
        ggplot(tasValues, aes(x = value, fill = type),main= "Model and ref histograms") +
          geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',binwidth = bw)
        
        
        assign(paste("alpha_",model[i],sep=""), cor(c(get(paste("tas_",model[i],sep=""))),c(tas_ERAI)))
        
        assign(paste("beta_",model[i],sep=""), (sd(get(paste("tas_",model[i],sep="")))/mean(get(paste("tas_",model[i],sep=""))))/
                 (sd(tas_ERAI)/mean(tas_ERAI)))
        
        assign(paste("hist_",model[i],sep=""), list(model = df_temp$value, ref = ref_tas$value))
        assign(paste("overlap_",model[i],sep=""), overlap(get(paste("hist_",model[i],sep=""))))
        df_temp <- assign(paste("overlap_",model[i],sep=""), overlap(get(paste("hist_",model[i],sep=""))))
        
        assign(paste("gamma_",model[i],sep=""), unname(df_temp$OV))
        
        assign(paste("SPAEF_",model[i],sep=""), 1-sqrt((get(paste("alpha_",model[i],sep=""))-1)^2+
                                                         (get(paste("beta_",model[i],sep=""))-1)^2+
                                                         (get(paste("gamma_",model[i],sep=""))-1)^2))
        
        spaef <- assign(paste("spaef_",model[i],sep=""), list(SPAEF = get(paste("SPAEF_",model[i],sep="")),
                                                              alpha = get(paste("alpha_",model[i],sep="")),
                                                              beta = get(paste("beta_",model[i],sep="")),
                                                              gamma = get(paste("gamma_",model[i],sep=""))))
      }
    }
    
    return(spaef)
  }
  
  spaef_GC <- spaef("GC")
  
  t=t+1
  
  weight_spaef = bias_tas_gc = vector(mode="list",length=321)
  weight_spaef[[t]] <- list("SPAEF" = spaef_GC, "Label attribution" = mat_gco_label_world)
  bias_tas_gc[[t]] <- list("Tas" = tas_GC, "Bias" = bias_GC
}

weight_mat_p0_2038 <- grid_search(weight_test, params=list(p0 = c(seq(0,0.01,0.0001),seq(0.01,1,0.01),seq(1.1,10,0.1),seq(11,40,1))), n.iter=1,output="list")
save(weight_mat_p0_2038, file="weight_mat_p0_2038.rdata")

