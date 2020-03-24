.libPaths("/home/users/mgarvik/GC_M2/R/x86_64-pc-linux-gnu-library/3.5")
list.of.packages <- c("ncdf4","fields","maps","mapdata","RcppXPtrUtils","devtools","paramtest","overlapping","lattice","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
install_github("thaos/gcoWrapR")
library(gcoWrapR)

setwd("/home/users/mgarvik/GC_M2/workspace/")
load("models_list.rdata")
source("load_tas_CV.R")
setwd("/home/users/mgarvik/GC_M2/workspace/")

colorTable<- designer.colors(64, c( "blue","grey90", "red"))

model_5mod <- TAS_MODELS[c(13,7,22,26,31),1]  ### Modèles utilisés (dans l'ordre) : ,IPSL,CNRM,MPI,GISS,NORESM
model_version_5mod <- TAS_MODELS[c(13,7,22,26,31),2]

weight_test <- function(iter,p0){
  for(reference in 1:length(model_version_5mod)){
    
    tas_ref = get(paste0("tas_",model_version_5mod[[reference]],"_2038"))
    
    nlabs = length(model_5mod)
    width = ncol(tas_ref)
    height = nrow(tas_ref)
    
    tas_list <- list()
    bias_list <- list()
    
    for(i in 1:length(model_version_5mod)){
      tas_list[[i]] <- get(paste0("tas_",model_version_5mod[[i]],"_2038"))
      bias_list[[i]] <- tas_list[[i]]- tas_ref
    }
    
    tas_list[[reference]] = bias_list[[reference]] = NULL
    
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
    gco$setDataCost(ptrDataCost, list(numPix = width * height, data = bias, weight = 1))
    gco$setSmoothCost(ptrSmoothCost, list(numPix = width * height, data = bias, weight = 1))
    
    mat_min <- matrix(1,nrow(tas_ref),ncol(tas_ref))
    
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
    
    tas_GC <- mat_gco_label_world
    
    for(y in 1:width){
      for(x in 1:height){
        for(i in 0:(length(tas_list)-1)){
          if(mat_gco_label_world[x,y] == i){
            tas_GC[x,y] = labs[x,y,(i+1)]
          } 
        }
      }
    }
    
    bias_GC <- mat_gco_label_world
    
    for(y in 1:width){
      for(x in 1:height){
        for(i in 0:(length(bias_list)-1)){
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
          ref_tas <- melt(tas_ref)
          
          df_temp <- melt(get(paste("tas_",model[i],sep="")))
          df_temp$type <- "Model" 
          
          assign(paste(model[i],"_tas",sep=""), df_temp)
          df_temp <- assign(paste(model[i],"_tas",sep=""), df_temp)
          ref_tas$type <- 'Reference'
          
          # and combine into your new data frame vegLengths
          tasValues <- rbind(get(paste(model[i],"_tas",sep="")), ref_tas)
          
          bw <- 2 * IQR(c(get(paste("tas_",model[i],sep="")),tas_ref))/
            length(c(get(paste("tas_",model[i],sep="")),tas_ref))^(1/3)
          
          
          ggplot(tasValues, aes(x = value, fill = type),main= "Model and ref histograms") +
            geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity',binwidth = bw)
          
          
          assign(paste("alpha_",model[i],sep=""), cor(c(get(paste("tas_",model[i],sep=""))),c(tas_ref)))
          
          assign(paste("beta_",model[i],sep=""), (sd(get(paste("tas_",model[i],sep="")))/mean(get(paste("tas_",model[i],sep=""))))/
                   (sd(tas_ref)/mean(tas_ref)))
          
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
    
    weight_spaef = bias_tas_gc = vector(mode="list",length=501)
    weight_spaef[[t]] <- list("SPAEF" = spaef_GC, "Label attribution" = mat_gco_label_world)
    bias_tas_gc[[t]] <- list("Tas" = tas_GC, "Bias" = bias_GC) 
  }
}


weight_mat_modparf_2038 <- grid_search(weight_test, params=list(p0 = c(seq(1.1,10,0.1),seq(11,40,1))), n.iter=1,output="list")
save(weight_mat_modparf_2038, file="weight_mat_modparf_2038.rdata")
