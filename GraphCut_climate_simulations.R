list.of.packages <- c("optrees", "ncdf4","fields","maps","mapdata","mmand")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
setwd("~/Bureau/Stage/Work_space")

source("~/Bureau/Stage/data_file.R")

mincut_list_3gcm = list()
time_list = list()
k=0

################################ Choix des données à utiliser ################################

seasons = list(saison="saison") # A choisir entre saisons ("winter","summer","spring" et "fall") ou "annual"
quant_node = seq(0.10,0.20,by=0.10) # % de biais faibles attribués aux terminaux (attention, pour le domaine "fr", les petits % ne marchent pas)
domaine = list(fr="fr") # Domaine (fr ou eur)
comb=1 # Combinaison des modèles pour un graph cut à 3 modèles (voir la matrice combinaison ci-dessous)
models = 1:3 # Nombre de modèles en entrée

################################ Boucle Graphcut ################################

combinaison = combn(models,2) # Matrice de combinaison des modèles
longitude = get(paste("lon_",domaine,"_cnrm",sep=""))
latitude = get(paste("lat_",domaine,"_cnrm",sep=""))

for (season in seasons){
  for(quant_nodes in quant_node){
    for(m in comb){
      low_bias_points = quantile(abs(get(paste(domaine,"_biais_annual",sep=""))),quant_nodes,na.rm = TRUE)
      crit_model1 <- abs(get(paste("biais_model1_",domaine,"_",season,sep=""))) <= low_bias_points
      mat_model1_low_bias <- get(paste("biais_model1_",domaine,"_",season,sep=""))
      mat_model1_low_bias[!(crit_model1)] <- NA
      
      crit_model2 <- abs(get(paste("biais_model2_",domaine,"_",season,sep=""))) <= low_bias_points
      mat_model2_low_bias <- get(paste("biais_model2_",domaine,"_",season,sep=""))
      mat_model2_low_bias[!(crit_model2)] <- NA
      
      crit_model3 <- abs(get(paste("biais_model3_",domaine,"_",season,sep=""))) <= low_bias_points
      mat_model3_low_bias <- get(paste("biais_model3_",domaine,"_",season,sep=""))
      mat_model3_low_bias[!(crit_model3)] <- NA
      
      model1 <- get(paste("biais_model1_",domaine,"_",season,sep = ""))
      model2 <- get(paste("biais_model2_",domaine,"_",season,sep = ""))
      model3 <- get(paste("biais_model3_",domaine,"_",season,sep = ""))
      
      overlap = get(paste("mat_model",combinaison[1,m],"_low_bias",sep=""))-get(paste("mat_model",combinaison[2,m],"_low_bias",sep=""))
      
      poids <- overlap
      indices <- matrix(1:length(poids),ncol=ncol(poids))
      
      mat.pad <- function(mat1){
        na1 <- matrix(NA,ncol=1,nrow=nrow(mat1))
        mat2 <- matrix(c(na1,mat1,na1),nrow=nrow(mat1),ncol=ncol(mat1)+2)
        na2 <- matrix(NA,nrow=1,ncol=ncol(mat2))
        mat3 <- rbind(na2,mat2,na2)
        return(mat3)
      }
      
      locate <- function(matrice){
        matrice2 <- mat.pad(matrice)
        x <- 1:length(matrice)
        ind_row <- 2:(nrow(matrice)+1)
        ind_col <- 2:(ncol(matrice)+1)
        neigh = rbind(N  = as.vector(matrice2[ind_row - 1, ind_col    ]),
                      E  = as.vector(matrice2[ind_row    , ind_col + 1]),
                      S  = as.vector(matrice2[ind_row + 1, ind_col    ]),
                      W  = as.vector(matrice2[ind_row    , ind_col - 1]))
        return(neigh[,x])
      }
      
      location <- locate(indices)
      colnames(location) <- 1:ncol(location)
      
      test_matgc <- function(matrice){
        n <- rep(1:length(matrice),each=4)
        x <- rep(1:4, times=length(matrice))
        test_matgc <- matrix(c(n,location[x,n]))
        return(test_matgc)
      }
      
      locate_poids <- function(matrice){
        matrice2 <- mat.pad(matrice)
        x <- 1:length(matrice)
        ind_row <- 2:(nrow(matrice)+1)
        ind_col <- 2:(ncol(matrice)+1)
        N  = abs(as.vector(matrice2[ind_row - 1, ind_col    ]) + as.vector(matrice2[ind_row,ind_col]))
        E  = abs(as.vector(matrice2[ind_row    , ind_col + 1]) + as.vector(matrice2[ind_row,ind_col]))
        S  = abs(as.vector(matrice2[ind_row + 1, ind_col    ]) + as.vector(matrice2[ind_row,ind_col]))
        W  = abs(as.vector(matrice2[ind_row    , ind_col - 1]) + as.vector(matrice2[ind_row,ind_col]))
        if (length(N) == 0) N=c(NA,NA)
        if (length(E) == 0) E=c(NA,NA)
        if (length(S) == 0) S=c(NA,NA)
        if (length(W) == 0) W=c(NA,NA)
        neigh =  rbind(N,E,S,W)   
        return(neigh[,x])
      }
      
      poids2 <- locate_poids(poids)
      
      n <- rep(1:ncol(location),each=4)
      n2 <- rep(1:ncol(poids2),each=4)
      x <- seq(1:(length(location)))
      x2 <- seq(1:(length(poids2)))
      
      test_matgc <- matrix(c(n,location[x]),ncol=2)
      test_poidsgc <- matrix(c(n2,poids2[x]),ncol=2)
      mat_gc_test <- cbind(test_matgc,test_poidsgc[,2])
      
      mat_gc <- mat_gc_test[-(which(is.na(mat_gc_test[,2]),arr.ind=TRUE)),]
      colnames(mat_gc) <- c("Noeud 1", "Noeud 2", "Poids")
      
      matriceGC2 <- subset(mat_gc, mat_gc[,1] < mat_gc[,2])
      
      nodes <- 1:(length(poids)+2)
      
      locate <- function(matrice){
        matrice2 <- mat.pad(matrice)
        x <- 1:length(matrice)
        ind_row <- 2:(nrow(matrice)+1)
        ind_col <- 2:(ncol(matrice)+1)
        neigh = rbind(N  = as.vector(matrice2[ind_row - 1, ind_col    ]),
                      E  = as.vector(matrice2[ind_row    , ind_col + 1]),
                      S  = as.vector(matrice2[ind_row + 1, ind_col    ]),
                      W  = as.vector(matrice2[ind_row    , ind_col - 1]))
        return(neigh[,x])
      }
      
      source_test_3=matrix(1:length(overlap),ncol=1)
      mat_model_source_test <- matrix(get(paste("mat_model",combinaison[2,m],"_low_bias",sep="")),ncol=1)
      for(i in 1:length(overlap)){
        if(!is.na(mat_model_source_test[i])){
          mat_model_source_test[i]=source_test_3[i]
        }
      }
      source_nodes <- mat_model_source_test[-(which(is.na(mat_model_source_test),arr.ind=TRUE)),]
      
      
      sink_test_3=matrix(1:(length(overlap)),ncol=1)
      mat_model_sink_test <- matrix(get(paste("mat_model",combinaison[1,m],"_low_bias",sep="")),ncol=1)
      for(i in 1:(length(overlap))){
        if(!is.na(mat_model_sink_test[i])){
          mat_model_sink_test[i]=sink_test_3[i]
        }
      }
      sink_nodes <- mat_model_sink_test[-(which(is.na(mat_model_sink_test),arr.ind=TRUE)),]
      
      diff = matrix((mat_model1_low_bias)-(mat_model1_low_bias),ncol=1)
      
      mat_terminaux <- c(source_nodes,sink_nodes)
      matrice_test_source <- matrix(seq(1:length(overlap)),nrow=nrow(overlap))
      
      for (i in 1:length(overlap)){
        if ((i %in% source_nodes) & (i %in% matrice_test_source)){
          matrice_test_source[matrice_test_source==i] = 1
        }
        else matrice_test_source[matrice_test_source==i] = 0
      }
      
      kern = c(1,1,1)
      
      matrice_source_eroded <- erode(matrice_test_source,kern)
      
      matrice_test_sink <- matrix(seq(1:length(overlap)),nrow=nrow(overlap))
      
      
      for (i in 1:length(overlap)){
        if ((i %in% sink_nodes) & (i %in% matrice_test_sink)){
          matrice_test_sink[matrice_test_sink==i] = 1
        }
        else matrice_test_sink[matrice_test_sink==i] = 0
      }
      
      matrice_sink_eroded <- erode(matrice_test_sink,kern)
      
      for(i in 1:length(overlap)){
        if(matrice_source_eroded[i] !=0 ){
          matrice_source_eroded[i]=get(paste("mat_model",combinaison[2,m],"_low_bias",sep=""))[i]
        }
      }
      for(i in 1:length(overlap)){
        if(matrice_sink_eroded[i] !=0 ){
          matrice_sink_eroded[i]=get(paste("mat_model",combinaison[1,m],"_low_bias",sep=""))[i]
        }
      }
      
      source_test_indices_eroded <- matrix(NA, nrow=nrow(overlap), ncol=ncol(overlap))
      sink_test_indices_eroded <- matrix(NA, nrow=nrow(overlap), ncol=ncol(overlap))
      
      for (i in 1:length(overlap)){
        if(matrice_source_eroded[i] !=0 ) {
          source_test_indices_eroded[i]=i
        }
      }
      
      for (i in 1:length(overlap)){
        if(matrice_sink_eroded[i] !=0 ){
          sink_test_indices_eroded[i]=i
        }
      }
      
      
      ################################ Choix d'attribution des nœuds aux terminaux ################################
      
      no_st <- for(i in 1:length(overlap)){
        if((i %in% source_test_indices_eroded) & (i %in% sink_test_indices_eroded)){
          source_test_indices_eroded[source_test_indices_eroded==i] <- NA
          sink_test_indices_eroded[sink_test_indices_eroded==i] <- NA
        }
      }
      
      diff = matrix(get(paste("mat_model",combinaison[2,m],"_low_bias",sep=""))-get(paste("mat_model",combinaison[1,m],"_low_bias",sep="")),ncol=1)
      
      st_smallest <- for(i in 1:length(overlap)){
        if((i %in% source_test_indices_eroded) & (i %in% sink_test_indices_eroded)){
          if(diff[i] < 0){
            sink_test_indices_eroded[sink_test_indices_eroded==i] <- NA
          }
          else source_test_indices_eroded[source_test_indices_eroded==i] <- NA
        }
      }
      
      st_smallest
      
      source_test_indices_eroded <- source_test_indices_eroded[which(!is.na(source_test_indices_eroded))]
      sink_test_indices_eroded <- sink_test_indices_eroded[which(!is.na(sink_test_indices_eroded))]
      source <- cbind(c(rep((length(nodes)-1),each=(length(source_test_indices_eroded)))),
                      c(source_test_indices_eroded),
                      10000)
      colnames(source) <- c("Noeud 1", "Noeud 2","Poids")
      sink <- cbind(c(rep((length(nodes)),each=(length(sink_test_indices_eroded)))),
                    c(sink_test_indices_eroded),
                    10000)
      colnames(sink) <- c("Noeud 1", "Noeud 2","Poids")
      
      matriceGC_eroded <- rbind(source,matriceGC2,sink)
      
      MinCut_2gcm = findMinCut(nodes,matriceGC_eroded,source.node = (length(nodes)-1),sink.node = length(nodes))
      
      s_nodes_2gcm <- MinCut_2gcm[1]
      s_nodes_2gcm <- matrix(unlist(s_nodes_2gcm),ncol=1)
      s_nodes_2gcm <- matrix(s_nodes_2gcm[2:length(s_nodes_2gcm)],ncol=1)
      s_nodes_2gcm <- matrix(sort(s_nodes_2gcm),ncol=1)
      
      t_nodes_2gcm <- MinCut_2gcm[2]
      t_nodes_2gcm <- matrix(unlist(t_nodes_2gcm),ncol=1)
      t_nodes_2gcm <- matrix(t_nodes_2gcm[1:(length(t_nodes_2gcm)-1)],ncol=1)
      t_nodes_2gcm <- matrix(sort(t_nodes_2gcm),ncol=1)
      
      gc_test_2gcm <- matrix(1:length(overlap),nrow=nrow(overlap))
      for(i in 1:length(overlap)){
        if (i %in% s_nodes_2gcm){
          gc_test_2gcm[i]=get(paste("model",combinaison[2,m],sep=""))[i]
        }
        else gc_test_2gcm[i]=get(paste("model",combinaison[1,m],sep=""))[i]
      }
      
      
      for(y in 1:3){
        if ((y %in% combinaison[,m]) == FALSE){
          z = y
        }
      }
      
      overlap = gc_test_2gcm-get(paste("model",y,sep=""))
      
      poids <- overlap
      indices <- matrix(1:length(poids),ncol=ncol(poids))
      
      mat.pad <- function(mat1){
        na1 <- matrix(NA,ncol=1,nrow=nrow(mat1))
        mat2 <- matrix(c(na1,mat1,na1),nrow=nrow(mat1),ncol=ncol(mat1)+2)
        na2 <- matrix(NA,nrow=1,ncol=ncol(mat2))
        mat3 <- rbind(na2,mat2,na2)
        return(mat3)
      }
      
      locate <- function(matrice){
        matrice2 <- mat.pad(matrice)
        x <- 1:length(matrice)
        ind_row <- 2:(nrow(matrice)+1)
        ind_col <- 2:(ncol(matrice)+1)
        neigh = rbind(N  = as.vector(matrice2[ind_row - 1, ind_col    ]),
                      E  = as.vector(matrice2[ind_row    , ind_col + 1]),
                      S  = as.vector(matrice2[ind_row + 1, ind_col    ]),
                      W  = as.vector(matrice2[ind_row    , ind_col - 1]))
        return(neigh[,x])
      }
      
      location <- locate(indices)
      colnames(location) <- 1:ncol(location)
      
      test_matgc <- function(matrice){
        n <- rep(1:length(matrice),each=4)
        x <- rep(1:4, times=length(matrice))
        test_matgc <- matrix(c(n,location[x,n]))
        return(test_matgc)
      }
      
      locate_poids <- function(matrice){
        matrice2 <- mat.pad(matrice)
        x <- 1:length(matrice)
        ind_row <- 2:(nrow(matrice)+1)
        ind_col <- 2:(ncol(matrice)+1)
        N  = abs(as.vector(matrice2[ind_row - 1, ind_col    ]) + as.vector(matrice2[ind_row,ind_col]))
        E  = abs(as.vector(matrice2[ind_row    , ind_col + 1]) + as.vector(matrice2[ind_row,ind_col]))
        S  = abs(as.vector(matrice2[ind_row + 1, ind_col    ]) + as.vector(matrice2[ind_row,ind_col]))
        W  = abs(as.vector(matrice2[ind_row    , ind_col - 1]) + as.vector(matrice2[ind_row,ind_col]))
        if (length(N) == 0) N=c(NA,NA)
        if (length(E) == 0) E=c(NA,NA)
        if (length(S) == 0) S=c(NA,NA)
        if (length(W) == 0) W=c(NA,NA)
        neigh =  rbind(N,E,S,W)   
        return(neigh[,x])
      }
      
      poids2 <- locate_poids(poids)
      
      n <- rep(1:ncol(location),each=4)
      n2 <- rep(1:ncol(poids2),each=4)
      x <- seq(1:(length(location)))
      x2 <- seq(1:(length(poids2)))
      
      test_matgc <- matrix(c(n,location[x]),ncol=2)
      test_poidsgc <- matrix(c(n2,poids2[x]),ncol=2)
      mat_gc_test <- cbind(test_matgc,test_poidsgc[,2])
      
      mat_gc <- mat_gc_test[-(which(is.na(mat_gc_test[,2]),arr.ind=TRUE)),]
      colnames(mat_gc) <- c("Noeud 1", "Noeud 2", "Poids")
      
      matriceGC2 <- subset(mat_gc, mat_gc[,1] < mat_gc[,2])
      
      nodes <- 1:(length(poids)+2)
      
      locate <- function(matrice){
        matrice2 <- mat.pad(matrice)
        x <- 1:length(matrice)
        ind_row <- 2:(nrow(matrice)+1)
        ind_col <- 2:(ncol(matrice)+1)
        neigh = rbind(N  = as.vector(matrice2[ind_row - 1, ind_col    ]),
                      E  = as.vector(matrice2[ind_row    , ind_col + 1]),
                      S  = as.vector(matrice2[ind_row + 1, ind_col    ]),
                      W  = as.vector(matrice2[ind_row    , ind_col - 1]))
        return(neigh[,x])
      }
      
      source_test_3=matrix(1:length(overlap),ncol=1)
      
      mat_model_source2_test <- matrix(get(paste("mat_model",y,"_low_bias",sep="")),ncol=1)
      
      for(i in 1:length(overlap)){
        if(!is.na(mat_model_source2_test[i])){
          mat_model_source2_test[i]=source_test_3[i]
        }
      }
      source_nodes <- mat_model_source2_test[-(which(is.na(mat_model_source2_test),arr.ind=TRUE)),]
      
      crit_3gcm <- abs(gc_test_2gcm) <= low_bias_points
      mat_gc3_low_bias <- gc_test_2gcm
      mat_gc3_low_bias[!(crit_3gcm)] <- NA
      
      
      sink_test_3=matrix(1:(length(overlap)),ncol=1)
      gc_test_2gcm2 <- matrix(mat_gc3_low_bias,ncol=1)
      for(i in 1:(length(overlap))){
        if(!is.na(gc_test_2gcm2[i])){
          gc_test_2gcm2[i]=sink_test_3[i]
        }
      }
      sink_nodes <- gc_test_2gcm2[-(which(is.na(gc_test_2gcm2),arr.ind=TRUE)),]
      
      mat_terminaux <- c(source_nodes,sink_nodes)
      matrice_test_source <- matrix(seq(1:length(overlap)),nrow=nrow(overlap))
      
      for (i in 1:length(overlap)){
        if ((i %in% source_nodes) & (i %in% matrice_test_source)){
          matrice_test_source[matrice_test_source==i] = 1
        }
        else matrice_test_source[matrice_test_source==i] = 0
      }
      
      kern = c(1,1,1)
      
      matrice_source_eroded <- erode(matrice_test_source,kern)
      
      matrice_test_sink <- matrix(seq(1:length(overlap)),nrow=nrow(overlap))
      
      
      for (i in 1:length(overlap)){
        if ((i %in% sink_nodes) & (i %in% matrice_test_sink)){
          matrice_test_sink[matrice_test_sink==i] = 1
        }
        else matrice_test_sink[matrice_test_sink==i] = 0
      }
      
      matrice_sink_eroded <- erode(matrice_test_sink,kern)
      
      for(i in 1:length(overlap)){
        if(matrice_source_eroded[i] !=0 ){
          mat_model_source2_test <- matrix(get(paste("mat_model",y,"_low_bias",sep="")),ncol=1)
        }
      }
      for(i in 1:length(overlap)){
        if(matrice_sink_eroded[i] !=0 ){
          matrice_sink_eroded[i]=mat_gc3_low_bias[i]
        }
      }
      
      source_test_indices_eroded <- matrix(NA, nrow=nrow(overlap), ncol=ncol(overlap))
      sink_test_indices_eroded <- matrix(NA, nrow=nrow(overlap), ncol=ncol(overlap))
      
      for (i in 1:length(overlap)){
        if(matrice_source_eroded[i] !=0 ){
          source_test_indices_eroded[i]=i
        }
      }
      
      for (i in 1:length(overlap)){
        if(matrice_sink_eroded[i] !=0 ){
          sink_test_indices_eroded[i]=i
        }
      }
      
      
      ################################ Choix d'attribution des nœuds aux terminaux ################################
      
      no_st <- for(i in 1:length(overlap)){
        if((i %in% source_test_indices_eroded) & (i %in% sink_test_indices_eroded)){
          source_test_indices_eroded[source_test_indices_eroded==i] <- NA
          sink_test_indices_eroded[sink_test_indices_eroded==i] <- NA
        }
      }
      
      diff = matrix(gc_test_2gcm-(get(paste("mat_model",y,"_low_bias",sep=""))),ncol=1)
      
      st_smallest <- for(i in 1:length(overlap)){
        if((i %in% source_test_indices_eroded) & (i %in% sink_test_indices_eroded)){
          if(diff[i] < 0){
            sink_test_indices_eroded[sink_test_indices_eroded==i] <- NA
          }
          else source_test_indices_eroded[source_test_indices_eroded==i] <- NA
        }
      }
      
      st_smallest
      
      source_test_indices_eroded <- source_test_indices_eroded[which(!is.na(source_test_indices_eroded))]
      sink_test_indices_eroded <- sink_test_indices_eroded[which(!is.na(sink_test_indices_eroded))]
      source <- cbind(c(rep((length(nodes)-1),each=(length(source_test_indices_eroded)))),
                      c(source_test_indices_eroded),
                      10000)
      colnames(source) <- c("Noeud 1", "Noeud 2","Poids")
      sink <- cbind(c(rep((length(nodes)),each=(length(sink_test_indices_eroded)))),
                    c(sink_test_indices_eroded),
                    10000)
      colnames(sink) <- c("Noeud 1", "Noeud 2","Poids")
      
      matriceGC_eroded <- rbind(source,matriceGC2,sink)
      
      k=k+1
      
      start_time <- Sys.time()
      
      mincut_list_3gcm[[k]] = findMinCut(nodes,matriceGC_eroded,source.node = (length(nodes)-1),sink.node = length(nodes))
      
      end_time <- Sys.time()
      
      time_list[[k]] = end_time - start_time
      
      
      s_nodes_3gcm <- mincut_list_3gcm[[k]][1]
      s_nodes_3gcm <- matrix(unlist(s_nodes_3gcm),ncol=1)
      s_nodes_3gcm <- matrix(s_nodes_3gcm[2:length(s_nodes_3gcm)],ncol=1)
      s_nodes_3gcm <- matrix(sort(s_nodes_3gcm),ncol=1)
      
      t_nodes_3gcm <- mincut_list_3gcm[[k]][2]
      t_nodes_3gcm <- matrix(unlist(t_nodes_3gcm),ncol=1)
      t_nodes_3gcm <- matrix(t_nodes_3gcm[1:(length(t_nodes_3gcm)-1)],ncol=1)
      t_nodes_3gcm <- matrix(sort(t_nodes_3gcm),ncol=1)
      
      gc_test_3gcm <- matrix(1:length(overlap),nrow=nrow(overlap))
      for(i in 1:length(overlap)){
        if (i %in% s_nodes_3gcm){
          gc_test_3gcm[i]=model3[i]
        }
        else gc_test_3gcm[i]=gc_test_2gcm[i]
      }
      
      name_model=c("CNRM","IPSL","MPI")
      paste(name_model[combinaison[,m]],sep="")
      name_model1 <- paste(name_model[combinaison[,m]],sep="")[1]
      name_model2 <- paste(name_model[combinaison[,m]],sep="")[2]
      name_model3 <- paste(name_model[z])
      save(gc_test_3gcm, file = paste("matrice_graphcut_Q",quant_nodes,"_",season,"_",name_model1,"_",name_model2,"_",name_model3,"_","_",domaine,".RData",sep = ""))
      pdf(paste("Q",quant_nodes,"_","multi","_","mean","_",season,"_",name_model1,"_",name_model2,"_",name_model3,"_",domaine,".pdf",sep = ""),width = 10,height = 15)
      image.plot(longitude,latitude,gc_test_3gcm,
                 main=paste("Carte GC biais ",name_model1,"/",name_model2,"/",name_model3," - ",season,sep=" "),
                 xlab="Longitude",
                 ylab="Latitude",
                 zlim=c(min(get(paste(domaine,"_biais_",season,sep=""))),max(get(paste(domaine,"_biais_",season,sep="")))),
                 col=colorTable)
      lines(get(paste(domaine)))
      mtext(paste("Pourcentage de points de grille correspondant à un biais faible attribués aux terminaux : ",quant_nodes*100,"%",sep=""),side=1,line = 4)
      
      x_pts <- matrix(seq(min(longitude),max(longitude),by=0.75),nrow=length(gc_test_3gcm))
      y_pts <- matrix(rep(seq(min(latitude),max(latitude),by=0.75),each=nrow(gc_test_3gcm)))
      gc_points <- matrix(1:length(gc_test_3gcm),nrow=length(gc_test_3gcm))
      
      target_points_3gcm <- mincut_list_3gcm[[k]][2]
      mat_target_3gcm <- as.matrix(unlist(target_points_3gcm))
      mat_target_3gcm <- matrix(mat_target_3gcm[1:length(mat_target_3gcm)-1])
      
      a=rep(NA,length(overlap))
      
      for(i in 1:(length(overlap))){
        if(i %in% mat_target_3gcm){
          a[i]=i
        }
      }
      
      mat_target_points <- matrix(a,ncol=1)
      
      source_points_3gcm <- mincut_list_3gcm[[k]][1]
      mat_source_3gcm <- as.matrix(unlist(source_points_3gcm))
      mat_source_3gcm <- matrix(mat_source_3gcm[2:length(mat_source_3gcm)])
      
      b=rep(NA,length(overlap))
      
      for(i in 1:(length(overlap))){
        if(i %in% mat_source_3gcm){
          b[i]=i
        }
      }
      
      mat_source_points <- matrix(b,ncol=1)
      
      mat_target_points <- cbind(gc_points,x_pts,y_pts)
      mat_model_source2_test2 <- mat_model_source2_test
      mat_model_source2_test2[which(is.na(mat_model_source2_test2))] <- 0
      crit_target <- mat_target_points[,1]==mat_model_source2_test2[,1]
      mat_target_points[!(crit_target)] <- NA
      
      mat_source_points <- cbind(gc_points,x_pts,y_pts)
      mat_model_source_test2 <- mat_model_source_test
      mat_model_source_test2[which(is.na(mat_model_source_test2))] <- 0
      crit_source <- mat_source_points[,1]==mat_model_source_test2[,1]
      mat_source_points[!(crit_source)] <- NA
      
      for(i in 1:length(overlap)){
        if((i %in% mat_target_points[,1]) & (i %in% mat_source_points[,1])){
          mat_source_points[i,] <- NA
          mat_target_points[i,] <- NA
        }
      }
      
      
      
      cut_set_3gcm <- mincut_list_3gcm[[k]][4]
      cut_set_3gcm <- matrix(unlist(cut_set_3gcm),ncol=3)
      cut_set_3gcm <- cut_set_3gcm[,1:2]
      
      
      mat_cut2_3gcm <- cut_set_3gcm
      
      for(i in 1:nrow(mat_cut2_3gcm)) {
        if (mat_cut2_3gcm[i,1] > mat_cut2_3gcm[i,2]) {
          mat_cut2_3gcm[i,1]=cut_set_3gcm[i,2]
          mat_cut2_3gcm[i,2]=cut_set_3gcm[i,1]
        }
      }
      
      mat_cut_3gcm <- mat_cut2_3gcm
      
      
      mat_lon <- matrix(rep(seq(min(longitude),max(longitude),by=0.75),each=ncol(overlap)),ncol=ncol(overlap),byrow=TRUE)
      mat_lat <- matrix(rep(seq(min(latitude),max(latitude),by=0.75),each=nrow(overlap)),ncol=nrow(overlap))
      
      for(i in 1:nrow(mat_cut_3gcm)){
        if((mat_cut_3gcm[i,2]-mat_cut_3gcm[i,1]) == 1){
          segments((mat_lon[mat_cut_3gcm[i]]+0.375),
                   (mat_lat[mat_cut_3gcm[i]]-0.375),
                   (mat_lon[mat_cut_3gcm[i+nrow(mat_cut_3gcm)]]-0.375),
                   (mat_lat[mat_cut_3gcm[i+nrow(mat_cut_3gcm)]]+0.375),
                   lty="dashed",
                   lwd=1)
        }
        else segments((mat_lon[mat_cut_3gcm[i]]-0.375),
                      (mat_lat[mat_cut_3gcm[i]]+0.375),
                      (mat_lon[mat_cut_3gcm[i+nrow(mat_cut_3gcm)]]+0.375),
                      (mat_lat[mat_cut_3gcm[i+nrow(mat_cut_3gcm)]]-0.375),
                      lty="dashed",
                      lwd=1)
      }
      dev.off()
    }
  }
}