list.of.packages <- c("ncdf4","fields","maps","mapdata","RcppXPtrUtils","devtools","paramtest","overlapping","lattice","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)
install_github("thaos/gcoWrapR")
library(gcoWrapR)

load("workspace/spaef_env.rdata")
load("workspace/weight_mat_p0_2038.rdata")

spaef = alpha = beta = gamma = vector("list",321)

for(i in 1:length(weight_mat_p0_2038$results)){
  spaef[[i]] <- weight_mat_p0_2038$results[[i]]$SPAEF$SPAEF
  alpha[[i]] <- weight_mat_p0_2038$results[[i]]$SPAEF$alpha
  beta[[i]] <- weight_mat_p0_2038$results[[i]]$SPAEF$beta
  gamma[[i]] <- weight_mat_p0_2038$results[[i]]$SPAEF$gamma
  
}

plot_list <- list(spaef="spaef",alpha="alpha",beta="beta",gamma="gamma")
spaef_list <- list(spaef=spaef,alpha=alpha,beta=beta,gamma=gamma)

for(n in 1:length(plot_list)){
  plot(1:length(weight_mat_p0_2038$results),spaef_list[[n]],
       type="l",
       main=paste0(plot_list[[n]], " avec p0 = 0, 0.0001:40 et p1=1"),
       xlab = "p0",
       ylab=plot_list[[n]],
       xaxt="n")
  axis(1,
       at=c(1,51,101,151,201,241,291,306,321),
       labels = c("0","0.005","0.01","0.5","1","5","10","25","40"))
}


##### TAS AND BIAS FOR GraphCut #####

load("bias_tas_gc.rdata")

breaks_lab <- list("0","0.02","0.51","2","40") ### Correspond aux 5 cassures principales dans la courbe SPAEF
breaks_index <- list(1,52,152,211,321)

for(n in 1:length(breaks_index)){
  image.plot(lon,lat,tas_list_gc[[breaks_index[[n]]]],
             col=colorTable,
             main=paste0("30 year mean (2009-2038) temperature of graphcut with p0 = ",breaks_lab[[n]]),
             zlim=c(min(tas_list_gc[[breaks_index[[3]]]]),c(max(tas_list_gc[[breaks_index[[3]]]]))))
  map("world2",add=T)
}

for(n in 1:length(breaks_index)){
  image.plot(lon,lat,abs(bias_list_gc[[breaks_index[[n]]]]),
             col=colorTable,
             main=paste0("30 year mean (2009-2038) absolute bias temperature of graphcut with p0 = ",breaks_lab[[n]]),
             zlim=c(min(abs(bias_list_gc[[1]])),c(max(abs(bias_list_gc[[1]])))))
  map("world2",add=T)
}

##### TAS AND BIAS FOR MME #####

load("bias_tas_mme.rdata")

image.plot(lon,lat,mat_tas_mme,
           main="Temperature (°K) of multimodel mean (2009-2038)",
           zlim=c(min(tas_list_gc[[breaks_index[[3]]]]),c(max(tas_list_gc[[breaks_index[[3]]]]))),
           col=colorTable)
map("world2",add=T)

image.plot(lon,lat,mat_bias_mme,
           main="Absolute temperature bias (°C) of multimodel mean (2009-2038)",
           zlim=c(min(abs(bias_list_gc[[1]])),c(max(abs(bias_list_gc[[1]])))),
           col=colorTable)
map("world2",add=T)



##### MSE #####

mse_gc <- vector("list",321)
for(i in 1:321){
  mse_gc[[i]] <- mean(bias_list_gc[[i]]^2)
}

plot(1:321,mse_gc,type = "l",main="MSE GC",xlab = "p0", ylab="MSE",xaxt="n")
axis(1,
     at=c(1,51,101,151,201,241,291,306,321),
     labels = c("0","0.005","0.01","0.5","1","5","10","25","40"))

mse_mme <- mean(mat_bias_mme^2)