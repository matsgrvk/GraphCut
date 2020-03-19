average_abs_tas_bias <- matrix(0,
                           nrow=nrow(tas_ERAI),
                           ncol=ncol(tas_ERAI))

for(x in 1:nrow(tas_ERAI)){
  for(y in 1:ncol(tas_ERAI)){
    average_abs_tas_bias[x,y] <- mean(c(abs(bias_wrld_cnrm_annual[x,y]),
                                        abs(bias_wrld_ipsl_annual[x,y]),
                                        abs(bias_wrld_mpi_annual[x,y])))
  }
}

mme_tas_bias <- matrix(0,
                       nrow=nrow(tas_ERAI),
                       ncol=ncol(tas_ERAI))

for(x in 1:nrow(tas_ERAI)){
  for(y in 1:ncol(tas_ERAI)){
    mme_tas_bias[x,y] <- mean(c(bias_wrld_cnrm_annual[x,y],
                                bias_wrld_ipsl_annual[x,y],
                                bias_wrld_mpi_annual[x,y]))
  }
}

mme_abs_tas_bias <- abs(mme_tas_bias)

image.plot(lon,lat,average_abs_tas_bias,
           main="Average absolute temperature bias (°C) of individual models",
           xlab="Longitude",
           ylab="Latitude",
           zlim=c(min(abs(mat_gco_bias_world)),max(abs(mat_gco_bias_world))),
           col=colorTable)
map("world2",add=T)
mean(average_abs_tas_bias)

image.plot(lon,lat,mme_abs_tas_bias,
           main="Absolute temperature bias (°C) of multimodel mean",
           xlab="Longitude",
           ylab="Latitude",
           zlim=c(min(abs(mat_gco_bias_world)),max(abs(mat_gco_bias_world))),
           col=colorTable)
map("world2",add=T)
mean(mme_abs_tas_bias)

image.plot(lon,lat,abs(mat_gco_bias_world),
           main="Absolute temperature bias (°C) of GCO",
           xlab="Longitude",
           ylab="Latitude",
           zlim=c(min(abs(mat_gco_bias_world)),max(abs(mat_gco_bias_world))),
           col=colorTable)
map("world2",add=T)
mean(abs(mat_gco_bias_world))

mme_tas<- matrix(0,
                       nrow=nrow(tas_ERAI),
                       ncol=ncol(tas_ERAI))

for(x in 1:nrow(tas_ERAI)){
  for(y in 1:ncol(tas_ERAI)){
    mme_tas[x,y] <- mean(c(`tas_CNRM-CM5`[x,y],`tas_IPSL-CM5A-LR`[x,y],`tas_MPI-ESM-LR`[x,y]))
  }
}
