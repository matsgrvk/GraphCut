list.of.packages <- c("optrees", "ncdf4","fields","maps","mapdata","mmand")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, library, character.only = TRUE)

# Chargement des donneés issues de fichiers netCDF --------

nc <- nc_open("Work_space/t2_erai_clim_1979_2008.nc")
nc_erai_winter <- nc_open("Work_space/t2_erai_clim_1979_2008_DJF.nc")
nc_erai_summer <- nc_open("Work_space/t2_erai_clim_1979_2008_JJA.nc")
nc_erai_spring <- nc_open("Work_space/t2_erai_clim_1979_2008_MAM.nc")
nc_erai_fall <- nc_open("Work_space/t2_erai_clim_1979_2008_SON.nc")

tas_erai_annual = ncvar_get(nc, varid = "t2")
tas_erai_winter = ncvar_get(nc_erai_winter, varid = "t2")
tas_erai_summer = ncvar_get(nc_erai_summer, varid = "t2")
tas_erai_spring = ncvar_get(nc_erai_spring, varid = "t2")
tas_erai_fall = ncvar_get(nc_erai_fall, varid = "t2")

lat_erai = ncvar_get(nc, varid = "lat")
lon_erai = ncvar_get(nc, varid = "lon")

tas_erai_annual = tas_erai_annual[, length(lat_erai) : 1]
tas_erai_winter = tas_erai_winter[, length(lat_erai) : 1]
tas_erai_summer = tas_erai_summer[, length(lat_erai) : 1]
tas_erai_spring = tas_erai_spring[, length(lat_erai) : 1]
tas_erai_fall = tas_erai_fall[, length(lat_erai) : 1]

lat_erai = rev(lat_erai)

nc_close(nc)
nc_close(nc_erai_winter)
nc_close(nc_erai_summer)
nc_close(nc_erai_spring)
nc_close(nc_erai_fall)

nc2 <- nc_open("Work_space/tas_cnrm_clim_1979_2008.nc")
nc_cnrm_winter <- nc_open("Work_space/tas_cnrm_clim_1979_2008_DJF.nc")
nc_cnrm_summer <- nc_open("Work_space/tas_cnrm_clim_1979_2008_JJA.nc")
nc_cnrm_spring <- nc_open("Work_space/tas_cnrm_clim_1979_2008_MAM.nc")
nc_cnrm_fall <- nc_open("Work_space/tas_cnrm_clim_1979_2008_SON.nc")

tas_cnrm_annual = ncvar_get(nc2, varid = "tas")
tas_cnrm_winter = ncvar_get(nc_cnrm_winter, varid = "tas")
tas_cnrm_summer = ncvar_get(nc_cnrm_summer, varid = "tas")
tas_cnrm_spring = ncvar_get(nc_cnrm_spring, varid = "tas")
tas_cnrm_fall = ncvar_get(nc_cnrm_fall, varid = "tas")

lat_cnrm = ncvar_get(nc2, varid = "lat")
lon_cnrm = ncvar_get(nc2, varid = "lon")

tas_cnrm_annual = tas_cnrm_annual[, length(lat_cnrm) : 1]
tas_cnrm_winter = tas_cnrm_winter[, length(lat_cnrm) : 1]
tas_cnrm_summer = tas_cnrm_summer[, length(lat_cnrm) : 1]
tas_cnrm_spring = tas_cnrm_spring[, length(lat_cnrm) : 1]
tas_cnrm_fall = tas_cnrm_fall[, length(lat_cnrm) : 1]

lat_cnrm = rev(lat_cnrm)

nc_close(nc2)
nc_close(nc_cnrm_winter)
nc_close(nc_cnrm_summer)
nc_close(nc_cnrm_spring)
nc_close(nc_cnrm_fall)

nc3 <- nc_open("Work_space/tas_ipsl_clim_1979_2008.nc")
nc_ipsl_winter <- nc_open("Work_space/tas_ipsl_clim_1979_2008_DJF.nc")
nc_ipsl_summer <- nc_open("Work_space/tas_ipsl_clim_1979_2008_JJA.nc")
nc_ipsl_spring <- nc_open("Work_space/tas_ipsl_clim_1979_2008_MAM.nc")
nc_ipsl_fall <- nc_open("Work_space/tas_ipsl_clim_1979_2008_SON.nc")

tas_ipsl_annual = ncvar_get(nc3, varid = "tas")
tas_ipsl_winter = ncvar_get(nc_ipsl_winter, varid = "tas")
tas_ipsl_summer = ncvar_get(nc_ipsl_summer, varid = "tas")
tas_ipsl_spring = ncvar_get(nc_ipsl_spring, varid = "tas")
tas_ipsl_fall = ncvar_get(nc_ipsl_fall, varid = "tas")

lat_ipsl = ncvar_get(nc3, varid = "lat")
lon_ipsl = ncvar_get(nc3, varid = "lon")

tas_ipsl_annual = tas_ipsl_annual[, length(lat_ipsl) : 1]
tas_ipsl_winter = tas_ipsl_winter[, length(lat_ipsl) : 1]
tas_ipsl_summer = tas_ipsl_summer[, length(lat_ipsl) : 1]
tas_ipsl_spring = tas_ipsl_spring[, length(lat_ipsl) : 1]
tas_ipsl_fall = tas_ipsl_fall[, length(lat_ipsl) : 1]
lat_ipsl = rev(lat_ipsl)

nc_close(nc3)
nc_close(nc_ipsl_winter)
nc_close(nc_ipsl_summer)
nc_close(nc_ipsl_spring)
nc_close(nc_ipsl_fall)

nc4 <- nc_open("Work_space/tas_mpi_clim_1979-01-01,2008-12-31.nc")
nc_mpi_winter <- nc_open("Work_space/tas_mpi_clim_1979-01-01,2008-12-31_DJF.nc")
nc_mpi_summer <- nc_open("Work_space/tas_mpi_clim_1979-01-01,2008-12-31_JJA.nc")
nc_mpi_spring <- nc_open("Work_space/tas_mpi_clim_1979-01-01,2008-12-31_MAM.nc")
nc_mpi_fall <- nc_open("Work_space/tas_mpi_clim_1979-01-01,2008-12-31_SON.nc")

tas_mpi_annual = ncvar_get(nc4, varid = "tas")
tas_mpi_winter = ncvar_get(nc_mpi_winter, varid = "tas")
tas_mpi_summer = ncvar_get(nc_mpi_summer, varid = "tas")
tas_mpi_spring = ncvar_get(nc_mpi_spring, varid = "tas")
tas_mpi_fall = ncvar_get(nc_mpi_fall, varid = "tas")

lat_mpi = ncvar_get(nc4, varid = "lat")
lon_mpi = ncvar_get(nc4, varid = "lon")

tas_mpi_annual = tas_mpi_annual[, length(lat_mpi) : 1]
tas_mpi_winter = tas_mpi_winter[, length(lat_mpi) : 1]
tas_mpi_summer = tas_mpi_summer[, length(lat_mpi) : 1]
tas_mpi_spring = tas_mpi_spring[, length(lat_mpi) : 1]
tas_mpi_fall = tas_mpi_fall[, length(lat_mpi) : 1]
lat_mpi = rev(lat_mpi)

cnrm_biais_tas_annual <- tas_cnrm_annual-tas_erai_annual 
cnrm_biais_tas_winter <- tas_cnrm_winter-tas_erai_winter
cnrm_biais_tas_summer <- tas_cnrm_summer-tas_erai_summer
cnrm_biais_tas_spring <- tas_cnrm_spring-tas_erai_spring
cnrm_biais_tas_fall <- tas_cnrm_fall-tas_erai_fall

ipsl_biais_tas_annual <- tas_ipsl_annual-tas_erai_annual
ipsl_biais_tas_winter <- tas_ipsl_winter-tas_erai_winter
ipsl_biais_tas_summer <- tas_ipsl_summer-tas_erai_summer
ipsl_biais_tas_spring <- tas_ipsl_spring-tas_erai_spring
ipsl_biais_tas_fall <- tas_ipsl_fall-tas_erai_fall

mpi_biais_tas_annual <- tas_mpi_annual-tas_erai_annual
mpi_biais_tas_winter <- tas_mpi_winter-tas_erai_winter
mpi_biais_tas_summer <- tas_mpi_summer-tas_erai_summer
mpi_biais_tas_spring <- tas_mpi_spring-tas_erai_spring
mpi_biais_tas_fall <- tas_mpi_fall-tas_erai_fall


nc5 <- nc_open("Work_space/tp_erai_clim_1979_2008.nc")
nc_pr_erai_winter <- nc_open("Work_space/tp_erai_clim_1979_2008_DJF.nc")
nc_pr_erai_summer <- nc_open("Work_space/tp_erai_clim_1979_2008_JJA.nc")
nc_pr_erai_spring <- nc_open("Work_space/tp_erai_clim_1979_2008_MAM.nc")
nc_pr_erai_fall <- nc_open("Work_space/tp_erai_clim_1979_2008_SON.nc")

pr_erai_annual = ncvar_get(nc5, varid = "tp")
pr_erai_winter = ncvar_get(nc_pr_erai_winter, varid = "tp")
pr_erai_summer = ncvar_get(nc_pr_erai_summer, varid = "tp")
pr_erai_spring = ncvar_get(nc_pr_erai_spring, varid = "tp")
pr_erai_fall = ncvar_get(nc_pr_erai_fall, varid = "tp")

lat_erai = ncvar_get(nc5, varid = "latitude")
lon_erai = ncvar_get(nc5, varid = "longitude")

pr_erai_annual = pr_erai_annual[, length(lat_erai) : 1]
pr_erai_winter = pr_erai_winter[, length(lat_erai) : 1]
pr_erai_summer = pr_erai_summer[, length(lat_erai) : 1]
pr_erai_spring = pr_erai_spring[, length(lat_erai) : 1]
pr_erai_fall = pr_erai_fall[, length(lat_erai) : 1]

lat_erai = rev(lat_erai)

nc_close(nc5)
nc_close(nc_pr_erai_winter)
nc_close(nc_pr_erai_summer)
nc_close(nc_pr_erai_spring)
nc_close(nc_pr_erai_fall)

nc6 <- nc_open("Work_space/pr_cnrm_clim_1979-01-01,2008-12-31.nc")
nc_pr_cnrm_winter <- nc_open("Work_space/pr_cnrm_clim_1979-01-01,2008-12-31_DJF.nc")
nc_pr_cnrm_summer <- nc_open("Work_space/pr_cnrm_clim_1979-01-01,2008-12-31_JJA.nc")
nc_pr_cnrm_spring <- nc_open("Work_space/pr_cnrm_clim_1979-01-01,2008-12-31_MAM.nc")
nc_pr_cnrm_fall <- nc_open("Work_space/pr_cnrm_clim_1979-01-01,2008-12-31_SON.nc")

pr_cnrm_annual = ncvar_get(nc6, varid = "pr")
pr_cnrm_winter = ncvar_get(nc_pr_cnrm_winter, varid = "pr")
pr_cnrm_summer = ncvar_get(nc_pr_cnrm_summer, varid = "pr")
pr_cnrm_spring = ncvar_get(nc_pr_cnrm_spring, varid = "pr")
pr_cnrm_fall = ncvar_get(nc_pr_cnrm_fall, varid = "pr")

lat_cnrm = ncvar_get(nc6, varid = "lat")
lon_cnrm = ncvar_get(nc6, varid = "lon")

pr_cnrm_annual = pr_cnrm_annual[, length(lat_cnrm) : 1]
pr_cnrm_winter = pr_cnrm_winter[, length(lat_cnrm) : 1]
pr_cnrm_summer = pr_cnrm_summer[, length(lat_cnrm) : 1]
pr_cnrm_spring = pr_cnrm_spring[, length(lat_cnrm) : 1]
pr_cnrm_fall = pr_cnrm_fall[, length(lat_cnrm) : 1]
lat_cnrm = rev(lat_cnrm)

nc_close(nc6)
nc_close(nc_pr_cnrm_winter)
nc_close(nc_pr_cnrm_summer)
nc_close(nc_pr_cnrm_spring)
nc_close(nc_pr_cnrm_fall)

nc7 <- nc_open("Work_space/pr_ipsl_clim_1979-01-01,2008-12-31.nc")
nc_pr_ipsl_winter <- nc_open("Work_space/pr_ipsl_clim_1979-01-01,2008-12-31_DJF.nc")
nc_pr_ipsl_summer <- nc_open("Work_space/pr_ipsl_clim_1979-01-01,2008-12-31_JJA.nc")
nc_pr_ipsl_spring <- nc_open("Work_space/pr_ipsl_clim_1979-01-01,2008-12-31_MAM.nc")
nc_pr_ipsl_fall <- nc_open("Work_space/pr_ipsl_clim_1979-01-01,2008-12-31_SON.nc")

pr_ipsl_annual = ncvar_get(nc7, varid = "pr")
pr_ipsl_winter = ncvar_get(nc_pr_ipsl_winter, varid = "pr")
pr_ipsl_summer = ncvar_get(nc_pr_ipsl_summer, varid = "pr")
pr_ipsl_spring = ncvar_get(nc_pr_ipsl_spring, varid = "pr")
pr_ipsl_fall = ncvar_get(nc_pr_ipsl_fall, varid = "pr")

lat_ipsl = ncvar_get(nc7, varid = "lat")
lon_ipsl = ncvar_get(nc7, varid = "lon")

pr_ipsl_annual = pr_ipsl_annual[, length(lat_ipsl) : 1]
pr_ipsl_winter = pr_ipsl_winter[, length(lat_ipsl) : 1]
pr_ipsl_summer = pr_ipsl_summer[, length(lat_ipsl) : 1]
pr_ipsl_spring = pr_ipsl_spring[, length(lat_ipsl) : 1]
pr_ipsl_fall = pr_ipsl_fall[, length(lat_ipsl) : 1]
lat_ipsl = rev(lat_ipsl)

nc_close(nc7)
nc_close(nc_pr_ipsl_winter)
nc_close(nc_pr_ipsl_summer)
nc_close(nc_pr_ipsl_spring)
nc_close(nc_pr_ipsl_fall)

nc8 <- nc_open("Work_space/pr_mpi_clim_1979-01-01,2008-12-31.nc")
nc_pr_mpi_winter <- nc_open("Work_space/pr_mpi_clim_1979-01-01,2008-12-31_DJF.nc")
nc_pr_mpi_summer <- nc_open("Work_space/pr_mpi_clim_1979-01-01,2008-12-31_JJA.nc")
nc_pr_mpi_spring <- nc_open("Work_space/pr_mpi_clim_1979-01-01,2008-12-31_MAM.nc")
nc_pr_mpi_fall <- nc_open("Work_space/pr_mpi_clim_1979-01-01,2008-12-31_SON.nc")

pr_mpi_annual = ncvar_get(nc8, varid = "pr")
pr_mpi_winter = ncvar_get(nc_pr_ipsl_winter, varid = "pr")
pr_mpi_summer = ncvar_get(nc_pr_ipsl_summer, varid = "pr")
pr_mpi_spring = ncvar_get(nc_pr_ipsl_spring, varid = "pr")
pr_mpi_fall = ncvar_get(nc_pr_ipsl_fall, varid = "pr")

lat_mpi = ncvar_get(nc8, varid = "lat")
lon_mpi = ncvar_get(nc8, varid = "lon")

pr_mpi_annual = pr_mpi_annual[, length(lat_mpi) : 1]
pr_mpi_winter = pr_mpi_winter[, length(lat_mpi) : 1]
pr_mpi_summer = pr_mpi_summer[, length(lat_mpi) : 1]
pr_mpi_spring = pr_mpi_spring[, length(lat_mpi) : 1]
pr_mpi_fall = pr_mpi_fall[, length(lat_mpi) : 1]
lat_mpi = rev(lat_mpi)

pr_erai_annual = pr_erai_annual*1000
pr_erai_winter = pr_erai_winter * 1000
pr_erai_summer = pr_erai_summer * 1000
pr_erai_spring = pr_erai_spring * 1000
pr_erai_fall = pr_erai_fall * 1000

pr_cnrm_annual = pr_cnrm_annual*86400
pr_cnrm_winter = pr_cnrm_winter * 86400
pr_cnrm_summer = pr_cnrm_summer * 86400
pr_cnrm_spring = pr_cnrm_spring * 86400
pr_cnrm_fall = pr_cnrm_fall * 86400

pr_ipsl_annual = pr_ipsl_annual*86400
pr_ipsl_winter = pr_ipsl_winter * 86400
pr_ipsl_summer = pr_ipsl_summer * 86400
pr_ipsl_spring = pr_ipsl_spring * 86400
pr_ipsl_fall = pr_ipsl_fall * 86400

pr_mpi_annual = pr_mpi_annual*86400
pr_mpi_winter = pr_mpi_winter * 86400
pr_mpi_summer = pr_mpi_summer * 86400
pr_mpi_spring = pr_mpi_spring * 86400
pr_mpi_fall = pr_mpi_fall * 86400

cnrm_biais_pr_annual <- pr_cnrm_annual-pr_erai_annual
cnrm_biais_pr_winter <- pr_cnrm_winter-pr_erai_winter
cnrm_biais_pr_summer <- pr_cnrm_summer-pr_erai_summer
cnrm_biais_pr_spring <- pr_cnrm_spring-pr_erai_spring
cnrm_biais_pr_fall <- pr_cnrm_fall-pr_erai_fall

ipsl_biais_pr_annual <- pr_ipsl_annual-pr_erai_annual
ipsl_biais_pr_winter <- pr_ipsl_winter-pr_erai_winter
ipsl_biais_pr_summer <- pr_ipsl_summer-pr_erai_summer
ipsl_biais_pr_spring <- pr_ipsl_spring-pr_erai_spring
ipsl_biais_pr_fall <- pr_ipsl_fall-pr_erai_fall

mpi_biais_pr_annual <- pr_mpi_annual-pr_erai_annual
mpi_biais_pr_winter <- pr_mpi_winter-pr_erai_winter
mpi_biais_pr_summer <- pr_mpi_summer-pr_erai_summer
mpi_biais_pr_spring <- pr_mpi_spring-pr_erai_spring
mpi_biais_pr_fall <- pr_mpi_fall-pr_erai_fall

#* Données utilisées dans le code (à modifier si besoin)-----------

biais_model1_unscale_annual <- cnrm_biais_tas_annual-cnrm_biais_pr_annual
biais_model1_unscale_winter <- cnrm_biais_tas_winter-cnrm_biais_pr_winter
biais_model1_unscale_summer <- cnrm_biais_tas_summer-cnrm_biais_pr_summer
biais_model1_unscale_spring <- cnrm_biais_tas_spring-cnrm_biais_pr_spring
biais_model1_unscale_fall <- cnrm_biais_tas_fall-cnrm_biais_pr_fall

biais_model2_unscale_annual <- ipsl_biais_tas_annual-ipsl_biais_pr_annual
biais_model2_unscale_winter <- ipsl_biais_tas_winter-ipsl_biais_pr_winter
biais_model2_unscale_summer <- ipsl_biais_tas_summer-ipsl_biais_pr_summer
biais_model2_unscale_spring <- ipsl_biais_tas_spring-ipsl_biais_pr_spring
biais_model2_unscale_fall <- ipsl_biais_tas_fall-ipsl_biais_pr_fall

biais_model3_unscale_annual <- mpi_biais_tas_annual-mpi_biais_pr_annual
biais_model3_unscale_winter <- mpi_biais_tas_winter-mpi_biais_pr_winter
biais_model3_unscale_summer <- mpi_biais_tas_summer-mpi_biais_pr_summer
biais_model3_unscale_spring <- mpi_biais_tas_spring-mpi_biais_pr_spring
biais_model3_unscale_fall <- mpi_biais_tas_fall-mpi_biais_pr_fall

biais_model1_annual <- scale(biais_model1_unscale_annual)
biais_model1_winter <- scale(biais_model1_unscale_winter)
biais_model1_summer <- scale(biais_model1_unscale_summer)
biais_model1_spring <- scale(biais_model1_unscale_spring)
biais_model1_fall <- scale(biais_model1_unscale_fall)

biais_model2_annual <- scale(biais_model2_unscale_annual)
biais_model2_winter <- scale(biais_model2_unscale_winter)
biais_model2_summer <- scale(biais_model2_unscale_summer)
biais_model2_spring <- scale(biais_model2_unscale_spring)
biais_model2_fall <- scale(biais_model2_unscale_fall)

biais_model3_annual <- scale(biais_model3_unscale_annual)
biais_model3_winter <- scale(biais_model3_unscale_winter)
biais_model3_summer <- scale(biais_model3_unscale_summer)
biais_model3_spring <- scale(biais_model3_unscale_spring)
biais_model3_fall <- scale(biais_model3_unscale_fall)

colorTable<- designer.colors(64, c( "blue","grey90", "red"))

# Modification des données pour les domaines utilisés ------- 
eur = map(xlim=c(-20.25,60),
          ylim=c(34.5,75),
          plot=FALSE)

fr = map(xlim=c(-5.25,9),
             ylim=c(40.5,54.75),
             plot=FALSE)

lon_eur_cnrm = c((lon_cnrm[454:480]-360),lon_cnrm[1:81])  
lat_eur_cnrm = lat_cnrm[167:221]

lon_fr_cnrm = c((lon_cnrm[474:480]-360),lon_cnrm[1:13])
lat_fr_cnrm = lat_cnrm[175:194]

tas_eur_cnrm_annual <- rbind(cnrm_biais_tas_annual[454:480,167:221],cnrm_biais_tas_annual[1:81,167:221])
tas_eur_cnrm_winter <- rbind(cnrm_biais_tas_winter[454:480,167:221],cnrm_biais_tas_winter[1:81,167:221])
tas_eur_cnrm_summer <- rbind(cnrm_biais_tas_summer[454:480,167:221],cnrm_biais_tas_summer[1:81,167:221])
tas_eur_cnrm_spring <- rbind(cnrm_biais_tas_spring[454:480,167:221],cnrm_biais_tas_spring[1:81,167:221])
tas_eur_cnrm_fall <- rbind(cnrm_biais_tas_fall[454:480,167:221],cnrm_biais_tas_fall[1:81,167:221])

tas_fr_cnrm_annual <- rbind(cnrm_biais_tas_annual[474:480,175:194],cnrm_biais_tas_annual[1:13,175:194])
tas_fr_cnrm_winter <- rbind(cnrm_biais_tas_winter[474:480,175:194],cnrm_biais_tas_winter[1:13,175:194])
tas_fr_cnrm_summer <- rbind(cnrm_biais_tas_summer[474:480,175:194],cnrm_biais_tas_summer[1:13,175:194])
tas_fr_cnrm_spring <- rbind(cnrm_biais_tas_spring[474:480,175:194],cnrm_biais_tas_spring[1:13,175:194])
tas_fr_cnrm_fall <- rbind(cnrm_biais_tas_fall[474:480,175:194],cnrm_biais_tas_fall[1:13,175:194])

pr_eur_cnrm_annual <- rbind(cnrm_biais_pr_annual[454:480,167:221],cnrm_biais_pr_annual[1:81,167:221])
pr_eur_cnrm_winter <- rbind(cnrm_biais_pr_winter[454:480,167:221],cnrm_biais_pr_winter[1:81,167:221])
pr_eur_cnrm_summer <- rbind(cnrm_biais_pr_summer[454:480,167:221],cnrm_biais_pr_summer[1:81,167:221])
pr_eur_cnrm_spring <- rbind(cnrm_biais_pr_spring[454:480,167:221],cnrm_biais_pr_spring[1:81,167:221])
pr_eur_cnrm_fall <- rbind(cnrm_biais_pr_fall[454:480,167:221],cnrm_biais_pr_fall[1:81,167:221])

pr_fr_cnrm_annual <- rbind(cnrm_biais_pr_annual[474:480,175:194],cnrm_biais_pr_annual[1:13,175:194])
pr_fr_cnrm_winter <- rbind(cnrm_biais_pr_winter[474:480,175:194],cnrm_biais_pr_winter[1:13,175:194])
pr_fr_cnrm_summer <- rbind(cnrm_biais_pr_summer[474:480,175:194],cnrm_biais_pr_summer[1:13,175:194])
pr_fr_cnrm_spring <- rbind(cnrm_biais_pr_spring[474:480,175:194],cnrm_biais_pr_spring[1:13,175:194])
pr_fr_cnrm_fall <- rbind(cnrm_biais_pr_fall[474:480,175:194],cnrm_biais_pr_fall[1:13,175:194])

biais_model1_eur_annual <- rbind(biais_model1_annual[454:480,167:221],biais_model1_annual[1:81,167:221])
biais_model1_eur_winter <- rbind(biais_model1_winter[454:480,167:221],biais_model1_winter[1:81,167:221])
biais_model1_eur_summer <- rbind(biais_model1_summer[454:480,167:221],biais_model1_summer[1:81,167:221])
biais_model1_eur_spring <- rbind(biais_model1_spring[454:480,167:221],biais_model1_spring[1:81,167:221])
biais_model1_eur_fall <- rbind(biais_model1_fall[454:480,167:221],biais_model1_fall[1:81,167:221])

biais_model1_fr_annual <- rbind(biais_model1_annual[474:480,175:194],biais_model1_annual[1:13,175:194])
biais_model1_fr_winter <- rbind(biais_model1_winter[474:480,175:194],biais_model1_winter[1:13,175:194])
biais_model1_fr_summer <- rbind(biais_model1_summer[474:480,175:194],biais_model1_summer[1:13,175:194])
biais_model1_fr_spring <- rbind(biais_model1_spring[474:480,175:194],biais_model1_spring[1:13,175:194])
biais_model1_fr_fall <- rbind(biais_model1_fall[474:480,175:194],biais_model1_fall[1:13,175:194])

tas_eur_ipsl_annual <- rbind(ipsl_biais_tas_annual[454:480,167:221],ipsl_biais_tas_annual[1:81,167:221])
tas_eur_ipsl_winter <- rbind(ipsl_biais_tas_winter[454:480,167:221],ipsl_biais_tas_winter[1:81,167:221])
tas_eur_ipsl_summer <- rbind(ipsl_biais_tas_summer[454:480,167:221],ipsl_biais_tas_summer[1:81,167:221])
tas_eur_ipsl_spring <- rbind(ipsl_biais_tas_spring[454:480,167:221],ipsl_biais_tas_spring[1:81,167:221])
tas_eur_ipsl_fall <- rbind(ipsl_biais_tas_fall[454:480,167:221],ipsl_biais_tas_fall[1:81,167:221])

tas_fr_ipsl_annual <- rbind(ipsl_biais_tas_annual[474:480,175:194],ipsl_biais_tas_annual[1:13,175:194])
tas_fr_ipsl_winter <- rbind(ipsl_biais_tas_winter[474:480,175:194],ipsl_biais_tas_winter[1:13,175:194])
tas_fr_ipsl_summer <- rbind(ipsl_biais_tas_summer[474:480,175:194],ipsl_biais_tas_summer[1:13,175:194])
tas_fr_ipsl_spring <- rbind(ipsl_biais_tas_spring[474:480,175:194],ipsl_biais_tas_spring[1:13,175:194])
tas_fr_ipsl_fall <- rbind(ipsl_biais_tas_fall[474:480,175:194],ipsl_biais_tas_fall[1:13,175:194])

pr_eur_ipsl_annual <- rbind(ipsl_biais_pr_annual[454:480,167:221],ipsl_biais_pr_annual[1:81,167:221])
pr_eur_ipsl_winter <- rbind(ipsl_biais_pr_winter[454:480,167:221],ipsl_biais_pr_winter[1:81,167:221])
pr_eur_ipsl_summer <- rbind(ipsl_biais_pr_summer[454:480,167:221],ipsl_biais_pr_summer[1:81,167:221])
pr_eur_ipsl_spring <- rbind(ipsl_biais_pr_spring[454:480,167:221],ipsl_biais_pr_spring[1:81,167:221])
pr_eur_ipsl_fall <- rbind(ipsl_biais_pr_fall[454:480,167:221],ipsl_biais_pr_fall[1:81,167:221])

pr_fr_ipsl_annual <- rbind(ipsl_biais_pr_annual[474:480,175:194],ipsl_biais_pr_annual[1:13,175:194])
pr_fr_ipsl_winter <- rbind(ipsl_biais_pr_winter[474:480,175:194],ipsl_biais_pr_winter[1:13,175:194])
pr_fr_ipsl_summer <- rbind(ipsl_biais_pr_summer[474:480,175:194],ipsl_biais_pr_summer[1:13,175:194])
pr_fr_ipsl_spring <- rbind(ipsl_biais_pr_spring[474:480,175:194],ipsl_biais_pr_spring[1:13,175:194])
pr_fr_ipsl_fall <- rbind(ipsl_biais_pr_fall[474:480,175:194],ipsl_biais_pr_fall[1:13,175:194])

biais_model2_eur_annual <- rbind(biais_model2_annual[454:480,167:221],biais_model2_annual[1:81,167:221])
biais_model2_eur_winter <- rbind(biais_model2_winter[454:480,167:221],biais_model2_winter[1:81,167:221])
biais_model2_eur_summer <- rbind(biais_model2_summer[454:480,167:221],biais_model2_summer[1:81,167:221])
biais_model2_eur_spring <- rbind(biais_model2_spring[454:480,167:221],biais_model2_spring[1:81,167:221])
biais_model2_eur_fall <- rbind(biais_model2_fall[454:480,167:221],biais_model2_fall[1:81,167:221])

biais_model2_fr_annual <- rbind(biais_model2_annual[474:480,175:194],biais_model2_annual[1:13,175:194])
biais_model2_fr_winter <- rbind(biais_model2_winter[474:480,175:194],biais_model2_winter[1:13,175:194])
biais_model2_fr_summer <- rbind(biais_model2_summer[474:480,175:194],biais_model2_summer[1:13,175:194])
biais_model2_fr_spring <- rbind(biais_model2_spring[474:480,175:194],biais_model2_spring[1:13,175:194])
biais_model2_fr_fall <- rbind(biais_model2_fall[474:480,175:194],biais_model2_fall[1:13,175:194])

tas_eur_mpi_annual <- rbind(mpi_biais_tas_annual[454:480,167:221],mpi_biais_tas_annual[1:81,167:221])
tas_eur_mpi_winter <- rbind(mpi_biais_tas_winter[454:480,167:221],mpi_biais_tas_winter[1:81,167:221])
tas_eur_mpi_summer <- rbind(mpi_biais_tas_summer[454:480,167:221],mpi_biais_tas_summer[1:81,167:221])
tas_eur_mpi_spring <- rbind(mpi_biais_tas_spring[454:480,167:221],mpi_biais_tas_spring[1:81,167:221])
tas_eur_mpi_fall <- rbind(mpi_biais_tas_fall[454:480,167:221],mpi_biais_tas_fall[1:81,167:221])

tas_fr_mpi_annual <- rbind(mpi_biais_tas_annual[474:480,175:194],mpi_biais_tas_annual[1:13,175:194])
tas_fr_mpi_winter <- rbind(mpi_biais_tas_winter[474:480,175:194],mpi_biais_tas_winter[1:13,175:194])
tas_fr_mpi_summer <- rbind(mpi_biais_tas_summer[474:480,175:194],mpi_biais_tas_summer[1:13,175:194])
tas_fr_mpi_spring <- rbind(mpi_biais_tas_spring[474:480,175:194],mpi_biais_tas_spring[1:13,175:194])
tas_fr_mpi_fall <- rbind(mpi_biais_tas_fall[474:480,175:194],mpi_biais_tas_fall[1:13,175:194])

pr_eur_mpi_annual <- rbind(mpi_biais_pr_annual[454:480,167:221],mpi_biais_pr_annual[1:81,167:221])
pr_eur_mpi_winter <- rbind(mpi_biais_pr_winter[454:480,167:221],mpi_biais_pr_winter[1:81,167:221])
pr_eur_mpi_summer <- rbind(mpi_biais_pr_summer[454:480,167:221],mpi_biais_pr_summer[1:81,167:221])
pr_eur_mpi_spring <- rbind(mpi_biais_pr_spring[454:480,167:221],mpi_biais_pr_spring[1:81,167:221])
pr_eur_mpi_fall <- rbind(mpi_biais_pr_fall[454:480,167:221],mpi_biais_pr_fall[1:81,167:221])

pr_fr_mpi_annual <- rbind(mpi_biais_pr_annual[474:480,175:194],mpi_biais_pr_annual[1:13,175:194])
pr_fr_mpi_winter <- rbind(mpi_biais_pr_winter[474:480,175:194],mpi_biais_pr_winter[1:13,175:194])
pr_fr_mpi_summer <- rbind(mpi_biais_pr_summer[474:480,175:194],mpi_biais_pr_summer[1:13,175:194])
pr_fr_mpi_spring <- rbind(mpi_biais_pr_spring[474:480,175:194],mpi_biais_pr_spring[1:13,175:194])
pr_fr_mpi_fall <- rbind(mpi_biais_pr_fall[474:480,175:194],mpi_biais_pr_fall[1:13,175:194])

biais_model3_eur_annual <- rbind(biais_model3_annual[454:480,167:221],biais_model3_annual[1:81,167:221])
biais_model3_eur_winter <- rbind(biais_model3_winter[454:480,167:221],biais_model3_winter[1:81,167:221])
biais_model3_eur_summer <- rbind(biais_model3_summer[454:480,167:221],biais_model3_summer[1:81,167:221])
biais_model3_eur_spring <- rbind(biais_model3_spring[454:480,167:221],biais_model3_spring[1:81,167:221])
biais_model3_eur_fall <- rbind(biais_model3_fall[454:480,167:221],biais_model3_fall[1:81,167:221])

biais_model3_fr_annual <- rbind(biais_model3_annual[474:480,175:194],biais_model3_annual[1:13,175:194])
biais_model3_fr_winter <- rbind(biais_model3_winter[474:480,175:194],biais_model3_winter[1:13,175:194])
biais_model3_fr_summer <- rbind(biais_model3_summer[474:480,175:194],biais_model3_summer[1:13,175:194])
biais_model3_fr_spring <- rbind(biais_model3_spring[474:480,175:194],biais_model3_spring[1:13,175:194])
biais_model3_fr_fall <- rbind(biais_model3_fall[474:480,175:194],biais_model3_fall[1:13,175:194])

tas_eur_erai_annual <- rbind(tas_erai_annual[454:480,167:221],tas_erai_annual[1:81,167:221])-273.15
tas_eur_erai_winter <- rbind(tas_erai_winter[454:480,167:221],tas_erai_winter[1:81,167:221])-273.15
tas_eur_erai_summer <- rbind(tas_erai_summer[454:480,167:221],tas_erai_summer[1:81,167:221])-273.15
tas_eur_erai_spring <- rbind(tas_erai_spring[454:480,167:221],tas_erai_spring[1:81,167:221])-273.15
tas_eur_erai_fall <- rbind(tas_erai_fall[454:480,167:221],tas_erai_fall[1:81,167:221])-273.15

tas_fr_erai_annual <- rbind(tas_erai_annual[474:480,175:194],tas_erai_annual[1:13,175:194])-273.15
tas_fr_erai_winter <- rbind(tas_erai_winter[474:480,175:194],tas_erai_winter[1:13,175:194])-273.15
tas_fr_erai_summer <- rbind(tas_erai_summer[474:480,175:194],tas_erai_summer[1:13,175:194])-273.15
tas_fr_erai_spring <- rbind(tas_erai_spring[474:480,175:194],tas_erai_spring[1:13,175:194])-273.15
tas_fr_erai_fall <- rbind(tas_erai_fall[474:480,175:194],tas_erai_fall[1:13,175:194])-273.15

pr_eur_erai_annual <- rbind(pr_erai_annual[454:480,167:221],pr_erai_annual[1:81,167:221])
pr_eur_erai_winter <- rbind(pr_erai_winter[454:480,167:221],pr_erai_winter[1:81,167:221])
pr_eur_erai_summer <- rbind(pr_erai_summer[454:480,167:221],pr_erai_summer[1:81,167:221])
pr_eur_erai_spring <- rbind(pr_erai_spring[454:480,167:221],pr_erai_spring[1:81,167:221])
pr_eur_erai_spring <- rbind(pr_erai_fall[454:480,167:221],pr_erai_fall[1:81,167:221])

pr_fr_erai_annual <- rbind(pr_erai_annual[474:480,175:194],pr_erai_annual[1:13,175:194])-273.15
pr_fr_erai_winter <- rbind(pr_erai_winter[474:480,175:194],pr_erai_winter[1:13,175:194])
pr_fr_erai_summer <- rbind(pr_erai_summer[474:480,175:194],pr_erai_summer[1:13,175:194])
pr_fr_erai_spring <- rbind(pr_erai_spring[474:480,175:194],pr_erai_spring[1:13,175:194])
pr_fr_erai_spring <- rbind(pr_erai_fall[474:480,175:194],pr_erai_fall[1:13,175:194])

#* Données permettant la création des zlim pour les plots ----------

eur_biais_annual = c(biais_model1_eur_annual,biais_model2_eur_annual,biais_model3_eur_annual)
eur_tas_annual = c(tas_eur_cnrm_annual,tas_eur_ipsl_annual,tas_eur_erai_annual,tas_eur_mpi_annual)
eur_pr_annual = c(pr_eur_cnrm_annual,pr_eur_ipsl_annual,pr_eur_erai_annual,pr_eur_mpi_annual)

fr_biais_annual = c(biais_model1_fr_annual,biais_model2_fr_annual,biais_model3_fr_annual)
fr_tas_annual = c(tas_fr_cnrm_annual,tas_fr_ipsl_annual,tas_fr_erai_annual,tas_fr_mpi_annual)
fr_pr_annual = c(pr_fr_cnrm_annual,pr_fr_ipsl_annual,pr_fr_erai_annual,pr_fr_mpi_annual)

eur_biais_annual = c(biais_model1_eur_annual,biais_model2_eur_annual,biais_model3_eur_annual)
eur_biais_winter = c(biais_model1_eur_winter,biais_model2_eur_winter,biais_model3_eur_winter)
eur_biais_summer = c(biais_model1_eur_summer,biais_model2_eur_summer,biais_model3_eur_summer)
eur_biais_spring = c(biais_model1_eur_spring,biais_model2_eur_spring,biais_model3_eur_spring)
eur_biais_fall = c(biais_model1_eur_fall,biais_model2_eur_fall,biais_model3_eur_fall)

fr_biais_annual = c(biais_model1_fr_annual,biais_model2_fr_annual,biais_model3_fr_annual)
fr_biais_winter = c(biais_model1_fr_winter,biais_model2_fr_winter,biais_model3_fr_winter)
fr_biais_summer = c(biais_model1_fr_summer,biais_model2_fr_summer,biais_model3_fr_summer)
fr_biais_spring = c(biais_model1_fr_spring,biais_model2_fr_spring,biais_model3_fr_spring)
fr_biais_fall = c(biais_model1_fr_fall,biais_model2_fr_fall,biais_model3_fr_fall)