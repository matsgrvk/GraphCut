nc <- nc_open("t2_erai_clim_1979_2008.nc")
nc_erai_winter <- nc_open("t2_erai_clim_1979_2008_DJF.nc")
nc_erai_summer <- nc_open("t2_erai_clim_1979_2008_JJA.nc")
nc_erai_spring <- nc_open("t2_erai_clim_1979_2008_MAM.nc")
nc_erai_fall <- nc_open("t2_erai_clim_1979_2008_SON.nc")

tas_erai = ncvar_get(nc, varid = "t2")
tas_erai_winter = ncvar_get(nc_erai_winter, varid = "t2")
tas_erai_summer = ncvar_get(nc_erai_summer, varid = "t2")
tas_erai_spring = ncvar_get(nc_erai_spring, varid = "t2")
tas_erai_fall = ncvar_get(nc_erai_fall, varid = "t2")

lat_erai = ncvar_get(nc, varid = "lat")
lon_erai = ncvar_get(nc, varid = "lon")

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

nc2 <- nc_open("tas_cnrm_clim_1979_2008.nc")
nc_cnrm_winter <- nc_open("tas_cnrm_clim_1979_2008_DJF.nc")
nc_cnrm_summer <- nc_open("tas_cnrm_clim_1979_2008_JJA.nc")
nc_cnrm_spring <- nc_open("tas_cnrm_clim_1979_2008_MAM.nc")
nc_cnrm_fall <- nc_open("tas_cnrm_clim_1979_2008_SON.nc")

tas_cnrm = ncvar_get(nc2, varid = "tas")
tas_cnrm_winter = ncvar_get(nc_cnrm_winter, varid = "tas")
tas_cnrm_summer = ncvar_get(nc_cnrm_summer, varid = "tas")
tas_cnrm_spring = ncvar_get(nc_cnrm_spring, varid = "tas")
tas_cnrm_fall = ncvar_get(nc_cnrm_fall, varid = "tas")

lat_cnrm = ncvar_get(nc2, varid = "lat")
lon_cnrm = ncvar_get(nc2, varid = "lon")
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

nc3 <- nc_open("tas_ipsl_clim_1979_2008.nc")
nc_ipsl_winter <- nc_open("tas_ipsl_clim_1979_2008_DJF.nc")
nc_ipsl_summer <- nc_open("tas_ipsl_clim_1979_2008_JJA.nc")
nc_ipsl_spring <- nc_open("tas_ipsl_clim_1979_2008_MAM.nc")
nc_ipsl_fall <- nc_open("tas_ipsl_clim_1979_2008_SON.nc")

tas_ipsl = ncvar_get(nc3, varid = "tas")
tas_ipsl_winter = ncvar_get(nc_ipsl_winter, varid = "tas")
tas_ipsl_summer = ncvar_get(nc_ipsl_summer, varid = "tas")
tas_ipsl_spring = ncvar_get(nc_ipsl_spring, varid = "tas")
tas_ipsl_fall = ncvar_get(nc_ipsl_fall, varid = "tas")

lat_ipsl = ncvar_get(nc3, varid = "lat")
lon_ipsl = ncvar_get(nc3, varid = "lon")
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

nc4 <- nc_open("tas_mpi_clim_1979-01-01,2008-12-31.nc")
nc_mpi_winter <- nc_open("tas_mpi_clim_1979-01-01,2008-12-31_DJF.nc")
nc_mpi_summer <- nc_open("tas_mpi_clim_1979-01-01,2008-12-31_JJA.nc")
nc_mpi_spring <- nc_open("tas_mpi_clim_1979-01-01,2008-12-31_MAM.nc")
nc_mpi_fall <- nc_open("tas_mpi_clim_1979-01-01,2008-12-31_SON.nc")

tas_mpi = ncvar_get(nc3, varid = "tas")
tas_mpi_winter = ncvar_get(nc_mpi_winter, varid = "tas")
tas_mpi_summer = ncvar_get(nc_mpi_summer, varid = "tas")
tas_mpi_spring = ncvar_get(nc_mpi_spring, varid = "tas")
tas_mpi_fall = ncvar_get(nc_mpi_fall, varid = "tas")

lat_mpi = ncvar_get(nc4, varid = "lat")
lon_mpi = ncvar_get(nc4, varid = "lon")
tas_mpi_winter = tas_mpi_winter[, length(lat_mpi) : 1]
tas_mpi_summer = tas_mpi_summer[, length(lat_mpi) : 1]
tas_mpi_spring = tas_mpi_spring[, length(lat_mpi) : 1]
tas_mpi_fall = tas_mpi_fall[, length(lat_mpi) : 1]
lat_mpi = rev(lat_mpi)

cnrm_biais_tas <- tas_cnrm-tas_erai
cnrm_biais_tas_winter <- tas_cnrm_winter-tas_erai_winter
cnrm_biais_tas_summer <- tas_cnrm_summer-tas_erai_summer
cnrm_biais_tas_spring <- tas_cnrm_spring-tas_erai_spring
cnrm_biais_tas_fall <- tas_cnrm_fall-tas_erai_fall

ipsl_biais_tas <- tas_ipsl-tas_erai
ipsl_biais_tas_winter <- tas_ipsl_winter-tas_erai_winter
ipsl_biais_tas_summer <- tas_ipsl_summer-tas_erai_summer
ipsl_biais_tas_spring <- tas_ipsl_spring-tas_erai_spring
ipsl_biais_tas_fall <- tas_ipsl_fall-tas_erai_fall

mpi_biais_tas <- tas_mpi-tas_erai
mpi_biais_tas_winter <- tas_mpi_winter-tas_erai_winter
mpi_biais_tas_summer <- tas_mpi_summer-tas_erai_summer
mpi_biais_tas_spring <- tas_mpi_spring-tas_erai_spring
mpi_biais_tas_fall <- tas_mpi_fall-tas_erai_fall


nc5 <- nc_open("tp_erai_clim_1979_2008.nc")
nc_pr_erai_winter <- nc_open("tp_erai_clim_1979_2008_DJF.nc")
nc_pr_erai_summer <- nc_open("tp_erai_clim_1979_2008_JJA.nc")
nc_pr_erai_spring <- nc_open("tp_erai_clim_1979_2008_MAM.nc")
nc_pr_erai_fall <- nc_open("tp_erai_clim_1979_2008_SON.nc")

pr_erai = ncvar_get(nc5, varid = "tp")
pr_erai_winter = ncvar_get(nc_pr_erai_winter, varid = "tp")
pr_erai_summer = ncvar_get(nc_pr_erai_summer, varid = "tp")
pr_erai_spring = ncvar_get(nc_pr_erai_spring, varid = "tp")
pr_erai_fall = ncvar_get(nc_pr_erai_fall, varid = "tp")

lat_erai = ncvar_get(nc5, varid = "latitude")
lon_erai = ncvar_get(nc5, varid = "longitude")

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

nc6 <- nc_open("pr_cnrm_clim_1979-01-01,2008-12-31.nc")
nc_pr_cnrm_winter <- nc_open("pr_cnrm_clim_1979-01-01,2008-12-31_DJF.nc")
nc_pr_cnrm_summer <- nc_open("pr_cnrm_clim_1979-01-01,2008-12-31_JJA.nc")
nc_pr_cnrm_spring <- nc_open("pr_cnrm_clim_1979-01-01,2008-12-31_MAM.nc")
nc_pr_cnrm_fall <- nc_open("pr_cnrm_clim_1979-01-01,2008-12-31_SON.nc")

pr_cnrm = ncvar_get(nc6, varid = "pr")
pr_cnrm_winter = ncvar_get(nc_pr_cnrm_winter, varid = "pr")
pr_cnrm_summer = ncvar_get(nc_pr_cnrm_summer, varid = "pr")
pr_cnrm_spring = ncvar_get(nc_pr_cnrm_spring, varid = "pr")
pr_cnrm_fall = ncvar_get(nc_pr_cnrm_fall, varid = "pr")

lat_cnrm = ncvar_get(nc6, varid = "lat")
lon_cnrm = ncvar_get(nc6, varid = "lon")
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

nc7 <- nc_open("pr_ipsl_clim_1979-01-01,2008-12-31.nc")
nc_pr_ipsl_winter <- nc_open("pr_ipsl_clim_1979-01-01,2008-12-31_DJF.nc")
nc_pr_ipsl_summer <- nc_open("pr_ipsl_clim_1979-01-01,2008-12-31_JJA.nc")
nc_pr_ipsl_spring <- nc_open("pr_ipsl_clim_1979-01-01,2008-12-31_MAM.nc")
nc_pr_ipsl_fall <- nc_open("pr_ipsl_clim_1979-01-01,2008-12-31_SON.nc")

pr_ipsl = ncvar_get(nc7, varid = "pr")
pr_ipsl_winter = ncvar_get(nc_pr_ipsl_winter, varid = "pr")
pr_ipsl_summer = ncvar_get(nc_pr_ipsl_summer, varid = "pr")
pr_ipsl_spring = ncvar_get(nc_pr_ipsl_spring, varid = "pr")
pr_ipsl_fall = ncvar_get(nc_pr_ipsl_fall, varid = "pr")

lat_ipsl = ncvar_get(nc7, varid = "lat")
lon_ipsl = ncvar_get(nc7, varid = "lon")
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

nc8 <- nc_open("pr_mpi_clim_1979-01-01,2008-12-31.nc")
nc_pr_mpi_winter <- nc_open("pr_mpi_clim_1979-01-01,2008-12-31_DJF.nc")
nc_pr_mpi_summer <- nc_open("pr_mpi_clim_1979-01-01,2008-12-31_JJA.nc")
nc_pr_mpi_spring <- nc_open("pr_mpi_clim_1979-01-01,2008-12-31_MAM.nc")
nc_pr_mpi_fall <- nc_open("pr_mpi_clim_1979-01-01,2008-12-31_SON.nc")

pr_mpi = ncvar_get(nc8, varid = "pr")
pr_mpi_winter = ncvar_get(nc_pr_ipsl_winter, varid = "pr")
pr_mpi_summer = ncvar_get(nc_pr_ipsl_summer, varid = "pr")
pr_mpi_spring = ncvar_get(nc_pr_ipsl_spring, varid = "pr")
pr_mpi_fall = ncvar_get(nc_pr_ipsl_fall, varid = "pr")

lat_mpi = ncvar_get(nc8, varid = "lat")
lon_mpi = ncvar_get(nc8, varid = "lon")
pr_mpi_winter = pr_mpi_winter[, length(lat_mpi) : 1]
pr_mpi_summer = pr_mpi_summer[, length(lat_mpi) : 1]
pr_mpi_spring = pr_mpi_spring[, length(lat_mpi) : 1]
pr_mpi_fall = pr_mpi_fall[, length(lat_mpi) : 1]
lat_mpi = rev(lat_mpi)

pr_erai = pr_erai*1000
pr_erai_winter = pr_erai_winter * 1000
pr_erai_summer = pr_erai_summer * 1000
pr_erai_spring = pr_erai_spring * 1000
pr_erai_fall = pr_erai_fall * 1000

pr_cnrm = pr_cnrm*86400
pr_cnrm_winter = pr_cnrm_winter * 86400
pr_cnrm_summer = pr_cnrm_summer * 86400
pr_cnrm_spring = pr_cnrm_spring * 86400
pr_cnrm_fall = pr_cnrm_fall * 86400

pr_ipsl = pr_ipsl*86400
pr_ipsl_winter = pr_ipsl_winter * 86400
pr_ipsl_summer = pr_ipsl_summer * 86400
pr_ipsl_spring = pr_ipsl_spring * 86400
pr_ipsl_fall = pr_ipsl_fall * 86400

pr_mpi = pr_mpi*86400
pr_mpi_winter = pr_mpi_winter * 86400
pr_mpi_summer = pr_mpi_summer * 86400
pr_mpi_spring = pr_mpi_spring * 86400
pr_mpi_fall = pr_mpi_fall * 86400

cnrm_biais_pr <- pr_cnrm-pr_erai
cnrm_biais_pr_winter <- pr_cnrm_winter-pr_erai_winter
cnrm_biais_pr_summer <- pr_cnrm_summer-pr_erai_summer
cnrm_biais_pr_spring <- pr_cnrm_spring-pr_erai_spring
cnrm_biais_pr_fall <- pr_cnrm_fall-pr_erai_fall

ipsl_biais_pr <- pr_ipsl-pr_erai
ipsl_biais_pr_winter <- pr_ipsl_winter-pr_erai_winter
ipsl_biais_pr_summer <- pr_ipsl_summer-pr_erai_summer
ipsl_biais_pr_spring <- pr_ipsl_spring-pr_erai_spring
ipsl_biais_pr_fall <- pr_ipsl_fall-pr_erai_fall

mpi_biais_pr <- pr_mpi-pr_erai
mpi_biais_pr_winter <- pr_mpi_winter-pr_erai_winter
mpi_biais_pr_summer <- pr_mpi_summer-pr_erai_summer
mpi_biais_pr_spring <- pr_mpi_spring-pr_erai_spring
mpi_biais_pr_fall <- pr_mpi_fall-pr_erai_fall

biais_model1_unscale <- cnrm_biais_tas-cnrm_biais_pr
biais_model1_unscale_winter <- cnrm_biais_tas_winter-cnrm_biais_pr_winter
biais_model1_unscale_summer <- cnrm_biais_tas_summer-cnrm_biais_pr_summer
biais_model1_unscale_spring <- cnrm_biais_tas_spring-cnrm_biais_pr_spring
biais_model1_unscale_fall <- cnrm_biais_tas_fall-cnrm_biais_pr_fall

biais_model2_unscale <- ipsl_biais_tas-ipsl_biais_pr
biais_model2_unscale_winter <- ipsl_biais_tas_winter-ipsl_biais_pr_winter
biais_model2_unscale_summer <- ipsl_biais_tas_summer-ipsl_biais_pr_summer
biais_model2_unscale_spring <- ipsl_biais_tas_spring-ipsl_biais_pr_spring
biais_model2_unscale_fall <- ipsl_biais_tas_fall-ipsl_biais_pr_fall

biais_model3_unscale <- mpi_biais_tas-mpi_biais_pr
biais_model3_unscale_winter <- mpi_biais_tas_winter-mpi_biais_pr_winter
biais_model3_unscale_summer <- mpi_biais_tas_summer-mpi_biais_pr_summer
biais_model3_unscale_spring <- mpi_biais_tas_spring-mpi_biais_pr_spring
biais_model3_unscale_fall <- mpi_biais_tas_fall-mpi_biais_pr_fall

biais_model1 <- scale(biais_model1_unscale)
biais_model1_winter <- scale(biais_model1_unscale_winter)
biais_model1_summer <- scale(biais_model1_unscale_summer)
biais_model1_spring <- scale(biais_model1_unscale_spring)
biais_model1_fall <- scale(biais_model1_unscale_fall)

biais_model2 <- scale(biais_model2_unscale)
biais_model2_winter <- scale(biais_model2_unscale_winter)
biais_model2_summer <- scale(biais_model2_unscale_summer)
biais_model2_spring <- scale(biais_model2_unscale_spring)
biais_model2_fall <- scale(biais_model2_unscale_fall)

biais_model3 <- scale(biais_model3_unscale)
biais_model3_winter <- scale(biais_model3_unscale_winter)
biais_model3_summer <- scale(biais_model3_unscale_summer)
biais_model3_spring <- scale(biais_model3_unscale_spring)
biais_model3_fall <- scale(biais_model3_unscale_fall)

colorTable<- designer.colors(64, c( "blue","grey90", "red"))

############################################ Settings biais ############################################

# Frontières 
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

tas_eur_cnrm <- rbind(cnrm_biais_tas[454:480,167:221],cnrm_biais_tas[1:81,167:221])
tas_eur_cnrm_winter <- rbind(cnrm_biais_tas_winter[454:480,167:221],cnrm_biais_tas_winter[1:81,167:221])
tas_eur_cnrm_summer <- rbind(cnrm_biais_tas_summer[454:480,167:221],cnrm_biais_tas_summer[1:81,167:221])
tas_eur_cnrm_spring <- rbind(cnrm_biais_tas_spring[454:480,167:221],cnrm_biais_tas_spring[1:81,167:221])
tas_eur_cnrm_fall <- rbind(cnrm_biais_tas_fall[454:480,167:221],cnrm_biais_tas_fall[1:81,167:221])

tas_fr_cnrm <- rbind(cnrm_biais_tas[474:480,175:194],cnrm_biais_tas[1:13,175:194])
tas_fr_cnrm_winter <- rbind(cnrm_biais_tas_winter[474:480,175:194],cnrm_biais_tas_winter[1:13,175:194])
tas_fr_cnrm_summer <- rbind(cnrm_biais_tas_summer[474:480,175:194],cnrm_biais_tas_summer[1:13,175:194])
tas_fr_cnrm_spring <- rbind(cnrm_biais_tas_spring[474:480,175:194],cnrm_biais_tas_spring[1:13,175:194])
tas_fr_cnrm_fall <- rbind(cnrm_biais_tas_fall[474:480,175:194],cnrm_biais_tas_fall[1:13,175:194])

pr_eur_cnrm <- rbind(cnrm_biais_pr[454:480,167:221],cnrm_biais_pr[1:81,167:221])
pr_eur_cnrm_winter <- rbind(cnrm_biais_pr_winter[454:480,167:221],cnrm_biais_pr_winter[1:81,167:221])
pr_eur_cnrm_summer <- rbind(cnrm_biais_pr_summer[454:480,167:221],cnrm_biais_pr_summer[1:81,167:221])
pr_eur_cnrm_spring <- rbind(cnrm_biais_pr_spring[454:480,167:221],cnrm_biais_pr_spring[1:81,167:221])
pr_eur_cnrm_fall <- rbind(cnrm_biais_pr_fall[454:480,167:221],cnrm_biais_pr_fall[1:81,167:221])

pr_fr_cnrm <- rbind(cnrm_biais_pr[474:480,175:194],cnrm_biais_pr[1:13,175:194])
pr_fr_cnrm_winter <- rbind(cnrm_biais_pr_winter[474:480,175:194],cnrm_biais_pr_winter[1:13,175:194])
pr_fr_cnrm_summer <- rbind(cnrm_biais_pr_summer[474:480,175:194],cnrm_biais_pr_summer[1:13,175:194])
pr_fr_cnrm_spring <- rbind(cnrm_biais_pr_spring[474:480,175:194],cnrm_biais_pr_spring[1:13,175:194])
pr_fr_cnrm_fall <- rbind(cnrm_biais_pr_fall[474:480,175:194],cnrm_biais_pr_fall[1:13,175:194])

biais_model1_eur <- rbind(biais_model1[454:480,167:221],biais_model1[1:81,167:221])
biais_model1_eur_winter <- rbind(biais_model1_winter[454:480,167:221],biais_model1_winter[1:81,167:221])
biais_model1_eur_summer <- rbind(biais_model1_summer[454:480,167:221],biais_model1_summer[1:81,167:221])
biais_model1_eur_spring <- rbind(biais_model1_spring[454:480,167:221],biais_model1_spring[1:81,167:221])
biais_model1_eur_fall <- rbind(biais_model1_fall[454:480,167:221],biais_model1_fall[1:81,167:221])

biais_model1_fr <- rbind(biais_model1[474:480,175:194],biais_model1[1:13,175:194])
biais_model1_fr_winter <- rbind(biais_model1_winter[474:480,175:194],biais_model1_winter[1:13,175:194])
biais_model1_fr_summer <- rbind(biais_model1_summer[474:480,175:194],biais_model1_summer[1:13,175:194])
biais_model1_fr_spring <- rbind(biais_model1_spring[474:480,175:194],biais_model1_spring[1:13,175:194])
biais_model1_fr_fall <- rbind(biais_model1_fall[474:480,175:194],biais_model1_fall[1:13,175:194])

tas_eur_ipsl <- rbind(ipsl_biais_tas[454:480,167:221],ipsl_biais_tas[1:81,167:221])
tas_eur_ipsl_winter <- rbind(ipsl_biais_tas_winter[454:480,167:221],ipsl_biais_tas_winter[1:81,167:221])
tas_eur_ipsl_summer <- rbind(ipsl_biais_tas_summer[454:480,167:221],ipsl_biais_tas_summer[1:81,167:221])
tas_eur_ipsl_spring <- rbind(ipsl_biais_tas_spring[454:480,167:221],ipsl_biais_tas_spring[1:81,167:221])
tas_eur_ipsl_fall <- rbind(ipsl_biais_tas_fall[454:480,167:221],ipsl_biais_tas_fall[1:81,167:221])

tas_fr_ipsl <- rbind(ipsl_biais_tas[474:480,175:194],ipsl_biais_tas[1:13,175:194])
tas_fr_ipsl_winter <- rbind(ipsl_biais_tas_winter[474:480,175:194],ipsl_biais_tas_winter[1:13,175:194])
tas_fr_ipsl_summer <- rbind(ipsl_biais_tas_summer[474:480,175:194],ipsl_biais_tas_summer[1:13,175:194])
tas_fr_ipsl_spring <- rbind(ipsl_biais_tas_spring[474:480,175:194],ipsl_biais_tas_spring[1:13,175:194])
tas_fr_ipsl_fall <- rbind(ipsl_biais_tas_fall[474:480,175:194],ipsl_biais_tas_fall[1:13,175:194])

pr_eur_ipsl <- rbind(ipsl_biais_pr[454:480,167:221],ipsl_biais_pr[1:81,167:221])
pr_eur_ipsl_winter <- rbind(ipsl_biais_pr_winter[454:480,167:221],ipsl_biais_pr_winter[1:81,167:221])
pr_eur_ipsl_summer <- rbind(ipsl_biais_pr_summer[454:480,167:221],ipsl_biais_pr_summer[1:81,167:221])
pr_eur_ipsl_spring <- rbind(ipsl_biais_pr_spring[454:480,167:221],ipsl_biais_pr_spring[1:81,167:221])
pr_eur_ipsl_fall <- rbind(ipsl_biais_pr_fall[454:480,167:221],ipsl_biais_pr_fall[1:81,167:221])

pr_fr_ipsl <- rbind(ipsl_biais_pr[474:480,175:194],ipsl_biais_pr[1:13,175:194])
pr_fr_ipsl_winter <- rbind(ipsl_biais_pr_winter[474:480,175:194],ipsl_biais_pr_winter[1:13,175:194])
pr_fr_ipsl_summer <- rbind(ipsl_biais_pr_summer[474:480,175:194],ipsl_biais_pr_summer[1:13,175:194])
pr_fr_ipsl_spring <- rbind(ipsl_biais_pr_spring[474:480,175:194],ipsl_biais_pr_spring[1:13,175:194])
pr_fr_ipsl_fall <- rbind(ipsl_biais_pr_fall[474:480,175:194],ipsl_biais_pr_fall[1:13,175:194])

biais_model2_eur <- rbind(biais_model2[454:480,167:221],biais_model2[1:81,167:221])
biais_model2_eur_winter <- rbind(biais_model2_winter[454:480,167:221],biais_model2_winter[1:81,167:221])
biais_model2_eur_summer <- rbind(biais_model2_summer[454:480,167:221],biais_model2_summer[1:81,167:221])
biais_model2_eur_spring <- rbind(biais_model2_spring[454:480,167:221],biais_model2_spring[1:81,167:221])
biais_model2_eur_fall <- rbind(biais_model2_fall[454:480,167:221],biais_model2_fall[1:81,167:221])

biais_model2_fr <- rbind(biais_model2[474:480,175:194],biais_model2[1:13,175:194])
biais_model2_fr_winter <- rbind(biais_model2_winter[474:480,175:194],biais_model2_winter[1:13,175:194])
biais_model2_fr_summer <- rbind(biais_model2_summer[474:480,175:194],biais_model2_summer[1:13,175:194])
biais_model2_fr_spring <- rbind(biais_model2_spring[474:480,175:194],biais_model2_spring[1:13,175:194])
biais_model2_fr_fall <- rbind(biais_model2_fall[474:480,175:194],biais_model2_fall[1:13,175:194])

tas_eur_mpi <- rbind(mpi_biais_tas[454:480,167:221],mpi_biais_tas[1:81,167:221])
tas_eur_mpi_winter <- rbind(mpi_biais_tas_winter[454:480,167:221],mpi_biais_tas_winter[1:81,167:221])
tas_eur_mpi_summer <- rbind(mpi_biais_tas_summer[454:480,167:221],mpi_biais_tas_summer[1:81,167:221])
tas_eur_mpi_spring <- rbind(mpi_biais_tas_spring[454:480,167:221],mpi_biais_tas_spring[1:81,167:221])
tas_eur_mpi_fall <- rbind(mpi_biais_tas_fall[454:480,167:221],mpi_biais_tas_fall[1:81,167:221])

tas_fr_mpi <- rbind(mpi_biais_tas[474:480,175:194],mpi_biais_tas[1:13,175:194])
tas_fr_mpi_winter <- rbind(mpi_biais_tas_winter[474:480,175:194],mpi_biais_tas_winter[1:13,175:194])
tas_fr_mpi_summer <- rbind(mpi_biais_tas_summer[474:480,175:194],mpi_biais_tas_summer[1:13,175:194])
tas_fr_mpi_spring <- rbind(mpi_biais_tas_spring[474:480,175:194],mpi_biais_tas_spring[1:13,175:194])
tas_fr_mpi_fall <- rbind(mpi_biais_tas_fall[474:480,175:194],mpi_biais_tas_fall[1:13,175:194])

pr_eur_mpi <- rbind(mpi_biais_pr[454:480,167:221],mpi_biais_pr[1:81,167:221])
pr_eur_mpi_winter <- rbind(mpi_biais_pr_winter[454:480,167:221],mpi_biais_pr_winter[1:81,167:221])
pr_eur_mpi_summer <- rbind(mpi_biais_pr_summer[454:480,167:221],mpi_biais_pr_summer[1:81,167:221])
pr_eur_mpi_spring <- rbind(mpi_biais_pr_spring[454:480,167:221],mpi_biais_pr_spring[1:81,167:221])
pr_eur_mpi_fall <- rbind(mpi_biais_pr_fall[454:480,167:221],mpi_biais_pr_fall[1:81,167:221])

pr_fr_mpi <- rbind(mpi_biais_pr[474:480,175:194],mpi_biais_pr[1:13,175:194])
pr_fr_mpi_winter <- rbind(mpi_biais_pr_winter[474:480,175:194],mpi_biais_pr_winter[1:13,175:194])
pr_fr_mpi_summer <- rbind(mpi_biais_pr_summer[474:480,175:194],mpi_biais_pr_summer[1:13,175:194])
pr_fr_mpi_spring <- rbind(mpi_biais_pr_spring[474:480,175:194],mpi_biais_pr_spring[1:13,175:194])
pr_fr_mpi_fall <- rbind(mpi_biais_pr_fall[474:480,175:194],mpi_biais_pr_fall[1:13,175:194])

biais_model3_eur <- rbind(biais_model3[454:480,167:221],biais_model3[1:81,167:221])
biais_model3_eur_winter <- rbind(biais_model3_winter[454:480,167:221],biais_model3_winter[1:81,167:221])
biais_model3_eur_summer <- rbind(biais_model3_summer[454:480,167:221],biais_model3_summer[1:81,167:221])
biais_model3_eur_spring <- rbind(biais_model3_spring[454:480,167:221],biais_model3_spring[1:81,167:221])
biais_model3_eur_fall <- rbind(biais_model3_fall[454:480,167:221],biais_model3_fall[1:81,167:221])

biais_model3_fr <- rbind(biais_model3[474:480,175:194],biais_model3[1:13,175:194])
biais_model3_fr_winter <- rbind(biais_model3_winter[474:480,175:194],biais_model3_winter[1:13,175:194])
biais_model3_fr_summer <- rbind(biais_model3_summer[474:480,175:194],biais_model3_summer[1:13,175:194])
biais_model3_fr_spring <- rbind(biais_model3_spring[474:480,175:194],biais_model3_spring[1:13,175:194])
biais_model3_fr_fall <- rbind(biais_model3_fall[474:480,175:194],biais_model3_fall[1:13,175:194])

tas_eur_erai <- rbind(tas_erai[454:480,167:221],tas_erai[1:81,167:221])-273.15
tas_eur_erai_winter <- rbind(tas_erai_winter[454:480,167:221],tas_erai_winter[1:81,167:221])-273.15
tas_eur_erai_summer <- rbind(tas_erai_summer[454:480,167:221],tas_erai_summer[1:81,167:221])-273.15
tas_eur_erai_spring <- rbind(tas_erai_spring[454:480,167:221],tas_erai_spring[1:81,167:221])-273.15
tas_eur_erai_fall <- rbind(tas_erai_fall[454:480,167:221],tas_erai_fall[1:81,167:221])-273.15

tas_fr_erai <- rbind(tas_erai[474:480,175:194],tas_erai[1:13,175:194])-273.15
tas_fr_erai_winter <- rbind(tas_erai_winter[474:480,175:194],tas_erai_winter[1:13,175:194])-273.15
tas_fr_erai_summer <- rbind(tas_erai_summer[474:480,175:194],tas_erai_summer[1:13,175:194])-273.15
tas_fr_erai_spring <- rbind(tas_erai_spring[474:480,175:194],tas_erai_spring[1:13,175:194])-273.15
tas_fr_erai_fall <- rbind(tas_erai_fall[474:480,175:194],tas_erai_fall[1:13,175:194])-273.15

pr_eur_erai <- rbind(pr_erai[454:480,167:221],pr_erai[1:81,167:221])
pr_eur_erai_winter <- rbind(pr_erai_winter[454:480,167:221],pr_erai_winter[1:81,167:221])
pr_eur_erai_summer <- rbind(pr_erai_summer[454:480,167:221],pr_erai_summer[1:81,167:221])
pr_eur_erai_spring <- rbind(pr_erai_spring[454:480,167:221],pr_erai_spring[1:81,167:221])
pr_eur_erai_spring <- rbind(pr_erai_fall[454:480,167:221],pr_erai_fall[1:81,167:221])

pr_fr_erai <- rbind(pr_erai[474:480,175:194],pr_erai[1:13,175:194])-273.15
pr_fr_erai_winter <- rbind(pr_erai_winter[474:480,175:194],pr_erai_winter[1:13,175:194])
pr_fr_erai_summer <- rbind(pr_erai_summer[474:480,175:194],pr_erai_summer[1:13,175:194])
pr_fr_erai_spring <- rbind(pr_erai_spring[474:480,175:194],pr_erai_spring[1:13,175:194])
pr_fr_erai_spring <- rbind(pr_erai_fall[474:480,175:194],pr_erai_fall[1:13,175:194])

# Zlim
eur_biais = c(biais_model1_eur,biais_model2_eur,biais_model3_eur)
eur_tas = c(tas_eur_cnrm,tas_eur_ipsl,tas_eur_erai,tas_eur_mpi)
eur_pr = c(pr_eur_cnrm,pr_eur_ipsl,pr_eur_erai,pr_eur_mpi)

fr_biais = c(biais_model1_fr,biais_model2_fr,biais_model3_fr)
fr_tas = c(tas_fr_cnrm,tas_fr_ipsl,tas_fr_erai,tas_fr_mpi)
fr_pr = c(pr_fr_cnrm,pr_fr_ipsl,pr_fr_erai,pr_fr_mpi)

eur_biais_winter = c(biais_model1_eur_winter,biais_model2_eur_winter,biais_model3_eur_winter)
eur_biais_summer = c(biais_model1_eur_summer,biais_model2_eur_summer,biais_model3_eur_summer)
eur_biais_spring = c(biais_model1_eur_spring,biais_model2_eur_spring,biais_model3_eur_spring)
eur_biais_fall = c(biais_model1_eur_fall,biais_model2_eur_fall,biais_model3_eur_fall)

fr_biais_winter = c(biais_model1_fr_winter,biais_model2_fr_winter,biais_model3_fr_winter)
fr_biais_summer = c(biais_model1_fr_summer,biais_model2_fr_summer,biais_model3_fr_summer)
fr_biais_spring = c(biais_model1_fr_spring,biais_model2_fr_spring,biais_model3_fr_spring)
fr_biais_fall = c(biais_model1_fr_fall,biais_model2_fr_fall,biais_model3_fr_fall)