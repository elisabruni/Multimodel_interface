
#################
#Compress files
################
# bzip2("/var/folders/yk/pr25cq6j09n1ktnhrq68sl600000gq/T/RtmpiEPSJS/Clay_WGS84.nc", 
#       ext="bz2", FUN=bzfile)

bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/SOIL/Clay_WGS84.nc", 
      ext="bz2", FUN=bzfile)


bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/SOIL/CN_WGS84.nc", 
      ext="bz2", FUN=bzfile)

bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/SOIL/Silt_WGS84.nc", 
      ext="bz2", FUN=bzfile)
bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/SOIL/socstockseu26_WGS84.nc", 
      ext="bz2", FUN=bzfile)

bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/SOIL/Bulk_density_WGS84.nc", 
      ext="bz2", FUN=bzfile)

##
bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/LITTER/npp_average_rcp26_EU_annual_2006_2099_fixed.nc4", 
      ext="bz2", FUN=bzfile)

bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/LITTER/npp_average_rcp60_EU_annual_2006_2099_fixed.nc4", 
      ext="bz2", FUN=bzfile)

bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/LITTER/npp_average_rcp60_EU_annual_2006_2099_var.nc4", 
      ext="bz2", FUN=bzfile)
bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/LITTER/npp_average_rcp26_EU_annual_2006_2099_var.nc4", 
      ext="bz2", FUN=bzfile)


bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/LITTER/cveg_average_rcp26_EU_annual_2006_2099.nc4", 
      ext="bz2", FUN=bzfile)
# bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/LITTER/cveg_average_rcp60_EU_annual_2006_2099.nc4", 
#       ext="bz2", FUN=bzfile)



#####
bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/RCP26/DAILY_FORCING/tas_month_rcp26_2006_2100_mm.nc4", 
      ext="bz2", FUN=bzfile)
bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/RCP26/DAILY_FORCING/prec_month_rcp26_2006_2100_mm.nc4", 
      ext="bz2", FUN=bzfile)

bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/RCP26/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp26_mm3mm3_top18_mm.nc4", 
      ext="bz2", FUN=bzfile)
bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/RCP26/MONTHLY_OUTPUTS/potevap_month_rcp26_2006_2100_mm.nc4", 
      ext="bz2", FUN=bzfile)

bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/RCP60/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp60_mm3mm3_top18_mm.nc4", 
      ext="bz2", FUN=bzfile)
bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/RCP60/MONTHLY_OUTPUTS/potevap_month_rcp60_2006_2100_mm.nc4", 
      ext="bz2", FUN=bzfile)

bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/RCP60/DAILY_FORCING/prec_month_rcp60_2006_2100_mm.nc4", 
      ext="bz2", FUN=bzfile)
bzip2("/Users/ebruni/Desktop/HOLISOILS/WEBAPP/v1/data/RCP60/DAILY_FORCING/tas_month_rcp60_2006_2100_mm.nc4", 
      ext="bz2", FUN=bzfile)


######---------------------------------######-----------------------######-------
###############
#Uncompress files


  
# clay_file_path<-bunzip2("data/SOIL/Clay_WGS84.nc.bz2",
#         ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# temp_file<-dirname(clay_file_path)
# 
# 
# 
# CN_file_path<-bunzip2("data/SOIL/CN_WGS84.nc.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# Silt_file_path<-bunzip2("data/SOIL/Silt_WGS84.nc.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# SOC_file_path<-bunzip2("data/SOIL/socstockseu26_WGS84.nc.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# BD_file_path<-bunzip2("data/SOIL/Bulk_density_WGS84.nc.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# ##
# litter_file_rcp26_path<-bunzip2("data/LITTER/npp_average_rcp26_EU_annual_2006_2099_fixed.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# litter_file_rcp60_path<-bunzip2("data/LITTER/npp_average_rcp60_EU_annual_2006_2099_fixed.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# litter_file_rcp60_var_path<-bunzip2("data/LITTER/npp_average_rcp60_EU_annual_2006_2099_var.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# litter_file_rcp26_var_path<-bunzip2("data/LITTER/npp_average_rcp26_EU_annual_2006_2099_var.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# 
# veg_clitter_file_rcp26_path <- bunzip2("data/LITTER/cveg_average_rcp26_EU_annual_2006_2099.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# veg_clitter_file_rcp60_path <- bunzip2("data/LITTER/cveg_average_rcp60_EU_annual_2006_2099.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# 
# 
# #####
# tas_file_rcp26_path <- bunzip2("data/RCP26/DAILY_FORCING/tas_month_rcp26_2006_2100_mm.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# prec_file_rcp26_path <- bunzip2("data/RCP26/DAILY_FORCING/prec_month_rcp26_2006_2100_mm.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# vswc_file_rcp26_path <- bunzip2("data/RCP26/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp26_mm3mm3_top18_mm.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# potevap_file_rcp26_path <- bunzip2("data/RCP26/MONTHLY_OUTPUTS/potevap_month_rcp26_2006_2100_mm.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# vswc_file_rcp60_path <- bunzip2("data/RCP60/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp60_mm3mm3_top18_mm.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# potevap_file_rcp60_path <-bunzip2("data/RCP60/MONTHLY_OUTPUTS/potevap_month_rcp60_2006_2100_mm.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# 
# prec_file_rcp60_path <-bunzip2("data/RCP60/DAILY_FORCING/prec_month_rcp60_2006_2100_mm.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)
# tas_file_rcp60_path <-bunzip2("data/RCP60/DAILY_FORCING/tas_month_rcp60_2006_2100_mm.nc4.bz2", 
#       ext="bz2", FUN=bzfile,temporary=TRUE)



#------



