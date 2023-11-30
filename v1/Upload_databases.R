#La lettura dei file prende tempo: se la metti fuori il sito mette molto a caricarsi,
#Se la metti dentro i modelli ci mettono molto a girare
#Creare una nuova finestra con Data Load??
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>
#>
print("...Uploading databases....")
ptm_data <- proc.time()

lon_lat_nan<-read.table("data/LITTER/lon_lat_pairs.txt",header = TRUE)


######---------------------------------######-----------------------######-------
###############
#Uncompress files

if(!is.null("data/SOIL/Clay_WGS84.nc")){
  bunzip2("data/SOIL/Clay_WGS84.nc.bz2",
                        ext="bz2", FUN=bzfile)}


if(!is.null("data/SOIL/CN_WGS84.nc")){
  bunzip2("data/SOIL/CN_WGS84.nc.bz2", 
                      ext="bz2", FUN=bzfile)}

if(!is.null("data/SOIL/Silt_WGS84.nc")){
  Silt_file_path<-bunzip2("data/SOIL/Silt_WGS84.nc.bz2", 
                        ext="bz2", FUN=bzfile)}

if(!is.null("data/SOIL/socstockseu26_WGS84.nc")){
  bunzip2("data/SOIL/socstockseu26_WGS84.nc.bz2", 
                       ext="bz2", FUN=bzfile)}

if(!is.null("data/SOIL/Bulk_density_WGS84.nc")){
  BD_file_path<-bunzip2("data/SOIL/Bulk_density_WGS84.nc.bz2", 
                      ext="bz2", FUN=bzfile)}



##
if(!is.null("data/LITTER/npp_average_rcp26_EU_annual_2006_2099_fixed.nc4")){
  bunzip2("data/LITTER/npp_average_rcp26_EU_annual_2006_2099_fixed.nc4.bz2", 
                                ext="bz2", FUN=bzfile)}

if(!is.null("data/LITTER/npp_average_rcp60_EU_annual_2006_2099_fixed.nc4")){
  bunzip2("data/LITTER/npp_average_rcp60_EU_annual_2006_2099_fixed.nc4.bz2", 
                                ext="bz2", FUN=bzfile)}

if(!is.null("data/LITTER/npp_average_rcp60_EU_annual_2006_2099_var.nc4")){
  bunzip2("data/LITTER/npp_average_rcp60_EU_annual_2006_2099_var.nc4.bz2", 
                                    ext="bz2", FUN=bzfile)}

if(!is.null("data/LITTER/npp_average_rcp26_EU_annual_2006_2099_var.nc4")){
  bunzip2("data/LITTER/npp_average_rcp26_EU_annual_2006_2099_var.nc4.bz2", 
                                    ext="bz2", FUN=bzfile)}


if(!is.null("data/LITTER/cveg_average_rcp26_EU_annual_2006_2099.nc4")){
  bunzip2("data/LITTER/cveg_average_rcp26_EU_annual_2006_2099.nc4.bz2", 
                                       ext="bz2", FUN=bzfile)}

if(!is.null("data/LITTER/cveg_average_rcp60_EU_annual_2006_2099.nc4")){
  bunzip2("data/LITTER/cveg_average_rcp60_EU_annual_2006_2099.nc4.bz2", 
                                       ext="bz2", FUN=bzfile)}



#####
if(!is.null("data/RCP26/DAILY_FORCING/tas_month_rcp26_2006_2100_mm.nc4")){
  bunzip2("data/RCP26/DAILY_FORCING/tas_month_rcp26_2006_2100_mm.nc4.bz2", 
                               ext="bz2", FUN=bzfile)}

if(!is.null("data/RCP26/DAILY_FORCING/prec_month_rcp26_2006_2100_mm.nc4")){
  bunzip2("data/RCP26/DAILY_FORCING/prec_month_rcp26_2006_2100_mm.nc4.bz2", 
                                ext="bz2", FUN=bzfile)}

if(!is.null("data/RCP26/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp26_mm3mm3_top18_mm.nc4")){
  bunzip2("data/RCP26/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp26_mm3mm3_top18_mm.nc4.bz2", 
                                ext="bz2", FUN=bzfile)}




if(!is.null("data/RCP26/MONTHLY_OUTPUTS/potevap_month_rcp26_2006_2100_mm.nc4")){
  bunzip2("data/RCP26/MONTHLY_OUTPUTS/potevap_month_rcp26_2006_2100_mm.nc4.bz2", 
                                   ext="bz2", FUN=bzfile)}

if(!is.null("data/RCP60/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp60_mm3mm3_top18_mm.nc4")){
  bunzip2("data/RCP60/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp60_mm3mm3_top18_mm.nc4.bz2", 
                                ext="bz2", FUN=bzfile)}

if(!is.null("data/RCP60/MONTHLY_OUTPUTS/potevap_month_rcp60_2006_2100_mm.nc4")){
  bunzip2("data/RCP60/MONTHLY_OUTPUTS/potevap_month_rcp60_2006_2100_mm.nc4.bz2", 
                                  ext="bz2", FUN=bzfile)}

if(!is.null("data/RCP60/DAILY_FORCING/prec_month_rcp60_2006_2100_mm.nc4")){
  bunzip2("data/RCP60/DAILY_FORCING/prec_month_rcp60_2006_2100_mm.nc4.bz2", 
                               ext="bz2", FUN=bzfile)}

if(!is.null("data/RCP60/DAILY_FORCING/tas_month_rcp60_2006_2100_mm.nc4")){
  bunzip2("data/RCP60/DAILY_FORCING/tas_month_rcp60_2006_2100_mm.nc4.bz2", 
                              ext="bz2", FUN=bzfile)}



#----------------------------------------------------
#Read potential evapotranspiration file rcp26
#----------------------------------------------------
potevap_file_rcp26<-nc_open("data/RCP26/MONTHLY_OUTPUTS/potevap_month_rcp26_2006_2100_mm.nc4")
#potevap_file_rcp26<-nc_open(paste0(temp_file,"/potevap_month_rcp26_2006_2100_mm.nc4"))

#----------------------------------------------------
#Read lon-lat-time variables
lon_potevap_rcp26 <- ncvar_get(potevap_file_rcp26, varid = "lon")
lat_potevap_rcp26 <- ncvar_get(potevap_file_rcp26, varid = "lat")
potevap_time_rcp26 <- as.Date("1661-1-1") %m+% months(potevap_file_rcp26$dim$time$vals)
#Convert time to dates
potevap_time_plot_rcp26 <- as.Date.character(format(potevap_time_rcp26, "%Y-%m-%d"))
#Retrieve variable for whole time length
potevap_rcp26 <- ncvar_get(potevap_file_rcp26, "potevap")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------------------------------
#Read potential evapotranspiration file rcp60
#----------------------------------------------------
potevap_file_rcp60<-nc_open("data/RCP60/MONTHLY_OUTPUTS/potevap_month_rcp60_2006_2100_mm.nc4")
#potevap_file_rcp60<-nc_open(paste0(temp_file,"/potevap_month_rcp60_2006_2100_mm.nc4"))

#----------------------------------------------------
#Read lon-lat-time variables
lon_potevap_rcp60 <- ncvar_get(potevap_file_rcp60, varid = "lon")
lat_potevap_rcp60 <- ncvar_get(potevap_file_rcp60, varid = "lat")
potevap_time_rcp60 <- as.Date("1661-1-1") %m+% months(potevap_file_rcp60$dim$time$vals)
#Convert time to dates
potevap_time_plot_rcp60 <- as.Date.character(format(potevap_time_rcp60, "%Y-%m-%d"))
#Retrieve variable for whole time length
potevap_rcp60 <- ncvar_get(potevap_file_rcp60, "potevap")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------------------------------
#Read temperature file rcp26
#----------------------------------------------------
temp_file_rcp26<-nc_open("data/RCP26/DAILY_FORCING/tas_month_rcp26_2006_2100_mm.nc4")
#temp_file_rcp26<-nc_open(paste0(temp_file,"/tas_month_rcp26_2006_2100_mm.nc4"))

#----------------------------------------------------
#Read lon-lat-time variables
lon_temp_rcp26 <- ncvar_get(temp_file_rcp26, varid = "lon")
lat_temp_rcp26 <- ncvar_get(temp_file_rcp26, varid = "lat")
#temp_time_rcp26 <- as.Date("2006-1-1") %d+% days(temp_file_rcp26$dim$time$vals)
temp_time_rcp26 <- nc.get.time.series(temp_file_rcp26, v = "tas",
                                      time.dim.name = "time")
#Convert time to dates
temp_time_plot_rcp26 <- as.Date.character(format(temp_time_rcp26, "%Y-%m-%d"))
#Retrieve variable for whole time length
temp_rcp26 <- ncvar_get(temp_file_rcp26, "tas")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------------------------------
#Read temperature file rcp60
#----------------------------------------------------
temp_file_rcp60<-nc_open("data/RCP60/DAILY_FORCING/tas_month_rcp60_2006_2100_mm.nc4")
#temp_file_rcp60<-nc_open(paste0(temp_file,"/tas_month_rcp60_2006_2100_mm.nc4"))
#----------------------------------------------------
#Read lon-lat-time variables
lon_temp_rcp60 <- ncvar_get(temp_file_rcp60, varid = "lon")
lat_temp_rcp60 <- ncvar_get(temp_file_rcp60, varid = "lat")
#temp_time_rcp60 <- as.Date("2006-1-1") %d+% days(temp_file_rcp60$dim$time$vals)
temp_time_rcp60 <- nc.get.time.series(temp_file_rcp60, v = "tas",
                                      time.dim.name = "time")
#Convert time to dates
temp_time_plot_rcp60 <- as.Date.character(format(temp_time_rcp60, "%Y-%m-%d"))
#Retrieve variable for whole time length
temp_rcp60 <- ncvar_get(temp_file_rcp60, "tas")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------------------------------
#Read prec file rcp26 (rain+snow)
#----------------------------------------------------
prec_file_rcp26<-nc_open("data/RCP26/DAILY_FORCING/prec_month_rcp26_2006_2100_mm.nc4")
#prec_file_rcp26<-nc_open(paste0(temp_file,"/prec_month_rcp26_2006_2100_mm.nc4"))
#----------------------------------------------------
#Read lon-lat-time variables
lon_prec_rcp26 <- ncvar_get(prec_file_rcp26, varid = "lon")
lat_prec_rcp26 <- ncvar_get(prec_file_rcp26, varid = "lat")
#prec_time_rcp26 <- as.Date("2006-1-1") %d+% days(prec_file_rcp26$dim$time$vals)
prec_time_rcp26 <- nc.get.time.series(prec_file_rcp26, v = "pr",
                                      time.dim.name = "time")
#Convert time to dates
prec_time_plot_rcp26 <- as.Date.character(format(prec_time_rcp26, "%Y-%m-%d"))
#Retrieve variable for whole time length
prec_rcp26 <- ncvar_get(prec_file_rcp26, "pr")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#----------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------------------------------
#Read prec file rcp60
#----------------------------------------------------
prec_file_rcp60<-nc_open("data/RCP60/DAILY_FORCING/prec_month_rcp60_2006_2100_mm.nc4")
#prec_file_rcp60<-nc_open(paste0(temp_file,"/prec_month_rcp60_2006_2100_mm.nc4"))
#----------------------------------------------------
#Read lon-lat-time variables
lon_prec_rcp60 <- ncvar_get(prec_file_rcp60, varid = "lon")
lat_prec_rcp60 <- ncvar_get(prec_file_rcp60, varid = "lat")
#prec_time_rcp60 <- as.Date("2006-1-1") %d+% days(prec_file_rcp60$dim$time$vals)
prec_time_rcp60 <- nc.get.time.series(prec_file_rcp60, v = "pr",
                                      time.dim.name = "time")
#Convert time to dates
prec_time_plot_rcp60 <- as.Date.character(format(prec_time_rcp60, "%Y-%m-%d"))
#Retrieve variable for whole time length
prec_rcp60 <- ncvar_get(prec_file_rcp60, "pr")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------------------------------
#Read soil moisture file rcp26
#----------------------------------------------------
vswc_file_rcp26<-nc_open("data/RCP26/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp26_mm3mm3_top18_mm.nc4")
#vswc_file_rcp26<-nc_open(paste0(temp_file,"/soilmoist_2006_2009_rcp26_mm3mm3_top18_mm.nc4"))
#----------------------------------------------------
#Read lon-lat-time variables
lon_vswc_rcp26 <- ncvar_get(vswc_file_rcp26, varid = "lon")
lat_vswc_rcp26 <- ncvar_get(vswc_file_rcp26, varid = "lat")
#lay_vswc_rcp60 <- ncvar_get(vswc_file_rcp26, varid = "solay")

vswc_time_rcp26 <- as.Date("1661-1-1") %m+% months(vswc_file_rcp26$dim$time$vals)
#Convert time to dates
vswc_time_plot_rcp26 <- as.Date.character(format(vswc_time_rcp26, "%Y-%m-%d"))
#Retrieve variable for whole time length
vswc_rcp26 <- ncvar_get(vswc_file_rcp26, "soilmoist")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------------------------------
#Read soil moisture file rcp60
#----------------------------------------------------
vswc_file_rcp60<-nc_open("data/RCP60/MONTHLY_OUTPUTS/soilmoist_2006_2009_rcp60_mm3mm3_top18_mm.nc4")
#vswc_file_rcp60<-nc_open(paste0(temp_file,"/soilmoist_2006_2009_rcp60_mm3mm3_top18_mm.nc4"))

#----------------------------------------------------
#Read lon-lat-time variables
lon_vswc_rcp60 <- ncvar_get(vswc_file_rcp60, varid = "lon")
lat_vswc_rcp60 <- ncvar_get(vswc_file_rcp60, varid = "lat")
vswc_time_rcp60 <- as.Date("1661-1-1") %m+% months(vswc_file_rcp60$dim$time$vals)
#Convert time to dates
vswc_time_plot_rcp60 <- as.Date.character(format(vswc_time_rcp60, "%Y-%m-%d"))
#Retrieve variable for whole time length
vswc_rcp60 <- ncvar_get(vswc_file_rcp60, "soilmoist")
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------------------------------
#Read SOIL MAPS
#----------------------------------------------------
#####
#Clay
#####
clay_file<-nc_open("data/SOIL/Clay_WGS84.nc")
#clay_file<-nc_open(paste0(temp_file,"/Clay_WGS84.nc"))
#Read lon-lat variables
lon_clay <- ncvar_get(clay_file, varid = "lon")
lat_clay <- ncvar_get(clay_file, varid = "lat")
#Retrieve variable
clay_data <- ncvar_get(clay_file, "Band1")

#####
#Silt
#####
silt_file<-nc_open("data/SOIL/Silt_WGS84.nc")
#silt_file<-nc_open(paste0(temp_file,"/Silt_WGS84.nc"))
#Read lon-lat variables
lon_silt <- ncvar_get(silt_file, varid = "lon")
lat_silt <- ncvar_get(silt_file, varid = "lat")
#Retrieve variable
silt_data <- ncvar_get(silt_file, "Band1")

#####
#CN
#####
CN_file<-nc_open("data/SOIL/CN_WGS84.nc")
#CN_file<-nc_open(paste0(temp_file,"/CN_WGS84.nc"))
#Read lon-lat variables
lon_CN <- ncvar_get(CN_file, varid = "lon")
lat_CN <- ncvar_get(CN_file, varid = "lat")
#Retrieve variable
CN_data <- ncvar_get(CN_file, "Band1")

#####
#ph_CaCl
#####
# ph_CaCl_file<-nc_open("data/SOIL/ph_CaCl_WGS84.nc")
# #Read lon-lat variables
# lon_ph_CaCl <- ncvar_get(ph_CaCl_file, varid = "lon")
# lat_ph_CaCl <- ncvar_get(ph_CaCl_file, varid = "lat")
# #Retrieve variable
# ph_CaCl_data <- ncvar_get(ph_CaCl_file, "Band1")

#####
#bulk_density
#####
BD_file<-nc_open("data/SOIL/Bulk_density_WGS84.nc")
#BD_file<-nc_open(paste0(temp_file,"/Bulk_density_WGS84.nc"))
#Read lon-lat variables
lon_BD <- ncvar_get(BD_file, varid = "lon")
lat_BD <- ncvar_get(BD_file, varid = "lat")
#Retrieve variable
BD_data <- ncvar_get(BD_file, "Band1")

#####
#SOC stocks
#####
SOC_file<-nc_open("data/SOIL/socstockseu26_WGS84.nc")
#SOC_file<-nc_open(paste0(temp_file,"/socstockseu26_WGS84.nc"))
#Read lon-lat variables
lon_SOC <- ncvar_get(SOC_file, varid = "lon")
lat_SOC <- ncvar_get(SOC_file, varid = "lat")
#Retrieve variable
SOC_data <- ncvar_get(SOC_file, "Band1")

#----------------------------------------------------
#Read NPP DATA - FIXED LAND USE
#----------------------------------------------------

# #####
# #RCP26
# #####

litter_file_rcp26<-nc_open("data/LITTER/npp_average_rcp26_EU_annual_2006_2099_fixed.nc4")
#litter_file_rcp26<-nc_open(paste0(temp_file,"/npp_average_rcp26_EU_annual_2006_2099_fixed.nc4"))
#----------------------------------------------------
#Read lon-lat-time variables
lon_litter_rcp26 <- ncvar_get(litter_file_rcp26, varid = "lon")
lat_litter_rcp26 <- ncvar_get(litter_file_rcp26, varid = "lat")
#Convert time to dates
litter_time_rcp26 <- as.Date("1661-1-1") %m+% years(litter_file_rcp26$dim$time$vals)

#Retrieve variable for whole time length
litter_rcp26_npp <- ncvar_get(litter_file_rcp26, "npp") #kg/m2/sec

#convert npp (#kg/m2/sec) to foliar Litterfall carbon with constant coefficient (see Neumann et al., 2018 and Malhi et al., 2011)
litter_rcp26_ag <- litter_rcp26_npp*0.34
#calculate belowground carbon input with equation from Jonard et al., 2007 fineroot_in (g/m2/yr) =0.333*(1.92*AG_litterfall(g/m2/yr)+130)
#where *1000*(60*60*24*365.25) is the conversion factor from kg/m2/sec to (g/m2/yr)
litter_rcp26_bg <- 0.333*(1.92*(litter_rcp26_ag*1000*(60*60*24*365.25))+130)/(1000*(60*60*24*365.25)) 
#calculate total litter input (kg/m2/sec)
litter_rcp26<- litter_rcp26_ag+litter_rcp26_bg

# 
# #####
# #RCP60
# ##### 
litter_file_rcp60<-nc_open("data/LITTER/npp_average_rcp60_EU_annual_2006_2099_fixed.nc4")
#litter_file_rcp60<-nc_open(paste0(temp_file,"/npp_average_rcp60_EU_annual_2006_2099_fixed.nc4"))
#----------------------------------------------------
#Read lon-lat-time variables
lon_litter_rcp60 <- ncvar_get(litter_file_rcp60, varid = "lon")
lat_litter_rcp60 <- ncvar_get(litter_file_rcp60, varid = "lat")
#Convert time to dates
litter_time_rcp60 <- as.Date("1661-1-1") %m+% years(litter_file_rcp60$dim$time$vals)
# #Convert time to dates
# litter_time_plot_rcp60 <- as.Date.character(format(litter_time_rcp60, "%Y-%m-%d"))
#Retrieve variable for whole time length
litter_rcp60_npp <- ncvar_get(litter_file_rcp60, "npp")

#convert npp (#kg/m2/sec) to foliar Litterfall carbon with constant coefficient (see Neumann et al., 2018 and Malhi et al., 2011)
litter_rcp60_ag <- litter_rcp60_npp*0.34
#calculate belowground carbon input with equation from Jonard et al., 2007 fineroot_in (g/m2/yr) =0.333*(1.92*AG_litterfall(g/m2/yr)+130)
#where *1000*(60*60*24*365.25) is the conversion factor from kg/m2/sec to (g/m2/yr)
litter_rcp60_bg <- 0.333*(1.92*(litter_rcp60_ag*1000*(60*60*24*365.25))+130)/(1000*(60*60*24*365.25)) 
#calculate total litter input (kg/m2/sec)
litter_rcp60<- litter_rcp60_ag+litter_rcp60_bg

#----------------------------------------------------
#Read NPP DATA with variable land use
#----------------------------------------------------
# #####
# #RCP26 var
# #####

litter_LU_file_rcp26<-nc_open("data/LITTER/npp_average_rcp26_EU_annual_2006_2099_var.nc4")
#litter_LU_file_rcp26<-nc_open(paste0(temp_file,"/npp_average_rcp26_EU_annual_2006_2099_var.nc4"))
#----------------------------------------------------
#Read lon-lat-time variables
lon_litter_LU_rcp26 <- ncvar_get(litter_LU_file_rcp26, varid = "lon")
lat_litter_LU_rcp26 <- ncvar_get(litter_LU_file_rcp26, varid = "lat")
#Convert time to dates
litter_LU_time_rcp26 <- as.Date("1661-1-1") %m+% years(as.integer(litter_LU_file_rcp26$dim$time$vals))
#Convert time to dates
litter_LU_time_plot_rcp26 <- as.Date.character(format(litter_LU_time_rcp26, "%Y-%m-%d"))
#Retrieve variable for whole time length
litter_LU_rcp26_npp <- ncvar_get(litter_LU_file_rcp26, "npp")

#convert npp (#kg/m2/sec) to foliar Litterfall carbon with constant coefficient (see Neumann et al., 2018 and Malhi et al., 2011)
litter_LU_rcp26_ag <- litter_LU_rcp26_npp*0.34
#calculate belowground carbon input with equation from Jonard et al., 2007 fineroot_in (g/m2/yr) =0.333*(1.92*AG_litterfall(g/m2/yr)+130)
#where *1000*(60*60*24*365.25) is the conversion factor from kg/m2/sec to (g/m2/yr)
litter_LU_rcp26_bg <- 0.333*(1.92*(litter_LU_rcp26_ag*1000*(60*60*24*365.25))+130)/(1000*(60*60*24*365.25)) 
#calculate total litter input (kg/m2/sec)
litter_LU_rcp26<- litter_LU_rcp26_ag+litter_LU_rcp26_bg

# 
# #####
# #RCP60
# #####
litter_LU_file_rcp60<-nc_open("data/LITTER/npp_average_rcp60_EU_annual_2006_2099_var.nc4")
#litter_LU_file_rcp60<-nc_open(paste0(temp_file,"/npp_average_rcp60_EU_annual_2006_2099_var.nc4"))
#----------------------------------------------------
#Read lon-lat-time variables
lon_litter_LU_rcp60 <- ncvar_get(litter_LU_file_rcp60, varid = "lon")
lat_litter_LU_rcp60 <- ncvar_get(litter_LU_file_rcp60, varid = "lat")
#Convert time to dates
litter_LU_time_rcp60 <- as.Date("1661-1-1") %m+% years(as.integer(litter_LU_file_rcp60$dim$time$vals))
#Convert time to dates
litter_LU_time_plot_rcp60 <- as.Date.character(format(litter_LU_time_rcp60, "%Y-%m-%d"))
#Retrieve variable for whole time length
litter_LU_rcp60_npp <- ncvar_get(litter_LU_file_rcp60, "npp")

#convert npp (#kg/m2/sec) to foliar Litterfall carbon with constant coefficient (see Neumann et al., 2018 and Malhi et al., 2011)
litter_LU_rcp60_ag <- litter_LU_rcp60_npp*0.34
#calculate belowground carbon input with equation from Jonard et al., 2007 fineroot_in (g/m2/yr) =0.333*(1.92*AG_litterfall(g/m2/yr)+130)
#where *1000*(60*60*24*365.25) is the conversion factor from kg/m2/sec to (g/m2/yr)
litter_LU_rcp60_bg <- 0.333*(1.92*(litter_LU_rcp60_ag*1000*(60*60*24*365.25))+130)/(1000*(60*60*24*365.25)) 
#calculate total litter input (kg/m2/sec)
litter_LU_rcp60<- litter_LU_rcp60_ag+litter_LU_rcp60_bg

#------------------------------------------------------------------------------------------------
#Read cLITTER DATA (aboveground and belowground carbon mass in plant) for land management options
#-----------------------------------------------------------------------------------------------

#####
#RCP26
#####
veg_clitter_file_rcp26<-nc_open("data/LITTER/cveg_average_rcp26_EU_annual_2006_2099.nc4") #kg/m2
#veg_clitter_file_rcp26<-nc_open(paste0(temp_file,"/cveg_average_rcp26_EU_annual_2006_2099.nc4")) #kg/m2
#----------------------------------------------------
#Read lon-lat-time variables
lon_veg_clitter_rcp26 <- ncvar_get(veg_clitter_file_rcp26, varid = "lon")
lat_veg_clitter_rcp26 <- ncvar_get(veg_clitter_file_rcp26, varid = "lat")
#Convert time to dates
veg_clitter_time_rcp26 <- as.Date("1661-1-1") %m+% years(veg_clitter_file_rcp26$dim$time$vals)
#Convert time to dates
veg_clitter_time_plot_rcp26 <- as.Date.character(format(veg_clitter_time_rcp26, "%Y-%m-%d"))
#Retrieve variable for whole time length
veg_clitter_rcp26 <- ncvar_get(veg_clitter_file_rcp26, "cveg")


# #####
# #RCP60
# #####
# veg_clitter_file_rcp60<-nc_open("data/LITTER/cveg_average_rcp60_EU_annual_2006_2099.nc4")#kg/m2
# #----------------------------------------------------
# #Read lon-lat-time variables
# lon_veg_clitter_rcp60 <- ncvar_get(veg_clitter_file_rcp60, varid = "lon")
# lat_veg_clitter_rcp60 <- ncvar_get(veg_clitter_file_rcp60, varid = "lat")
# #Convert time to dates
# veg_clitter_time_rcp60 <- as.Date("1661-1-1") %m+% years(veg_clitter_file_rcp60$dim$time$vals)
# #Convert time to dates
# veg_clitter_time_plot_rcp60 <- as.Date.character(format(veg_clitter_time_rcp60, "%Y-%m-%d"))
# #Retrieve variable for whole time length
# veg_clitter_rcp60 <- ncvar_get(veg_clitter_file_rcp60, "cveg")

print(".......Databases uploaded......")
print("........................")
elapsed_time_data<- round((proc.time() - ptm_data)["elapsed"]/60,2)
print(paste0("Elapsed time:",elapsed_time_data," minutes")) #minutes
print("........................")

#--------------------------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------