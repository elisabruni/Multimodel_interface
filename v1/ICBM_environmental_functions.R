#----------------------------------------------------------
#ICBM environmental functions affecting decomposition
#----------------------------------------------------------
###################
#Moisture function
###################
fW.ICBM<-function(clay, water_in_m3){
  #clay g/g
  # mcw = wilting point
  # mcfc = field capacity
  
  water_in_m3 = as.numeric(water_in_m3)

  #--------
  gamma=0.85 #From graph in Karlsson et al 2011
  rmin=0.55 #From graph in Karlsson et al 2011
  alpha=0.5 #From Fortin et al. 2011
  #--------
  #From katterer et al 2006
  mcfc=0.2699+0.3247*clay
  mcw=0.0284+0.3790*clay
  
  #water_in_m3 = np.maximum(water_in_m3,alpha*mcw)
  re_wat<-c()
  for(i in 1:length(water_in_m3)){
    if(water_in_m3[i]<alpha*mcw){
      re_wat[i]=0.18*(water_in_m3[i]/(alpha*mcw))
    }else if((water_in_m3[i]>alpha*mcw | water_in_m3[i]==alpha*mcw) & (water_in_m3[i]<gamma*mcfc | water_in_m3[i]==gamma*mcfc)){
      re_wat[i]=0.18+(1.-0.18)*((water_in_m3[i]-alpha*mcw)/(gamma*mcfc-alpha*mcw))
    }else if(water_in_m3[i]>gamma*mcfc){
      re_wat[i]=1+(1-rmin)*((water_in_m3[i]-gamma*mcfc)/(gamma*mcfc-mcfc))
    }
  }
  re_wat[re_wat<0]=0 #added to avoid negative values
  return(re_wat)
}


#######################
#Temperature function
#######################
fT.ICBM<-function(Tsoil){
  #----------
  Tmin=-3.8 #From Karlsson et al 2011
  #----------
  Tsoil = as.numeric(Tsoil)
  temp_func<-c()
  for(i in 1:length(Tsoil)){
    temp_func[i]=max(0.,((Tsoil[i]-Tmin)^2)/((30-Tmin)^2))
    
  }
  return(temp_func)
}


ultuna_coeff=0.07246223 #default Ultuna r value for 1947-2017 previously calculated
#ultuna_coeff=NA #set to NA if you need to calculate the default r value for Ultuna

if(is.na(ultuna_coeff)==TRUE){
  #Use Ultuna data to calculate default reduction coefficient for normalization
  #Ultuna test site
  #Climate data (1947 to 2017)
  PATH_data <-"/Users/ebruni/Desktop/DOTTORATO/DATI/SUEDE_Katterer/"
  #Read data
  dat<-readxl::read_excel(paste0(PATH_data,"Ultuna climate 1947-2017.xls"))
  daily_Temp_Ultu = dat$Temperature #C
  day_number = length(daily_Temp_Ultu)  
  
  #print(day_number/365)
  #humidity data (1925 to 1955)
  PATH_data_hum_1925_1955 <-"/Users/ebruni/Desktop/DOTTORATO/SCRIPT_MODELLI/ULTU/hum_ULTU_1926_1955.txt"
  soil_moist_Ultu_1925_1955<-read.table(PATH_data_hum_1925_1955)$V1/100. #From kg(H2O)/m2(soil) to m3(H2O)/m3(soil)
  soil_moist_Ultu_1947_1955<-soil_moist_Ultu_1925_1955[((1947-1925-1)*365):length(soil_moist_Ultu_1925_1955)]
  length(soil_moist_Ultu_1947_1955)/365
  #humidity data (1956 to 2008)
  PATH_data_hum_1956_2008 <-"/Users/ebruni/Desktop/DOTTORATO/SCRIPT_MODELLI/ULTU/hum_ULTU_1956_2008.txt"
  soil_moist_Ultu_1956_2008<-read.table(PATH_data_hum_1956_2008)$V1/100. #From kg(H2O)/m2(soil) to m3(H2O)/m3(soil)
  length(soil_moist_Ultu_1956_2008)/365
  
  #Add first 8 years of moisture data
  daily_Moist_Ultu_1947_2008<- c(soil_moist_Ultu_1947_1955,soil_moist_Ultu_1956_2008)
  #print(length(daily_Moist_Ultu_1947_2008)/365)
  #daily_Moist_Ultu = rep_len(soil_moist_Ultu,day_number)
  #Repeat last 9 years, to add from 2008 to 2017 unavailable data
  # daily_Moist_Ultu_2009_2017 = daily_Moist_Ultu_1947_2008[((2008-(2017-2008)-1947-1)*365):length(daily_Moist_Ultu_1947_2008)]
  # daily_Moist_Ultu = c(daily_Moist_Ultu_1947_2008,daily_Moist_Ultu_2009_2017)
  #daily_Moist_Ultu = dat$humidity/100 #m3/m3
  
  #add last missing years
  daily_Moist_Ultu = rep_len(daily_Moist_Ultu_1947_2008,day_number)
  #print(length(daily_Moist_Ultu)/365)
  
  clay_Ultu = 0.365 #g/g
  
  re_water_daily = fW.ICBM(clay_Ultu, daily_Moist_Ultu)
  re_temp_daily = fT.ICBM(daily_Temp_Ultu)
  coef_daily_Ultu=re_water_daily*re_temp_daily
  #From 1947 to 2013 (moisture data recycled from 1954 to 1980)
  Ymean_coef_daily_Ultu = colMeans(matrix(coef_daily_Ultu[1:(365*71)],365))
  
  
  mean_coef_daily_Ultu = mean(Ymean_coef_daily_Ultu)
  
  print("Yearly average r_clim for Ultuna")
  print(Ymean_coef_daily_Ultu)
  print("Average r_clim for Ultuna")
  print(mean_coef_daily_Ultu)
  
  #Ultuna r_clim: must be used for normalization for other sites
  ultuna_coeff=mean_coef_daily_Ultu
}


#######################
#Yearly climate function
#######################
fClim.ICBM<-function(re_temp,re_wat,coef_ultu){
  if(length(re_temp)==length(re_wat)){
    #Daily climate reduction coefficient for ICBM, normalized against Ultuna
    re_clim_daily  = (re_temp*re_wat)/coef_ultu
  }else{
    print("Check soil temperature and humidity data. They are not the same length")
    print(paste0("length re_temp = ",length(re_temp)))
    print(paste0("length re_wat = ",length(re_wat)))
    min_length = min(length(re_temp),length(re_wat))
    re_temp = re_temp[1:min_length]
    re_wat = re_wat[1:min_length]
    re_clim_daily  = (re_temp*re_wat)/coef_ultu
  }
  
  #Average climate reduction coefficient for ICBM, already normalized against Ultuna
  #re_clim = colMeans(matrix(re_clim_daily[1:time_steps_simu],365))
  re_clim = mean(re_clim_daily)
  
  return(re_clim)
}