###################################################
#ICBM model
###################################################
#This function launches the SoilR version of ICBM
#First, a spin-up run is performed
#Then, the estimated C pool fractions are used to rescale total SOC stocks with measured SOC stocks
#Finally, a forward run is perfomed
#The function provides SOC stock and CO2 fluxes for the chosen simulation lenght

#ICBM
#' Andren, O. and T. Katterer. 1997. ICBM: The Introductory Carbon
#' Balance Model for Exploration of Soil Carbon Balances. Ecological
#' Applications 7:1226-1236.


#CHANGE! Currently using same environmental function as RothC

Call_ICBM_i1<-function(plot_figures,
                       temperature_spinup, soilmoisture_spinup,
                       temperature_fwd, soilmoisture_fwd,
                       SOC_0,C_input_ag_spinup,C_input_bg_spinup,C_input_ag_fwd,C_input_bg_fwd,
                       clay_p,
                       #soil_thickness,
                       decomposition_param_ICBM,
                       t_spinup,t_fwd,
                       spinupcheck, thresholdspin){
  
  
  #Convert variables in ICBM
  InICBM_spinup = C_input_ag_spinup+C_input_bg_spinup #MgC/yr
  ts_spinup_ICBM = t_spinup      #yr
  
  InICBM_fwd = C_input_ag_fwd+C_input_bg_fwd       #MgC/yr
  ts_fwd_ICBM = t_fwd            #yr
  
  clay_ICBM = clay_p/100 #% to g/g
  
  # #Convert daily to monthly temperature
  # temperature_spinup_ICBM = aggregate(temperature_spinup, list(format(temperature_spinup$Date,"%Y-%m")),mean, na.rm = TRUE)
  # temperature_fwd_ICBM = aggregate(temperature_fwd, list(format(temperature_fwd$Date,"%Y-%m")),mean, na.rm = TRUE)
  # 
  # #Convert daily precipitation to monthly precipitation
  # precipitation_spinup_ICBM <- aggregate(precipitation_spinup["Precip"], list(format(precipitation_spinup$Date,"%Y-%m")),FUN=sum, na.rm = TRUE)
  # precipitation_fwd_ICBM <- aggregate(precipitation_fwd["Precip"], list(format(precipitation_fwd$Date,"%Y-%m")),FUN=sum, na.rm = TRUE)
  # 
  # #Monthly potential evapotranspiration
  # potential_evapotranspiration_spinup_ICBM = potential_evapotranspiration_spinup
  # potential_evapotranspiration_fwd_ICBM = potential_evapotranspiration_fwd
  
  temperature_spinup_ICBM=temperature_spinup
  temperature_fwd_ICBM=temperature_fwd
  
  soilmoisture_spinup_ICBM=soilmoisture_spinup #m3/m3
  soilmoisture_fwd_ICBM=soilmoisture_fwd       #m3/m3
  #--
  #Rate modifiers
  
  #Rate modifier spinup
  fT_spinup=fT.ICBM(temperature_spinup_ICBM$Temp)
  fW_spinup=fW.ICBM(clay_ICBM, soilmoisture_spinup_ICBM)
  r_ICBM_spinup=fClim.ICBM(fT_spinup,fW_spinup,ultuna_coeff) #annual rate modifier
  
  #Rate modifier forward
  fT_fwd=fT.ICBM(temperature_fwd_ICBM$Temp)
  fW_fwd=fW.ICBM(clay_ICBM, soilmoisture_fwd_ICBM)
  r_ICBM_fwd=fClim.ICBM(fT_fwd,fW_fwd,ultuna_coeff) #annual rate modifier
  
  #*************************************************
  #ICBM
  #*************************************************
  #Rate modifiers
  
  # #Rate modifier spinup
  # r_ICBM_spinup = rep(mean(fClim_spinup),length.out=length(ts_spinup_ICBM))
  # 
  # #Rate modifier forward
  # r_ICBM_fwd = rep(fClim_spinup,length.out=length(ts_fwd_ICBM))
  
  
  #####
  #1)## Initialization
  #####
  
  ks_ICBM = decomposition_param_ICBM[1:2]
  h_ICBM = decomposition_param_ICBM[3]
  #Run spinup
  ICBM_spinup = ICBMModel(t_spinup,ks_ICBM,h_ICBM,r=r_ICBM_spinup,
                          c0=c(Y0=0,O0=0),In=InICBM_spinup,solver=deSolve.lsoda.wrapper, pass=FALSE)
  
  #Get SOC spinup
  CICBM_spinup=getC(ICBM_spinup)
  
  
  #Check that steady state is reached
  INIZ=CICBM_spinup[nrow(CICBM_spinup)-spinupcheck,] #initialize SOC pool values
  for(row in 1:nrow(tail(CICBM_spinup,spinupcheck))){ 
    pools_i = tail(CICBM_spinup,spinupcheck)[row,]
    deltai = (INIZ-pools_i)/INIZ
    
    if(all(deltai>thresholdspin)){ #Check that SOC stock variation of each pool is <thresholdspin for all years
      stop("spinup length should be increased, current annual SOC variation is ")
    }
    INIZ=pools_i
  }
  
  
  if(plot_figures==TRUE){
    #plot the pools
    matplot(t_spinup, CICBM_spinup, type="l", lty=1, col=1:2,
            xlab="Time (years)", ylab="C stocks (MgC/ha)",main="ICBM spinup")
    legend("topleft", c("Young", "Old"),
           lty=1, col=1:2, bty="n")
  }
  #####
  #2)## Relaxation
  #####
  #C after spinup in each pool
  CICBM_pools_spinup = CICBM_spinup[dim(CICBM_spinup)[1],]
  #Average total C after spinup
  CICBM_ACTtot_spinup = sum(CICBM_pools_spinup)
  
  #Proportion of C in each pool, relative to total C
  prop_ACTpools_CICBM = CICBM_pools_spinup/CICBM_ACTtot_spinup
  
  #Initial C in each pool after relaxation
  CICBM_ACTpools_relax = SOC_0*prop_ACTpools_CICBM
  
  
  #####
  #3)## Forward
  #####
  #t_prova=seq(1,simu_lenght,by=1)
  
  InICBM = mean(InICBM_fwd)
  
  ICBM_fwd = ICBMModel(t=t_fwd,ks_ICBM,h_ICBM,r=r_ICBM_fwd,
                       c0=CICBM_ACTpools_relax,In=InICBM,solver=deSolve.lsoda.wrapper, pass=FALSE)

  
  #Get SOC fwd
  CICBM_fwd=getC(ICBM_fwd)
  
  #Outputs of C
  Rt1ICBM=getReleaseFlux(ICBM_fwd) 
  
  #Cumulative Outputs of C
  Rc1ICBM=getAccumulatedRelease(ICBM_fwd)

  
  if(plot_figures==TRUE){
    #plot the pools
    matplot(ts_fwd_ICBM, CICBM_fwd, type="l", lty=1, col=1:2,
            xlab="Time (years)", ylab="C stocks (MgC/ha)",main="ICBM forward")
    legend("topleft", c("Young", "Old"),
           lty=1, col=1:2, bty="n")
  }
  
  
  return(list(Rt1ICBM,CICBM_fwd))
}
