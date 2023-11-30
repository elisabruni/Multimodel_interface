###################################################
#Yasso20 model
###################################################
#This function launches the SoilR version of Yasso20 based on Viskari et al. 2022
#First, a spin-up run is performed
#Then, the estimated C pool fractions are used to rescale total SOC stocks with measured SOC stocks
#Finally, a forward run is perfomed
#The function provides SOC stock and CO2 fluxes for the chosen simulation lenght


#Yasso20
#' Toni Viskari, Janne Pusa, Istem Fer, Anna Repo, Julius Vira, and Jari Liski (2022). 
#' Calibrating the soil organic carbon model Yasso20 with multiple datasets.
#' Geoscientific model development, 1735:1752 - 15.
#' 
#https://doi.org/10.5194/gmd-15-1735-2022

Call_Yasso20_i1<-function(plot_figures,
                       temperature_spinup, precipitation_spinup,
                       temperature_fwd, precipitation_fwd,
                       woodylittersize,
                       AWEN_in,
                       #simulation_length, #add for temperature function (from Tuomi et al,2009)
                       SOC_0,C_input_ag_spinup,C_input_bg_spinup,C_input_ag_fwd,C_input_bg_fwd,
                       decomposition_param_Yasso20,
                       t_spinup,t_fwd,
                       spinupcheck, thresholdspin){
  
  #Convert variables in Yasso20
  #InYasso20_spinup = C_input_spinup #MgC/yr
  InYasso20_spinup = C_input_ag_spinup+C_input_bg_spinup #MgC/yr
  ts_spinup_Yasso20 = t_spinup      #yr
  
  #InYasso20_fwd = C_input_fwd       #MgC/yr
  InYasso20_fwd = C_input_ag_fwd+C_input_bg_fwd #MgC/yr
  ts_fwd_Yasso20 = t_fwd            #yr
  
  
  temperature_spinup_Yasso20 = temperature_spinup
  temperature_fwd_Yasso20 = temperature_fwd
  
  #Convert daily precipitation to annual precipitation
  precipitation_spinup_Yasso20 <- aggregate(precipitation_spinup["Precip"], list(format(precipitation_spinup$Date,"%Y-%m")),FUN=sum, na.rm = TRUE)
  precipitation_fwd_Yasso20 <- aggregate(precipitation_fwd["Precip"], list(format(precipitation_fwd$Date,"%Y")),FUN=sum, na.rm = TRUE)
  
  #--
  #Rate modifiers spinup
  #Calculate temperature monthly 
  temperature_spinup_Yasso20 <- aggregate(temperature_spinup_Yasso20, list(format(temperature_spinup_Yasso20$Date,"%Y-%m")),FUN=mean, na.rm = TRUE)
  temperature_fwd_Yasso20 <- aggregate(temperature_fwd_Yasso20, list(format(temperature_fwd_Yasso20$Date,"%Y-%m")),FUN=mean, na.rm = TRUE)
  
  #Create temperature database with monthly temperature split in years
  temperature_spinup_Yasso20split = 
    do.call(rbind,split(temperature_spinup_Yasso20$Temp,format(temperature_spinup_Yasso20$Date,format="%Y")))
  
  temperature_fwd_Yasso20split = 
    do.call(rbind,split(temperature_fwd_Yasso20$Temp,format(temperature_fwd_Yasso20$Date,format="%Y")))
  #--
  
  beta1_20<-c(decomposition_param_Yasso20["beta1AWE"],decomposition_param_Yasso20["beta1N"],decomposition_param_Yasso20["beta1H"])
  beta2_20<-c(decomposition_param_Yasso20["beta2AWE"],decomposition_param_Yasso20["beta2N"],decomposition_param_Yasso20["beta2H"])
  gamma_20<-c(decomposition_param_Yasso20["g"],decomposition_param_Yasso20["gN"],decomposition_param_Yasso20["gH"])
  delta1_20<-as.numeric(decomposition_param_Yasso20["delta1"])
  delta2_20<-as.numeric(decomposition_param_Yasso20["delta2"])
  r_20<-as.numeric(decomposition_param_Yasso20["r"])
  
  #Rate modifier spinup
  #Rate modifier forward
  #####
  #1)## Initialization
  #####
  #Read Yasso20 parameters for spinup
  p_spinup=as.numeric(decomposition_param_Yasso20[c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","pH")])
  
  #Read decomposition rates
  decomp_rates = as.numeric(decomposition_param_Yasso20[c("kA","kW","kE","kN","kH")])

  
  #Build dataframe of litter input for spinup
  InYasso20_spinup=data.frame(year=ts_spinup_Yasso20,Litter=rep(InYasso20_spinup,length(ts_spinup_Yasso20)))
  
 
  #Build dataframe of temperature for spinup
  # print("^^^^^^^^^^^^^^^^^^^^")
  # print("^^^^^^^^^^^^^^^^^^^^")
  # print("temperature_spinup_Yasso20")
  # print(temperature_spinup_Yasso20)
  # print("^^^^^^^^^^^^^^^^^^^^")
  # print("temperature_spinup_Yasso20split")
  # print(temperature_spinup_Yasso20split)
  # print("^^^^^^^^^^^^^^^^^^^^")
  mean_monthly_temp_spinup = colMeans(temperature_spinup_Yasso20split)

  mean_monthly_temp_spinup_split = matrix(NaN,length(ts_spinup_Yasso20),12)
  for(i in 1:length(ts_spinup_Yasso20)){
    mean_monthly_temp_spinup_split[i,]=mean_monthly_temp_spinup
  }
  
  #Run spinup Yass20
  Yasso20_spinup=Yasso20Modelfi(t=ts_spinup_Yasso20, #years
                                #decomposition rates
                                ksY = decomp_rates, 
                                #transfers and feedbacks
                                pY = p_spinup,
                                # climate dependence parameters
                                beta1 = beta1_20, 
                                beta2 = beta2_20, 
                                gamma = gamma_20, 
                                # Woody litter size dependence parameters
                                delta1 = delta1_20, 
                                delta2 = delta2_20, 
                                r =  r_20,
                                C0=rep(0,5), #initial C , 5 element vector, e.g. c(0,0,0,0,0) 
                                In=InYasso20_spinup, # litter C input, data.frame(years, litter), same length as years, if AWEN faractionatoin not provided it has to be in 5 element form for each time step
                                AWEN=AWEN_in, #5 element vector, fractionation of plant C litter input to yasso AWEN pools
                                #AWEN=c(1/4,1/4,1/4,1/4,0),
                                xi = 0, # x1 != 1 will use climate data
                                # xi = 1  will ignore climate data, no climate effect,
                                MTm=mean_monthly_temp_spinup_split,# MeanTemperature
                                PR_mm=rep(mean(precipitation_spinup_Yasso20$Precip),length(ts_spinup_Yasso20)), # Precipitation_mm
                                WS=woodylittersize, # woody size, 0 no effect  for nonwoody, 2 finewoody, 20 coarse woody
                                solver = deSolve.lsoda.wrapper, 
                                pass = FALSE)
  
  
  #Get SOC spinup
  CYasso20_spinup=getC(Yasso20_spinup)
  

  
  #Check that steady state is reached
  INIZ=CYasso20_spinup[nrow(CYasso20_spinup)-spinupcheck,] #initialize SOC pool values
  for(row in 1:nrow(tail(CYasso20_spinup,spinupcheck))){ 
    pools_i = tail(CYasso20_spinup,spinupcheck)[row,]
    deltai = (INIZ-pools_i)/INIZ
    
    if(all(deltai>thresholdspin)){ #Check that SOC stock variation of each pool is <thresholdspin for all years
      stop("spinup length should be increased, current annual SOC variation is ")
    }
    INIZ=pools_i
  }
  
  #Plot the spinup run
  if(plot_figures==TRUE){
    legend_Yasso20<-c("A", "W", "E", "N", "H")
    #plot the pools
    matplot(ts_spinup_Yasso20, CYasso20_spinup, type="l", lty=1, col=1:5,
            xlab="Time (years)", ylab="C stocks (MgC/ha)",main="Yasso20 spinup")
    legend("topleft", legend_Yasso20,
           lty=1, col=1:5, bty="n")
  }
  
  
  #####
  #2)## Relaxation (rescale total measured C in the different pools at the beginning of the forward simulations)
  #####
  #C after spinup in each pool
  CYasso20_pools_spinup = tail(CYasso20_spinup,1)
  #Total C after spinup
  CYasso20_ACTtot_spinup = sum(CYasso20_pools_spinup)
  
  #Proportion of C in each active pool, relative to total C
  prop_ACTpools_CYasso20 = CYasso20_pools_spinup/CYasso20_ACTtot_spinup
  
  #Initial C in each pool after relaxation
  CYasso20_ACTpools_relax = SOC_0*prop_ACTpools_CYasso20
  
  #####
  #3)## Forward
  #####
  #Define forward parameters
  p_fwd=p_spinup
  
  #Build dataframe of litter input for forward run
  InYasso20=data.frame(year=ts_fwd_Yasso20,Litter=rep(InYasso20_fwd,length(ts_fwd_Yasso20)))

  #Build dataframe of litter input for spinup
  if(length(InYasso20_fwd)==1){
    InYasso20=data.frame(year=ts_fwd_Yasso20,Litter=rep(InYasso20_fwd,length(ts_fwd_Yasso20)))
  }else if(length(InYasso20_fwd)==length(ts_fwd_Yasso20)){
    InYasso20=data.frame(year=ts_fwd_Yasso20,Litter=InYasso20_fwd)
  }else(
    print(paste0("The length of the C input data should be 1 \
                 or equal to",length(InYasso20_fwd)," (i.e., the length of the simulations)"))
  )
  
  #Build dataframe of temperature for forward
  # mean_monthly_temp_fwd = colMeans(temperature_fwd_Yasso20split)
  # mean_monthly_temp_fwd_split = matrix(NaN,length(ts_fwd_Yasso20),12)
  # for(i in 1:length(ts_fwd_Yasso20)){
  #   mean_monthly_temp_fwd_split[i,]=mean_monthly_temp_fwd
  # }
  
  # print("temperature_fwd_Yasso20split")
  # print(temperature_fwd_Yasso20split)

  #Run Yasso20 forward
  Yasso20_fwd=Yasso20Modelfi(t=ts_fwd_Yasso20, #years
                             #decomposition rates
                             ksY = decomp_rates, 
                             #transfers and feedbacks
                             pY = p_fwd,
                             # climate dependence parameters
                             beta1 = beta1_20, 
                             beta2 = beta2_20, 
                             gamma = gamma_20, 
                             # Woody litter size dependence parameters
                             delta1 = delta1_20, 
                             delta2 = delta2_20, 
                             r =  r_20,
                             C0=as.numeric(CYasso20_ACTpools_relax), #initial C , 5 element vector, e.g. c(0,0,0,0,0) 
                             In=InYasso20, # litter C input, data.frame(years, litter), same length as years, if AWEN faractionatoin not provided it has to be in 5 element form for each time step
                             AWEN=AWEN_in, #5 element vector, fractionation of plant C litter input to yasso AWEN pools
                             #AWEN=c(1/4,1/4,1/4,1/4,0),
                             xi = 0, # x1 != 1 will use climate data
                             # xi = 1  will ignore climate data, no climate effect,
                             MTm=temperature_fwd_Yasso20split,# MeanTemperature
                             PR_mm=precipitation_fwd_Yasso20$Precip, # Precipitation_mm
                             WS=woodylittersize, # woody size, 0 no effect  for nonwoody, 2 finewoody, 20 coarse woody
                             solver = deSolve.lsoda.wrapper, 
                             pass = FALSE)
  
  
  #Get SOC fwd
  CYasso20_fwd=getC(Yasso20_fwd)
  
  #Plot the forward run
  if(plot_figures==TRUE){
    #plot the pools
    matplot(ts_fwd_Yasso20, CYasso20_fwd, type="l", lty=1, col=1:5,
            xlab="Time (years)", ylab="C stocks (MgC/ha)",main="Yasso20 forward")
    legend("topleft", legend_Yasso20,
           lty=1, col=1:5, bty="n")
  }
  
  #Outputs of C
  Rt1Yasso20=getReleaseFlux(Yasso20_fwd) 
  
  #Cumulative Outputs of C
  Rc1Yasso20=getAccumulatedRelease(Yasso20_fwd)
  
  return(list(Rt1Yasso20,CYasso20_fwd))
}
