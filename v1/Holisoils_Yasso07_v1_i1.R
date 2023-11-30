###################################################
#Yasso07 model fixed
###################################################
#This function launches the SoilR version of Yasso07, fixed to take into account the actual partitioning of initial C input into AWEN pools
#First, a spin-up run is performed
#Then, the estimated C pool fractions are used to rescale total SOC stocks with measured SOC stocks
#Finally, a forward run is perfomed
#The function provides SOC stock and CO2 fluxes for the chosen simulation lenght


#Yasso07
#' Tuomi, M., Thum, T., Jarvinen, H., Fronzek, S., Berg, B.,
#' Harmon, M., Trofymow, J., Sevanto, S., and Liski, J. (2009). Leaf litter
#' decomposition-estimates of global variability based on Yasso07 model.
#' Ecological Modelling, 220:3362 - 3371.
#' 


Call_Yasso07_i1<-function(plot_figures,
                       temperature_spinup, precipitation_spinup,
                       temperature_fwd, precipitation_fwd,
                     woodylittersize,
                     AWEN_in,
                     #simulation_length, #add for temperature function (from Tuomi et al,2009)
                     SOC_0,C_input_ag_spinup,C_input_bg_spinup,C_input_ag_fwd,C_input_bg_fwd,
                     decomposition_param_Yasso07,
                     t_spinup,t_fwd,
                     spinupcheck, thresholdspin){
  
  #Convert variables in Yasso07
  #InYasso07_spinup = C_input_spinup #MgC/yr
  InYasso07_spinup = C_input_ag_spinup + C_input_bg_spinup #MgC/yr
  ts_spinup_Yasso07 = t_spinup      #yr
  
  #InYasso07_fwd = C_input_fwd       #MgC/yr
  InYasso07_fwd = C_input_ag_fwd + C_input_bg_fwd  #MgC/yr
  ts_fwd_Yasso07 = t_fwd            #yr
  

  temperature_spinup_Yasso07 = temperature_spinup
  temperature_fwd_Yasso07 = temperature_fwd
  
  #Convert daily precipitation to annual precipitation
  precipitation_spinup_Yasso07 <- aggregate(precipitation_spinup["Precip"], list(format(precipitation_spinup$Date,"%Y-%m")),FUN=sum, na.rm = TRUE)
  precipitation_fwd_Yasso07 <- aggregate(precipitation_fwd["Precip"], list(format(precipitation_fwd$Date,"%Y")),FUN=sum, na.rm = TRUE)
  
  #--
  #Rate modifiers spinup
  #Calculate temperature yearly amplitude (Tmax-Tmin)/2
  Temp_max_yr_spinup <- aggregate(temperature_spinup_Yasso07["Temp"], list(format(temperature_spinup_Yasso07$Date,"%Y")),FUN=max, na.rm = TRUE)
  Temp_min_yr_spinup <- aggregate(temperature_spinup_Yasso07["Temp"], list(format(temperature_spinup_Yasso07$Date,"%Y")),FUN=min, na.rm = TRUE)
  
  temp_ampli_spinup <- (Temp_max_yr_spinup$Temp-Temp_min_yr_spinup$Temp)/2
  
  #Create annual dataframe for mean temperature and temperature amplitude
  temperature_annual_spinup <- aggregate(temperature_spinup_Yasso07["Temp"], list(format(temperature_spinup_Yasso07$Date,"%Y")),FUN=mean, na.rm = TRUE)
  temperature_annual_spinup$Temp_ampli<-temp_ampli_spinup

  #Rate modifiers forward
  #Calculate temperature yearly amplitude (Tmax-Tmin)/2
  Temp_max_yr_fwd <- aggregate(temperature_fwd_Yasso07["Temp"], list(format(temperature_fwd_Yasso07$Date,"%Y")),FUN=max, na.rm = TRUE)
  Temp_min_yr_fwd <- aggregate(temperature_fwd_Yasso07["Temp"], list(format(temperature_fwd_Yasso07$Date,"%Y")),FUN=min, na.rm = TRUE)
  
  temp_ampli_fwd <- (Temp_max_yr_fwd$Temp-Temp_min_yr_fwd$Temp)/2
  
  #Create annual dataframe for mean temperature and temperature amplitude
  temperature_annual_fwd <- aggregate(temperature_fwd_Yasso07["Temp"], list(format(temperature_fwd_Yasso07$Date,"%Y")),FUN=mean, na.rm = TRUE)
  #print("Temp yasso07")
  #print(temperature_annual_fwd)
  temperature_annual_fwd$Temp_ampli<-temp_ampli_fwd
  
  #--
 
  beta1_07<-as.numeric(decomposition_param_Yasso07["beta1"])
  beta2_07<-as.numeric(decomposition_param_Yasso07["beta2"])
  gamma_07<-as.numeric(decomposition_param_Yasso07["gamma"])
  delta1_07<-as.numeric(decomposition_param_Yasso07["delta1"])
  delta2_07<-as.numeric(decomposition_param_Yasso07["delta2"])
  r_07<-as.numeric(decomposition_param_Yasso07["r"])

  #Rate modifier spinup
  #Rate modifier forward
  #####
  #1)## Initialization
  #####
  #Read Yasso07 parameters for spinup
  p_spinup=as.numeric(decomposition_param_Yasso07[c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10","p11","p12","pH")])

  #Read decomposition rates
  decomp_rates = as.numeric(decomposition_param_Yasso07[c("kA","kW","kE","kN","kH")])

  #Build dataframe of litter input for spinup
  InYasso07_spinup=data.frame(year=ts_spinup_Yasso07,Litter=rep(InYasso07_spinup,length(ts_spinup_Yasso07)))
  
  # print("*********************")
  # print("*********************")
  # print("BEFORE SPINUP YASSO")
  # print("*********************")
  # print("*********************")

  
  #Run spinup Yass07
  Yasso07_spinup=Yasso07Modelfi(t=ts_spinup_Yasso07, #years
                 #decomposition rates
                 ksY = decomp_rates, 
                 #transfers and feedbacks
                 pY = p_spinup,
                 # climate dependence parameters
                 beta1 = beta1_07, 
                 beta2 = beta2_07, 
                 gamma = gamma_07, 
                 # Woody litter size dependence parameters
                 delta1 = delta1_07, 
                 delta2 = delta2_07, 
                 r =  r_07,
                 C0=rep(0,5), #initial C , 5 element vector, e.g. c(0,0,0,0,0) 
                 In=InYasso07_spinup, # litter C input, data.frame(years, litter), same length as years, if AWEN faractionatoin not provided it has to be in 5 element form for each time step
                 AWEN=AWEN_in, #5 element vector, fractionation of plant C litter input to yasso AWEN pools
                 #AWEN=c(1/4,1/4,1/4,1/4,0),
                 xi = 0, # x1 != 1 will use climate data
                 # xi = 1  will ignore climate data, no climate effect,
                 MT=rep(mean(temperature_annual_spinup$Temp),length(ts_spinup_Yasso07)),# MeanTemperature
                 TA=rep(mean(temperature_annual_spinup$Temp_ampli),length(ts_spinup_Yasso07)), # TemperatureAmplitude = (mothly temp. range)/2
                 PR_mm=rep(mean(precipitation_spinup_Yasso07$Precip),length(ts_spinup_Yasso07)), # Precipitation_mm
                 WS=woodylittersize, # woody size, 0 no effect  for nonwoody, 2 finewoody, 20 coarse woody
                 solver = deSolve.lsoda.wrapper, 
                 pass = FALSE)
  
  
  #Get SOC spinup
  CYasso07_spinup=getC(Yasso07_spinup)
  

  #Check that steady state is reached
  INIZ=CYasso07_spinup[nrow(CYasso07_spinup)-spinupcheck,] #initialize SOC pool values
  for(row in 1:nrow(tail(CYasso07_spinup,spinupcheck))){ 
    pools_i = tail(CYasso07_spinup,spinupcheck)[row,]
    deltai = (INIZ-pools_i)/INIZ
    
    if(all(deltai>thresholdspin)){ #Check that SOC stock variation of each pool is <thresholdspin for all years
      stop("spinup length should be increased, current annual SOC variation is ")
    }
    INIZ=pools_i
  }
  
  #Plot the spinup run
  if(plot_figures==TRUE){
    legend_Yasso07<-c("A", "W", "E", "N", "H")
    #plot the pools
    matplot(ts_spinup_Yasso07, CYasso07_spinup, type="l", lty=1, col=1:5,
            xlab="Time (years)", ylab="C stocks (MgC/ha)",main="Yasso07 spinup")
    legend("topleft", legend_Yasso07,
           lty=1, col=1:5, bty="n")
  }
  
  
  #####
  #2)## Relaxation (rescale total measured C in the different pools at the beginning of the forward simulations)
  #####
  #C after spinup in each pool
  CYasso07_pools_spinup = tail(CYasso07_spinup,1)
  #Total C after spinup
  CYasso07_ACTtot_spinup = sum(CYasso07_pools_spinup)
  
  #Proportion of C in each active pool, relative to total C
  prop_ACTpools_CYasso07 = CYasso07_pools_spinup/CYasso07_ACTtot_spinup
  
  #Initial C in each pool after relaxation
  CYasso07_ACTpools_relax = SOC_0*prop_ACTpools_CYasso07
  
  #####
  #3)## Forward
  #####
  #Define forward parameters
  p_fwd=p_spinup
  
  #Build dataframe of litter input for forward run
  #Build dataframe of litter input for spinup
  if(length(InYasso07_fwd)==1){
    InYasso07=data.frame(year=ts_fwd_Yasso07,Litter=rep(InYasso07_fwd,length(ts_fwd_Yasso07)))
  }else if(length(InYasso07_fwd)==length(ts_fwd_Yasso07)){
    InYasso07=data.frame(year=ts_fwd_Yasso07,Litter=InYasso07_fwd)
  }else(
    print(paste0("The length of the C input data should be 1 \
                 or equal to",length(InYasso07_fwd)," (i.e., the length of the simulations)"))
  ) 
 
  #Run Yasso07 forward
  Yasso07_fwd=Yasso07Modelfi(t=ts_fwd_Yasso07, #years
                                #decomposition rates
                                ksY = decomp_rates, 
                                #transfers and feedbacks
                                pY = p_fwd,
                                # climate dependence parameters
                                beta1 = beta1_07, 
                                beta2 = beta2_07, 
                                gamma = gamma_07, 
                                # Woody litter size dependence parameters
                                delta1 = delta1_07, 
                                delta2 = delta2_07, 
                                r =  r_07,
                                C0=as.numeric(CYasso07_ACTpools_relax), #initial C , 5 element vector, e.g. c(0,0,0,0,0) 
                                In=InYasso07, # litter C input, data.frame(years, litter), same length as years, if AWEN faractionatoin not provided it has to be in 5 element form for each time step
                                AWEN=AWEN_in, #5 element vector, fractionation of plant C litter input to yasso AWEN pools
                                #AWEN=c(1/4,1/4,1/4,1/4,0),
                                xi = 0, # x1 != 1 will use climate data
                                # xi = 1  will ignore climate data, no climate effect,
                                MT=temperature_annual_fwd$Temp,# MeanTemperature
                                TA=temperature_annual_fwd$Temp_ampli, # TemperatureAmplitude = (mothly temp. range)/2
                                PR_mm=precipitation_fwd_Yasso07$Precip, # Precipitation_mm
                                WS=woodylittersize, # woody size, 0 no effect  for nonwoody, 2 finewoody, 20 coarse woody
                                solver = deSolve.lsoda.wrapper, 
                                pass = FALSE)
  
  
  #Get SOC fwd
  CYasso07_fwd=getC(Yasso07_fwd)
  
  #Plot the forward run
  if(plot_figures==TRUE){
    #plot the pools
    matplot(ts_fwd_Yasso07, CYasso07_fwd, type="l", lty=1, col=1:5,
            xlab="Time (years)", ylab="C stocks (MgC/ha)",main="Yasso07 forward")
    legend("topleft", legend_Yasso07,
           lty=1, col=1:5, bty="n")
  }
  
  #Outputs of C
  Rt1Yasso07=getReleaseFlux(Yasso07_fwd) 
  
  #Cumulative Outputs of C
  Rc1Yasso07=getAccumulatedRelease(Yasso07_fwd)
  
  return(list(Rt1Yasso07,CYasso07_fwd))
}
