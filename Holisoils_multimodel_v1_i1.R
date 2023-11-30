###################################################
#Holisoils Multimodel ensemble version 1
###################################################
#This function launches an ensemble of models 
#that are either coded in SoilR, or sourced from .R files that must come with this script
#The models currently included in the ensemble are:

# Century
#' Parton, W.J, D.S. Schimel, C.V. Cole, and D.S. Ojima. 1987.
#' Analysis of factors controlling soil organic matter levels in Great Plain
#' grasslands. Soil Science Society of America Journal 51: 1173--1179.

#RothC
#' Jenkinson, D. S., S. P. S. Andrew, J. M. Lynch, M. J. Goss, and
#' P. B. Tinker. 1990. The Turnover of Organic Carbon and Nitrogen in Soil.
#' Philosophical Transactions: Biological Sciences 329:361-368.

#ICBM 
#' Andren, O. and T. Katterer. 1997. ICBM: The Introductory Carbon
#' Balance Model for Exploration of Soil Carbon Balances. Ecological
#' Applications 7:1226-1236.

#Yasso07
#' Tuomi, M., Thum, T., Jarvinen, H., Fronzek, S., Berg, B.,
#' Harmon, M., Trofymow, J., Sevanto, S., and Liski, J. (2009). Leaf litter
#' decomposition-estimates of global variability based on Yasso07 model.
#' Ecological Modelling, 220:3362 - 3371.
#' 

#Millennial
#' Abramoff, Rose Z., Bertrand Guenet, Haicheng Zhang, Katerina Georgiou, Xiaofeng Xu, 
#' Raphael A. Viscarra Rossel, Wenping Yuan, and Philippe Ciais. “Improved Global-Scale Predictions 
#' of Soil Carbon Stocks with Millennial Version 2.” Soil Biology and Biochemistry 164 (January 2022):
#'  108466. https://doi.org/10.1016/j.soilbio.2021.108466.


#Yasso20
#' Toni Viskari, Janne Pusa, Istem Fer, Anna Repo, Julius Vira, and Jari Liski (2022). 
#' Calibrating the soil organic carbon model Yasso20 with multiple datasets.
#' Geoscientific model development, 1735:1752 - 15.
#' https://doi.org/10.5194/gmd-15-1735-2022

#Other references
#' Sierra, C.A., M. Mueller, S.E. Trumbore. 2012. Models of soil organic matter
#' decomposition: the SoilR package version 1.0. Geoscientific Model
#' Development 5, 1045-1060.


########################################################################
#PREDIFINED INPUT DATA & UNITS
########################################################################

#If not specified otherwise, all soil variables should refer to the the top 0-30 cm
#         --Plot--
#'@param plot_figures a boolean (TRUE/FALSE) deciding wether to plot the default figures or not
#         --Time--
#'@param simulation_length               A scalar indicating the forward simulation lenght: [years]
#'@param spinup_length                   A scalar indicating the lenght of the spinup run:  [years]

#'@param start_date_simulations          A `Date` object in the format "YYYY-MM-DD"" [e.g. as.Date("2021-01-01")] at which the experiment starts

#         --Meteo
#'@param temperature                     A `data.frame` object that has as first column the dates of measurments in the format "YYYY-MM" and as second column the monthly average temperatures [˚C]
#'@param precipitation                   A `data.frame` object that has as first column the dates of measurments in the format "YYYY-MM" and as second column the monthly cumulative precipitations [mm]
#'@param potential_evapotranspiration    A `data.frame` object that has as first column the dates of measurments in the format "YYYY-MM" and as second column the monthly potentia evapotranspiration [mm/month]


#         --Soil--
#'@param SOC_0                           A scalar indicating the level of SOC stock at the beginning of the simulation                                             [Mg/ha]
#'@param C_input_spinup                  A scalar indicating the level of average annual C input during the spinup run                                             [Mg/ha/yr]
#'@param C_input_fwd                     A vector (or scalar if information is not available) indicating the level of annual C input during the forward simulation [Mg/ha/yr]
#'@param clay_p                          A scalar indicating the percentage concentration of clay       [%]
#'@param silt_p                          A scalar indicating the percentage concentration of silt       [%]
#'@param soil_thickness                  A scalar indicating the thikness of the organic layer topsoil  [cm] (default is 25)

#         --Litter--
#'@param lignin_to_nitrogen              A scalar indicating the Lignin:Nitrogen ratio of the litter input          [unitless] (default is 0.5) 
#'@param structural_in_lignin            A scalar indicating the fraction of structural material in the lignin  [unitless] (default is 0.1) 
#'@param woodylittersize                 A scalar indicating the size of the woody litter input [cm] (set to 0 for nonwoody, 2 for finewoody, and 20 for coarse woody)
#'@param AWEN_in                         A vector of 5 scalars indicating the fraction of AWEN pools in Yasso07 and Yasso20 [unitless]

#'@param CN_Ratio                        A scalar indicating the Carbon:Nitrogen ratio of the soil (@5cm depth)     [unitless]
#'@param WFPS                            A scalar indicating the water filled pore space (@5cm depth)               [unitless] (if not available, set WFPS to <0. It will be calculated using Bulk_Density and vswc) 
#'@param vswc                            A scalar indicating the volumetric soil water content (@5cm depth)         [%] (this is needed only if WFPS is not available) 
#'@param CH4_Conc                        A scalar indicating the atmospheric CH4 concentration                      [ppb] (if not available, a default value from Japan will be used)


#       --Decomposition rate parameters
#'@param decomposition_param_RothC       A vector of 5 elements providing the decomposition rate parameters of the RothC C pools:  [1/yr] (default is: c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66, k.HUM = 0.02, k.IOM = 0))
#'@param decomposition_param_ICBM        A vector of 3 elements providing: the 2 decomposition rate parameters of the ICBM C pools [1/yr] and the humification coefficient [unitless] (default is: c(k1=0.8,k2=0.00605,h=0.13))
#'@param decomposition_param_Century     A vector of 7 elements providing the decomposition rate parameters of the Century C pools [1/month] (default is c(STR.surface = 1/(12*0.245), MET.surface = 1/(12*0.066), STR.belowground = 1/(12*0.245), MET.belowground = 1/(12*0.066), ACT = 1/(12*0.149), SLW = 1/(12*5.480), PAS = 1/(12*241)))
#'@param decomposition_param_Yasso07     A vector of 24 elements providing: the decomposition rate parameters of the Yasso07 C pools [1/yr] (default is kA = 0.73, kW = 5.8, kE = 0.29, kN = 0.031,kH = 0.0017); the environmental function's parameters, and the proportion of fluxes among pools



Call_MULTIMODEL_i1<-function(plot_figures,simulation_length, spinup_length,
                             computation_time_step_fwd,
                          start_date_simulations,
                          temperature_spinup, precipitation_spinup, potential_evapotranspiration_spinup, soilmoisture_spinup,
                          temperature_fwd, precipitation_fwd, potential_evapotranspiration_fwd, soilmoisture_fwd,
                          SOC_0,C_input_ag_spinup,C_input_bg_spinup,C_input_ag_fwd,C_input_bg_fwd,
                          clay_p,silt_p, soil_thickness, pH_p,
                          lignin_to_nitrogen,structural_in_lignin,woodylittersize,AWEN_in,decomp_to_resist_ratio,
                          CN_Ratio, Bulk_Density, WFPS, CH4_Conc,
                          decomposition_param_RothC,
                          decomposition_param_ICBM,
                          decomposition_param_Century,
                          decomposition_param_Yasso07,
#                          decomposition_param_Millennial2,
                          decomposition_param_Yasso20){
  
  
  spinupcheck=10 #number of years for which the steady state is sought
  thresholdspin=0.001 #SOC variation for which the pool is considered at steady state
  
  #---------------------------------------------
  #CREATE INTEGRALS TIME STEPS
  #---------------------------------------------
  #Define time step spinup run [in years]
  computation_time_step_spinup = 1
  #Define time step forward run [in years]
  #computation_time_step_fwd = 1/12
  
  
  #Create a vector of points in time where the spinup run solution is sought
  t_spinup = seq(1,spinup_length,by=computation_time_step_spinup)
  #Create a vector of points in time where the forward solution is sought
  t_fwd = seq(1,simulation_length,by=computation_time_step_fwd)
  
  # #Create a vector of points in time where the spinup run solution is sought
  # t_spinup = seq(computation_time_step_spinup,spinup_length,by=computation_time_step_spinup)
  # #Create a vector of points in time where the forward solution is sought
  # t_fwd = seq(computation_time_step_fwd,simulation_length,by=computation_time_step_fwd)
  
  #CALL ROTHC
  Roth_C_fwd<-Call_RothC_i1(plot_figures=plot_figures,
                         temperature_spinup, precipitation_spinup, potential_evapotranspiration_spinup,
                         temperature_fwd, precipitation_fwd, potential_evapotranspiration_fwd,
                         SOC_0=SOC_0,C_input_ag_spinup=C_input_ag_spinup,C_input_bg_spinup=C_input_bg_spinup,
                         C_input_ag_fwd=C_input_ag_fwd,C_input_bg_fwd=C_input_bg_fwd,
                         clay_p=clay_p,soil_thickness=soil_thickness,decomp_to_resist_ratio=decomp_to_resist_ratio,
                         decomposition_param_RothC=decomposition_param_RothC,t_spinup=t_spinup,t_fwd=t_fwd,
                         spinupcheck=spinupcheck, thresholdspin=thresholdspin)
  print("RothC ok")
  print(head(Roth_C_fwd))
  
  #CALL ICBM
  ICBM_C_fwd<-Call_ICBM_i1(plot_figures=plot_figures,
                           temperature_spinup, soilmoisture_spinup,
                           temperature_fwd, soilmoisture_fwd,
                           SOC_0=SOC_0,C_input_ag_spinup=C_input_ag_spinup,C_input_bg_spinup=C_input_bg_spinup,
                           C_input_ag_fwd=C_input_ag_fwd,C_input_bg_fwd=C_input_bg_fwd,
                           clay_p=clay_p,
                           #soil_thickness=soil_thickness,
                           decomposition_param_ICBM=decomposition_param_ICBM,t_spinup=t_spinup,t_fwd=t_fwd,
                           spinupcheck=spinupcheck, thresholdspin=thresholdspin)
  print("ICBM ok")
  print(head(ICBM_C_fwd))
  
  #CALL CENTURY
  Century_C_fwd<-Call_Century_i1(plot_figures=plot_figures,
                              temperature_spinup, precipitation_spinup, potential_evapotranspiration_spinup,
                              temperature_fwd, precipitation_fwd, potential_evapotranspiration_fwd,
                              SOC_0=SOC_0,C_input_ag_spinup=C_input_ag_spinup,C_input_bg_spinup=C_input_bg_spinup,
                              C_input_ag_fwd=C_input_ag_fwd,C_input_bg_fwd=C_input_bg_fwd,
                              clay_p=clay_p,silt_p=silt_p,
                              lignin_to_nitrogen=lignin_to_nitrogen, structural_in_lignin=structural_in_lignin,
                              decomposition_param_Century=decomposition_param_Century,
                              t_spinup=t_spinup,t_fwd=t_fwd,
                              spinupcheck=spinupcheck, thresholdspin=thresholdspin)
  print("Century ok")
  print(head(Century_C_fwd))
  
  #CALL YASSO07
  Yasso07_C_fwd<-Call_Yasso07_i1(plot_figures=plot_figures,
                              temperature_spinup, precipitation_spinup,
                              temperature_fwd, precipitation_fwd,
                              woodylittersize=woodylittersize,AWEN_in=AWEN_in,
                              #simulation_length=simulation_length, #remove once Yasso function for temp and moist implemented
                              SOC_0=SOC_0,C_input_ag_spinup=C_input_ag_spinup,C_input_bg_spinup=C_input_bg_spinup,
                              C_input_ag_fwd=C_input_ag_fwd,C_input_bg_fwd=C_input_bg_fwd,
                              decomposition_param_Yasso07=decomposition_param_Yasso07,
                              t_spinup=t_spinup,t_fwd=t_fwd, 
                              spinupcheck=spinupcheck, thresholdspin=thresholdspin)
  print("Yasso07 ok")
  print(head(Yasso07_C_fwd))
  
  #CALL YASSO20
  Yasso20_C_fwd<-Call_Yasso20_i1(plot_figures=plot_figures,
                              temperature_spinup, precipitation_spinup,
                              temperature_fwd, precipitation_fwd,
                              woodylittersize=woodylittersize,AWEN_in=AWEN_in,
                              #simulation_length=simulation_length, #remove once Yasso function for temp and moist implemented
                              SOC_0=SOC_0,C_input_ag_spinup=C_input_ag_spinup,C_input_bg_spinup=C_input_bg_spinup,
                              C_input_ag_fwd=C_input_ag_fwd,C_input_bg_fwd=C_input_bg_fwd,
                              decomposition_param_Yasso20=decomposition_param_Yasso20,
                              t_spinup=t_spinup,t_fwd=t_fwd, 
                              spinupcheck=spinupcheck, thresholdspin=thresholdspin)
  print("Yasso20 ok")
  print(head(Yasso20_C_fwd))
  
  # Millennial2_C_fwd<-Call_Millennial2_i1(plot_figures=plot_figures,
  #                                     temperature_spinup, soilmoisture_spinup,#mm3/mm3
  #                                     temperature_fwd, soilmoisture_fwd,#mm3/mm3
  #                            SOC_0=SOC_0,#MgC/ha
  #                            C_input_spinup=C_input_spinup,C_input_fwd=C_input_fwd,#MgC/ha/yr
  #                            pH_p=pH_p,
  #                            Bulk_Density=Bulk_Density,#Mg/m3
  #                            soil_thickness=soil_thickness,#cm
  #                            clay_p=clay_p,#%
  #                            silt_p=silt_p,#%
  #                            decomposition_param_Millennial2=decomposition_param_Millennial2,
  #                            t_spinup=t_spinup,t_fwd=t_fwd,
  #                            spinupcheck=spinupcheck, thresholdspin=thresholdspin)
  # print("Millennial2 ok")
  # print(head(Millennial2_C_fwd))
  
  
  #CALL SG
  SG_C_fwd<-Call_SG(CN_Ratio=CN_Ratio, Bulk_Density=Bulk_Density, 
                    WFPS=WFPS, vswc=mean(soilmoisture_fwd)*100, Soil_Temperature=temperature_fwd, CH4_Conc=CH4_Conc)
  
  #Select only sought solutions
  SG_C_fwd<-SG_C_fwd[c(1:length(t_fwd)),]
  SG_C_fwd[,1]=SG_C_fwd[,1]*(1e-12*1e4*3.171e7) #convert CO2: from ugC/m2/sec to tC/ha/year
  SG_C_fwd[,c(2,3)]=SG_C_fwd[,c(2,3)]*(1e-12*1e4*8760) #convert CO2: from ug/m2/hour to tC/ha/year
  
  print("SG ok")
  print(SG_C_fwd)
  
  #Plot SG fluxes
  if(computation_time_step_fwd==1/12){
    time_legend = "month"
  }else if(computation_time_step_fwd==1){
    time_legend = "yr"
  }else if(computation_time_step_fwd==1/365){
    time_legend = "day"
  }else{
    time_legend = "CHECK UNITS"
  }
  if(plot_figures==TRUE){
    legend_SG<-c("CH4", "N20")
    #plot the pools
    matplot(t_fwd, SG_C_fwd[,c(2,3)], type="l", lty=1, col=1:2,
            xlab="Time (years)", ylab=paste0("Fluxes (tC/ha/year)"),main="SG fluxes")
    legend("topleft", legend_SG,
           lty=1, col=1:2, bty="n")
  }
  

  #Calculate total SOC stock
  totC_Roth_C_fwd<-rowSums(Roth_C_fwd[[2]])
  totC_ICBM_C_fwd<-rowSums(ICBM_C_fwd[[2]])
  totC_Century_C_fwd<-rowSums(Century_C_fwd[[2]])
  totC_Yasso07_C_fwd<-rowSums(Yasso07_C_fwd[[2]])
  #totC_Millennial2_C_fwd<-rowSums(Millennial2_C_fwd[[2]])
  totC_Yasso20_C_fwd<-rowSums(Yasso20_C_fwd[[2]])
  
  #Calculate multi-model mean of total SOC stock
  # mmmean_totC <- rowMeans(cbind(totC_Roth_C_fwd,totC_ICBM_C_fwd,
  #                               totC_Century_C_fwd,totC_Yasso07_C_fwd,
  #                               totC_Millennial2_C_fwd,totC_Yasso20_C_fwd))
  # 
  # minC <- min(as.numeric(cbind(totC_Roth_C_fwd,totC_ICBM_C_fwd,
  #                              totC_Century_C_fwd,totC_Yasso07_C_fwd,
  #                              totC_Millennial2_C_fwd,totC_Yasso20_C_fwd)))
  # 
  # maxC <- max(as.numeric(cbind(totC_Roth_C_fwd,totC_ICBM_C_fwd,
  #                              totC_Century_C_fwd,totC_Yasso07_C_fwd,
  #                              totC_Millennial2_C_fwd,totC_Yasso20_C_fwd)))
  
  mmmean_totC <- rowMeans(cbind(totC_Roth_C_fwd,totC_ICBM_C_fwd,
                                totC_Century_C_fwd,totC_Yasso07_C_fwd,totC_Yasso20_C_fwd))
  
  minC <- min(as.numeric(cbind(totC_Roth_C_fwd,totC_ICBM_C_fwd,
                               totC_Century_C_fwd,totC_Yasso07_C_fwd,totC_Yasso20_C_fwd)))
  
  maxC <- max(as.numeric(cbind(totC_Roth_C_fwd,totC_ICBM_C_fwd,
                               totC_Century_C_fwd,totC_Yasso07_C_fwd,totC_Yasso20_C_fwd)))
  
  #Calculate total CO2 fluxes
  totF_Roth_C_fwd<-rowSums(Roth_C_fwd[[1]])
  totF_ICBM_C_fwd<-rowSums(ICBM_C_fwd[[1]])
  totF_Century_C_fwd<-rowSums(Century_C_fwd[[1]])
  totF_Yasso07_C_fwd<-rowSums(Yasso07_C_fwd[[1]])
#  totF_Millennial2_C_fwd<-rowSums(Millennial2_C_fwd[[1]])
  totF_Yasso20_C_fwd<-rowSums(Yasso20_C_fwd[[1]])
  #Add SG
  totF_SG_C_fwd <- SG_C_fwd[,1]

  #Calculate multi-model mean of total CO2 fluxes
  # mmmean_totF <- rowMeans(cbind(totF_Roth_C_fwd,totF_ICBM_C_fwd,
  #                               totF_Century_C_fwd,totF_Yasso07_C_fwd,
  #                               totF_Millennial2_C_fwd,totF_Yasso20_C_fwd))
  # 
  # 
  # minF <- min(as.numeric(cbind(totF_Roth_C_fwd,totF_ICBM_C_fwd,
  #                              totF_Century_C_fwd,totF_Yasso07_C_fwd,
  #                              totF_Millennial2_C_fwd,totF_Yasso20_C_fwd)))
  # 
  # maxF <- max(as.numeric(cbind(totF_Roth_C_fwd,totF_ICBM_C_fwd,
  #                              totF_Century_C_fwd,totF_Yasso07_C_fwd,
  #                              totF_Millennial2_C_fwd,totF_Yasso20_C_fwd)))
  
  mmmean_totF <- rowMeans(cbind(totF_Roth_C_fwd,totF_ICBM_C_fwd,
                                totF_Century_C_fwd,totF_Yasso07_C_fwd,totF_Yasso20_C_fwd,
                                totF_SG_C_fwd))
  
  
  # minF <- min(as.numeric(cbind(totF_Roth_C_fwd,totF_ICBM_C_fwd,
  #                              totF_Century_C_fwd,totF_Yasso07_C_fwd,totF_Yasso20_C_fwd,
  #                              totF_SG_C_fwd)))
  # 
  # maxF <- max(as.numeric(cbind(totF_Roth_C_fwd,totF_ICBM_C_fwd,
  #                              totF_Century_C_fwd,totF_Yasso07_C_fwd,totF_Yasso20_C_fwd,
  #                              totF_SG_C_fwd)))
  
  #PLOT MULTIMODEL
  express_plotC = expression("SOC stocks (MgC"~{ha}^{-1}~")")
  if(plot_figures==TRUE){
    par(mfrow=c(2,1),oma=c(2, 0, 0, 5))
    print("Plotting SOC multimodel")
    #print("length t_fwd")
    #print(length(t_fwd))
    #plot total C stocks
    plot(t_fwd, totC_Roth_C_fwd, type="l", lty=1,xlab="Time (years)",ylab=" ",col="red", ylim=c(minC-5,maxC+5))
    title(ylab=express_plotC,main="Multi-model SOC stocks",mgp=c(2,1,0))
    lines(t_fwd,totC_ICBM_C_fwd,type="l", lty=1, col="black")
    lines(t_fwd,totC_Century_C_fwd,type="l", lty=1, col="green")
    lines(t_fwd,totC_Yasso07_C_fwd,type="l", lty=1, col="pink")
#    lines(t_fwd,totC_Millennial2_C_fwd,type="l", lty=1, col="blue")
    lines(t_fwd,totC_Yasso20_C_fwd,type="l", lty=1, col="hotpink")
    lines(t_fwd,mmmean_totC,type="l", lwd = 3,col="grey")
    legend(par('usr')[2], par('usr')[4], c("RothC", "ICBM","Century","Yasso07","Yasso20","Mean"),
           lty=1,lwd=c(1,1,1,1,1,3), col=c("red","black","green","pink","hotpink","grey"),
           cex=0.8,xpd=NA,bty="n")}
  if(plot_figures==TRUE){
    print("Plotting CO2 multimodel")
    #plot CO2 fluxes
    express_plotF = expression(CO[2]~"flux (MgC"~{ha}^{-1}*{year}^{-1}~")")
    
    totF_Roth_C_fwd_plt<-colMeans(matrix(totF_Roth_C_fwd[1:(length(totF_Roth_C_fwd)-1)],1/computation_time_step_fwd))
    totF_ICBM_C_fwd_plt<-colMeans(matrix(totF_ICBM_C_fwd[1:(length(totF_ICBM_C_fwd)-1)],1/computation_time_step_fwd))
    totF_Century_C_fwd_plt<-colMeans(matrix(totF_Century_C_fwd[1:(length(totF_Century_C_fwd)-1)],1/computation_time_step_fwd))
    totF_Yasso07_C_fwd_plt<-colMeans(matrix(totF_Yasso07_C_fwd[1:(length(totF_Yasso07_C_fwd)-1)],1/computation_time_step_fwd))
#    totF_Millennial2_C_fwd_plt<-colMeans(matrix(totF_Millennial2_C_fwd[1:(length(totF_Millennial2_C_fwd)-1)],1/computation_time_step_fwd))
    totF_Yasso20_C_fwd_plt<-colMeans(matrix(totF_Yasso20_C_fwd[1:(length(totF_Yasso20_C_fwd)-1)],1/computation_time_step_fwd))
    mmmean_totF_plt<-colMeans(matrix(mmmean_totF[1:(length(mmmean_totF)-1)],1/computation_time_step_fwd))
    totF_SG_C_fwd_plt<-totF_SG_C_fwd[1:simulation_length-1]
    t_fwd_plt<-seq(1,simulation_length-1,by=1)
    
    # print(" ")
    # print("Length SG")
    # print(length(totF_SG_C_fwd))
    # print("Length SG plot")
    # print(length(totF_SG_C_fwd_plt))
    # print("Length t")
    # print(length(t_fwd_plt))
    # print(" ")
    
    # totF_Roth_C_fwd_plt<-colMeans(matrix(totF_Roth_C_fwd,1/computation_time_step_fwd))
    # totF_ICBM_C_fwd_plt<-colMeans(matrix(totF_ICBM_C_fwd,1/computation_time_step_fwd))
    # totF_Century_C_fwd_plt<-colMeans(matrix(totF_Century_C_fwd,1/computation_time_step_fwd))
    # totF_Yasso07_C_fwd_plt<-colMeans(matrix(totF_Yasso07_C_fwd,1/computation_time_step_fwd))
    # totF_Millennial2_C_fwd_plt<-colMeans(matrix(totF_Millennial2_C_fwd,1/computation_time_step_fwd))
    # totF_Yasso20_C_fwd_plt<-colMeans(matrix(totF_Yasso20_C_fwd,1/computation_time_step_fwd))
    # mmmean_totF_plt<-colMeans(matrix(mmmean_totF,1/computation_time_step_fwd))
    # t_fwd_plt<-seq(1,simulation_length,by=1)
    
    # minF_plt <- min(as.numeric(cbind(totF_Roth_C_fwd_plt,totF_ICBM_C_fwd_plt,
    #                              totF_Century_C_fwd_plt,totF_Yasso07_C_fwd_plt,
    #                              totF_Millennial2_C_fwd_plt,totF_Yasso20_C_fwd_plt)))
    # 
    # maxF_plt <- max(as.numeric(cbind(totF_Roth_C_fwd_plt,totF_ICBM_C_fwd_plt,
    #                              totF_Century_C_fwd_plt,totF_Yasso07_C_fwd_plt,
    #                              totF_Millennial2_C_fwd_plt,totF_Yasso20_C_fwd_plt)))
    
    minF_plt <- min(as.numeric(cbind(totF_Roth_C_fwd_plt,totF_ICBM_C_fwd_plt,
                                     totF_Century_C_fwd_plt,totF_Yasso07_C_fwd_plt,totF_Yasso20_C_fwd_plt,
                                     totF_SG_C_fwd_plt)))
    
    maxF_plt <- max(as.numeric(cbind(totF_Roth_C_fwd_plt,totF_ICBM_C_fwd_plt,
                                     totF_Century_C_fwd_plt,totF_Yasso07_C_fwd_plt,totF_Yasso20_C_fwd_plt,
                                     totF_SG_C_fwd_plt)))
    
    # print(length(t_fwd))
    # print(length(totF_Roth_C_fwd))
    # print(length(mmmean_totF_plt))
    # print(t_fwd_plt)
    # print(totF_Roth_C_fwd_plt)
    # print(mmmean_totF_plt)

    # print(head(matrix(totF_Roth_C_fwd,1/computation_time_step_fwd)))
    # print(tail(matrix(totF_Roth_C_fwd,1/computation_time_step_fwd)))
    
    plot(t_fwd_plt, totF_Roth_C_fwd_plt, type="l", lty=1, xlab="Time (years)",ylab=" ",col="red",ylim=c(minF_plt-2,maxF_plt+2))
    title(ylab=express_plotF,main="Multi-model CO2 fluxes",mgp=c(2,1,0))
    lines(t_fwd_plt,totF_ICBM_C_fwd_plt,type="l", lty=1, col="black")
    lines(t_fwd_plt,totF_Century_C_fwd_plt,type="l", lty=1, col="green")
    lines(t_fwd_plt,totF_Yasso07_C_fwd_plt,type="l", lty=1, col="pink")
#    lines(t_fwd_plt,totF_Millennial2_C_fwd_plt,type="l", lty=1, col="blue")
    lines(t_fwd_plt,totF_Yasso20_C_fwd_plt,type="l", lty=1, col="hotpink")
    lines(t_fwd_plt,totF_SG_C_fwd_plt,type="l", lty=1, col="violet")
    lines(t_fwd_plt,mmmean_totF_plt,type="l", lwd = 3,col="grey")
    legend(par('usr')[2], par('usr')[4], c("RothC", "ICBM","Century","Yasso07","Yasso20","SG","Mean"),
           lty=1,lwd=c(1,1,1,1,1,3), col=c("red","black","green","pink","hotpink","violet","grey"), cex=0.8,xpd=NA,bty="n")
    
  }
  
  #return(list(Roth_C_fwd,ICBM_C_fwd,Century_C_fwd,Yasso07_C_fwd,Yasso20_C_fwd,Millennial2_C_fwd))
  return(list(Roth_C_fwd,ICBM_C_fwd,Century_C_fwd,Yasso07_C_fwd,Yasso20_C_fwd,SG_C_fwd))
  
}
