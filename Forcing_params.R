#Forcing
library(dplyr)
#---------------------------
#USER DEFINED PARAMETERS
#Decide wether to plot default figures [TRUE] or not [FALSE]
plot_figures=TRUE


#Define time step forward run [in years]
#computation_time_step_fwd = 1/365

#Other parameters that need to be defined
#Define spinup duration [years]
spinup_length=5000
#Define forward simulation length [years]
#as number of data/time_step
#simulation_length=length(Temp_month$Date)*computation_time_step_fwd
#simulation_length=10

#Soil variables that are needed but are not in the data: defined by default
soil_OM_thick = 25 #[cm]
#Default values
lignin_to_nitrogen_ratio=0.5 #[unitless] lignin to nitrogen ratio of the C input
structural_in_lignin_ratio=0.1 #[unitless] structural to lignin ratio of the C input
woodylittersize_scalar=2 #[cm] set to 0 for nonwoody, 2 finewoody, 20 coarse woody
#historical_LU = 'grassland' #historical land use of the site. Set either to 'arable', 'grassland' or 'forest'

#All these variables should be measured @5cm depth:

water_filled_pore_space= -1 #[unitless] ratio of the pore space filled with water at 5cm depth. If value not available, set to <0

ph_site = 6.

#CH4_conc_site=1858 #ppb atmospheric CH4 concentration

#CN ratio by plant species from TRY database
df_CH4 = read_excel("data/ch4_annmean_gl.xls",
                     col_names = TRUE,skip=0)
df_CH4 = df_CH4[c(2:length(df_CH4$year)),] #ppb

#AWEN values from Yasso manualii
df_AWEN = read_excel("data/AWEN_values_tree_species.xlsx",
           col_names = TRUE,skip=1)
#Default litter input partitioning Yasso07 and Yasso20
AWEN_default = c(0.52,0.18,0.08,0.2,0)

list_AWEN=list()
list_vectors = list(AWEN_default)
list_name_trees = c("Default",rep(NA,nrow(df_AWEN)))
list_Npool = c(AWEN_default[4],rep(NA,nrow(df_AWEN)))
for(i in 1:nrow(df_AWEN)){
  name_tree=as.character(df_AWEN[i,1])
  list_name_trees[i+1]=name_tree
  AWENH_i=as.numeric(df_AWEN[i,2:6])
  list_vectors[[i+1]]=AWENH_i
  list_Npool[i+1]=AWENH_i[4]

}

list_AWEN = setNames(list_vectors, list_name_trees)

##
#CN ratio by plant species from TRY database
df_CN = read.csv("data/Mean_CN_all_littertypes_TRY_db.csv",
                   header = TRUE)
#Order species in alphabetical order
df_CN <- df_CN[order(df_CN$SpeciesName), ]

mean_CN = df_CN$Mean[df_CN$SpeciesName == "Thuja plicata"]

#Add "Default" C:N = mean 
df_CN <- df_CN %>% add_row(SpeciesName = "Default", Mean = mean_CN, .before = 1)

list_CN <- setNames(list(df_CN$Mean)[[1]],list(df_CN$SpeciesName)[[1]])

#Average C content in plants  From Ma et al., 2018
avg_C_content_plants <- 48.209375 #%

#Calculate ligning:N ratio by plant species as C:N * lignin:C
#Suppose lignin = [N] litter pool in Yasso (from AWEN)
list_LC = list_Npool*100/avg_C_content_plants

list_LN = list_CN *list_LC

#Decomposition rates models 
#RothC
ksRothC = c(k.DPM = 10, k.RPM = 0.3, k.BIO = 0.66, k.HUM = 0.02,
            k.IOM = 0) #(1/yr)
# ksRothC = c(k.DPM = 10/12, k.RPM = 0.3/12, k.BIO = 0.66/12, k.HUM = 0.02/12, 
#             k.IOM = 0) #(1/month)

#ICBM
param_ICBM = c(k1=0.8,k2=0.00605,h=0.13) #(1/yr)

#Century
#ksCent = c(STR.surface = 0.076, MET.surface = 0.28, STR.belowground = 0.094, MET.belowground = 0.35, ACT = 0.14, SLW = 0.0038, PAS = 0.00013) #(1/week)
ksCent = c(STR.surface = 1/(12*0.245), MET.surface = 1/(12*0.066), STR.belowground = 1/(12*0.245), MET.belowground = 1/(12*0.066), ACT = 1/(12*0.149), SLW = 1/(12*5.480), PAS = 1/(12*241)) #1/month

#Yasso07
paramYasso07=c(kA = 0.73, kW = 5.8, kE = 0.29, kN = 0.031,kH = 0.0017, #[1/yr]
               beta1=0.096, beta2=-0.0014, gamma=-1.21,
               delta1=-1.7,delta2=0.86,r=-0.306,
               p1 = 0.48, p2 = 0.01, p3 = 0.83, p4 = 0.99,
               p5 = 0, p6 = 0.01, p7 = 0, p8 = 0, p9 = 0.03, p10 = 0,
               p11 = 0.01, p12 = 0.92, pH = 0.0045)


p13_Yasso = 0.0042
eps = 1e-15
paramYasso20 = c(kA = 0.51, kW = 5.19, kE = 0.13, kN = 0.1, kH = 0.0015,
                 p1 = 0.5, p2 =0, p3 = 1-p13_Yasso-eps,
                 p4 =1-p13_Yasso-eps, p5 = 0.99, p6 = 0.,
                 p7 = 0, p8 = 0., p9 = 0.,
                 p10 = 0, p11 = 0.163, p12 = 0.,
                 pH = p13_Yasso,
                 beta1AWE=0.158, beta1N=0.17, beta1H=0.067,
                 beta2AWE=-0.002, beta2N=-0.005, beta2H=0.,
                 g=-1.44, gN=-2.0,gH=-6.9,
                 delta1 = -2.55,
                 delta2 = 1.24,
                 r =  -0.25)

#Millennial2
paramMill2 =  list(p=c(CUEref=0.60,CUEt=0.012,Tae_ref=15,Kpl=10e3,Klb=290,phi=-15,porosity=0.60,
                       alfa_pl=2.5e12,Eapl=64320,alfa_lb=2.6e12,Ealb=60260,lambda=2.1*10e-4,ka_min=0.2,
                       Kld=1,pa=0.33,pb=0.5,pi=0.66,p1=0.186,p2=0.216,pc=0.86),
                   ks=c(kpa=0.02,kl=0.0015,kb=0.019,kma=0.020,kbd=0.0036))

