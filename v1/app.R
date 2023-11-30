######################################
## Multi-model ensemble webtool v1 ##
#####################################
#---------------------------------------------
#User friendly interface that
#launches an ensemble of SOC models
#with user input data or data extracted from
#global and regional opensource databases
#--------------------------------------------
#Developed by Elisa Bruni (ebruni93@gmail.com)
#November 2023
#--------------------------------------------

#Upload required packages
library(shiny)
library(shinydashboard)
library(SoilR)
library(shinyWidgets)
library(readxl)
library(dplyr)
library(scales)
library(shinyvalidate)
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(lubridate)
library(DT)
library(leaflet)


#---------------------------
#Call required functions
#---------------------------
#css
source("css_commands.R")
#---------------------------
#Forcing
source("Forcing_params.R")
#---------------------------
#Models resolution and multi-model tool
source("Holisoils_multimodel_v1_i1.R")
source("Holisoils_RothC_v1_i1.R")
source("Holisoils_ICBM_v1_i1.R")
source("Holisoils_Century_v1_i1.R")
source("Holisoils_Yasso07_v1_i1.R")
#source("Holisoils_Millennial2_v1_i1.R")
source("Holisoils_Yasso20_v1_i1.R")
source("Holisoils_SG_v1.R")
#---------------------------
#--coded models
source("Yasso07Model_fixed.R")
#source("Millennial2Model.R")
source("Yasso20Model.R")
source("ICBM_environmental_functions.R")
#---------------------------
#Load databases
source("Upload_databases.R")


#--------------------------#--------------------------#--------------------------#--------------------------
#--------BODY----#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------
body<- dashboardBody(
  #Background color 
  #hex colors https://htmlcolorcodes.com/(greens: #ddead1 #DAF7A6)
  tags$head(
    
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@100&display=swap")
  ),
  tags$style(HTML(css)),
  tags$script(HTML(js)),
  tabItems(
    #-----------------------
    #--------Project tab----
    #-----------------------
    tabItem(tabName = "Project", 
            # fluidRow(
            #   column(10,HTML("<h2><span class='bolder-text'>Project</span></h2>")),
            #   column(10,
            #          uiOutput("tabHOLI2"))),
            # fluidRow(
            #   column(10,HTML("<h2><span class='bolder-text'>Multi-model ensembles</span></h2>")),
            #   column(10, HTML("<h4 style='text-align: justify;'>
            #   One of the goals of HoliSoils is to develop and improve state-of-the-art soil models 
            #                   to predict the <span class='bolder-text'>effect of land management changes</span>  on forest soils. 
            #                   In order to benefit from the complementarity of different soil organic carbon (SOC) models,
            #                   the project aims to predict SOC stocks and greenhouse gas (GHG) emissions in forest soils 
            #                   with a multi-model ensemble.
            #                   That is, an ensemble of structurally different SOC models that can predict 
            #                   the evolution of SOC stocks and GHG gases emissions and provide the associated <span class='bolder-text'>level of uncertainty</span>.
            #                   </h4>"))),
            fluidRow(
              column(10,HTML("<h2><span class='bolder-text'>Multi-model ensemble interface</span></h2>")),
              column(10, HTML("<h4 style='text-align: justify;'>This webtool is designed to run a multi-model ensemble over
              <span class='bolder-text'>European forest sites</span>. It allows to predict the effect of climate, land-use and land management changes on  
                     <span class='bolder-text'>soil organic carbon</span> (SOC) stocks and <span class='bolder-text'>greenhouse gas</span> (GHG) emissions. By benefitting from the complementarity of
                     structurally different SOC models, it provides the level of uncertainty
                     of the predictions.</h4>"))
            ),
            fluidRow(
              column(10,HTML("<h2><span class='bolder-text'>Features</span></h2>")),
              column(10,
                     uiOutput("Download_documentation")),
              column(12,h4(" ")),
              column(10, DTOutput("data_ref_list"))),
            fluidRow(
              column(10,HTML("<h2><span class='bolder-text'>Limitations</span></h2>")),
              column(10,HTML("<h4 style='text-align: justify;'>
                     Please note that, even though the webtool can run with open source databases,
                     using <span class='bolder-text'>site measurements</span> can really improve 
                     the predictive capability of the models.
                     Thus, when possible, measurements should be prioritized.
                     Also, the multi-model ensemble uses default model parameter values.
                     Models should be <span class='bolder-text'> calibrated and validated</span> with observed data 
                     befaure applying them for predictions.
                             </h4>"))
            )
    ),
    #----------------------------------
    #--------Models description tab----
    #----------------------------------
    tabItem(tabName = "Models_description", 
            fluidRow(
              #Allows math expressions within dollar signs
              tags$script(
                type = "text/x-mathjax-config",
                HTML("
              MathJax.Hub.Config({
              tex2jax: {inlineMath: [['$', '$']]}
              });
              ")
              ),
              column(10,HTML("<h2><span class='bolder-text'>Models description</span></h2>")),
              column(10,HTML("<h3><span class='bolder-text'>Simulations of SOC stocks</span></h3>")),
              column(10,h4("Soil organic carbon models represent 
                           SOC with a conventional multi-compartmental structure 
                           that can be summarized with the following matrix equation:")), 
              br(),
              #div("$$\\alpha+\\beta$$ dC/dt=I(t)-A∙K∙ξ(t)∙C(t)"),
              column(10,h4(style="font-family: Roboto, sans-serif;",
                           "$\\frac{d\\mathbf{C}}{dt}=\\mathbf{I}(t)- ξ(t)\\times \\mathbf{A} \\times \\mathbf{K}\\times \\mathbf{C}(t),\\mathbf{\\ }\\mathbf{\\  } \\mathbf{\\ } \\mathbf{\\  } \\mathbf{C}(t=0)=\\mathbf{C}_{0},$")),
              column(10,h4("Where:") ),
              column(10,  withMathJax( h4(style = "font-family: Roboto, sans-serif;","● $\\mathbf{C}(t)$ is a nx1 vector describing the mass of SOC in the n compartments as a function of time (t);"))),
              column(10,  withMathJax( h4(style = "font-family: Roboto, sans-serif;","● $\\mathbf{I}(t)$ is a nx1 vector representing the C inputs to the soil;"))),
              column(10,  withMathJax( h4(style = "font-family: Roboto, sans-serif;","● $ξ(t)$ is the scalar effect of the pedo-climatic conditions on the decomposition of SOC;"))),
              column(10,  withMathJax( h4(style = "font-family: Roboto, sans-serif;","● $\\mathbf{A}$ is a nxn matrix describing the mass flow among each compartment.
                                          Its elements ${a}_{i,j}$ represent the flow of SOC from compartment j to compartment i, for i, j = 1,...,n;"))),
              column(10,  withMathJax( h4(style = "font-family: Roboto, sans-serif;","● $\\mathbf{K}$ is a nxn diagonal matrix containing the decomposition coefficients of the n compartments;"))),
              column(10,  withMathJax( h4(style = "font-family: Roboto, sans-serif;","● $\\mathbf{C}_{0}$ is a nx1 vector representing the initial level of SOC in each compartment at t=0.")))
              ,
              br(),
              column(10,HTML("<h3><span class='bolder-text'>Simulations of CO<sub>2</sub> fluxes</span></h3>")),
              column(10, HTML("<h4>The CO<sub>2</sub> fluxes can also be calculated with the SOC models as:
                              ${r}= \\mathbf{R} \\times \\mathbf{C}(t)$, 
                              where ${r}$ is the instantaneous release of C for all compartments 
                              and $\\mathbf{R}$ is a nxn diagonal matrix with the release 
                              coefficients ${r}_{j}$ calculated from matrix $\\mathbf{A}$ as: 
                              $\\mathbf{r}_{j}= [1-\\sum_{i=1}^{n}{(a}_{i,j}{)}]$.</h4>")),
              column(10,HTML("<h3><span class='bolder-text'>Other greenhouse gases</span></h3>")),
              column(10, HTML("<h4>In addition to the CO<sub>2</sub> fluxes, 
              <span class='bolder-text'>CH<sub>4</sub> uptake and N<sub>2</sub>O fluxes</span> 
              are also estimated using the SG models. 
                              These are simple empirical models allowing to estimate GHG fluxes using data on 
                              soil physiochemical properties, water and temperature.
                              </h4>")),
              br(),
              column(10, HTML("<h3><span class='bolder-text'>Models in the ensemble</span></h3>")),
              column(10, h4("The models currently included in the ensemble are:")),
              column(10,uiOutput("tabCentury")),
              column(10,uiOutput("tabICBM")),
              column(10,uiOutput("tabRothC")),
              column(10,uiOutput("tabYasso")),
              column(10,uiOutput("tabYasso20")),
              column(10,uiOutput("tabSG")),
              #verbatimTextOutput("text"),
              br(),
              br(),
              column(10,HTML("<h3><span class='bolder-text'>Schematization of the SOC models</span></h3>")),
              br(),
              column(10,img(src = "Schematization_models_v1.png", height = 1010/3, width = 1943/3)),
              column(10,HTML("<h5><span class='bolder-text'>Figure </span>
                             Schematization of the SOC models used in the ensemble.
                             Each box represents a SOC compartment where the C is transferred (black arrows),
                             or respired (red arrows). 
                             DPM, RPM BIO, HUM, IOM = decomposable plant material, resistant plant material, 
                             microbial biomass, humified organic matter, inert organic matter;
                             AM, BM, AS, BS  = aboveground metabolic, belowground metabolic; 
                             aboveground structural, and belowground structural.
                             </h5>")),
              br(),
              column(10,HTML("<h3><span class='bolder-text'>Models resolution</span></h3>")),
              column(10,HTML("<h4>To solve the equations of the SOC models, the initial partitioning of C in the different pools needs to be estimated.</h4>"),
                     HTML("<h4>To do that, we run the models with constant inputs until all the SOC pools reach a <span class='bolder-text'>steady-state</span>. 
                     That is, the annual variation of SOC in all pools is lower than 0.1% for at least 10 years.
                        As forcing, we use the average climate and environmental conditions of the decades preceding the onset of the simulations.</h4>")),
              column(10,uiOutput("tabDimassi")),
              column(10,h4("Finally, we solve the matrix differential equation for the specified simulation length.")),
              column(10,uiOutput("tabSoilR"))
            )#Fluidrow
    ),
    #----------------------------------
    #--------Scenarios building tab----
    #----------------------------------
    tabItem(tabName = "Scenarios_building", 
            fluidRow(column(10,HTML("<h2><span class='bolder-text'>Scenarios building</span></h2>")),
                     column(10,
                            HTML("<h4>The webtool allows the user to simulate and plot different 
                            <span class='bolder-text'>scenarios of climate, land-use and 
                            land management</span> changes, in order to see their effect
                               on the SOC stocks and GHGs emissions. In the following paragraphs, 
                               we briefly describe how the scenarios are built and the assumptions made.
                                 </h4>")),
                     br(),
                     column(10,HTML("<h3><span class='bolder-text'>Climate change scenarios</span></h3>")),
                     column(10,uiOutput("tabIPCC_AR5")),
                     column(12,h4(" ")),
                     column(10,img(src = "RCP_image.png", height = 1579/5, width = 2560/5)),
                     column(8,uiOutput("CClicence")),
                     br(),
                     column(10,HTML("<h3><span class='bolder-text'>Land-use change scenario</span></h3>")),
                     column(10,uiOutput("tabIPCC_AR6")),
                     column(12,h4(" ")),
                     column(10,img(src = "SSP_image.png", height = 1452/3.75, width = 1920/3.75)),
                     column(8,uiOutput("CClicence2")),
                     br(),
                     column(10,HTML("<h3><span class='bolder-text'>Land management change scenario</span></h3>")),
                     column(12,h4(" ")),
                     column(10,uiOutput("Download_documentation2")),
                     column(12,h4(" ")),
                     column(10,img(src = "Land_management_scheme_forsite.png", height = 1120/3 , width = 968/3 )),
                     column(8,HTML("<h5><span class='bolder-text'>Figure </span>
                             Representation of the assumptions made to calculate the litter input
                             following a disturbance event. On the left side of the image,
                             equations used to calculate the different variables are presented, 
                             while the right side shows a stylized representation of these variables.
                             </h5>")),
                     br()
            )#Fluidrow
    )#tabitem
    ,
    ############################################################################################
    ##########################  NEW TAB ITEM WITH DATA INPUT  ##################################
    ############################################################################################
    tabItem(tabName = "Data1", 
            HTML("<h2><span class='bolder-text'>Data input</span></h2>"),
            h4("This is the data input required to run the multi-model ensemble."),
            # Output: Tabset w/ plot, summary, and table ----
            ############################################################################################
            ########################## TABSET PANEL DATA INPUT  ########################################
            ############################################################################################    
            tabsetPanel(type = "tabs",
                        ############################################################################################
                        ########################## TAB PANEL SIMULATION SETUP ######################################
                        ############################################################################################  
                        tabPanel(h4("Simulations setup"), 
                                 fluidRow(
                                   column(10,
                                          h4(style="text-align: justify;",
                                             "Choose the geographical coordinates of the site, 
                                             the simulation length and the initial date of the simulations."))
                                 ),
                                 fluidRow(
                                   column(3,
                                          uiOutput("coordsLAT")),
                                   column(3,
                                          uiOutput("coordsLON")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Select the geographical coordinates 
                                            of the site (in decimal degrees). Note that only locations in
                                            European continental surface area will work.",
                                            icon("info-circle")
                                          )
                                   )
                                   
                                 ),
                                 fluidRow(
                                   column(6,span(textOutput("errorMessage"),style="color:red"))
                                 ),
                                 fluidRow(
                                   column(6,
                                          leafletOutput("map_data",height = "300px", width = "500px"))
                                 ),
                                 fluidRow(
                                   column(4, 
                                          uiOutput("simulationlength")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Select the duration of the simulations in number of years.",
                                            icon("info-circle")
                                          )
                                   )
                                 )
                                 ,
                                 fluidRow(
                                   column(4, 
                                          uiOutput("dates")),
                                   
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Insert the starting date of the simulations.",
                                            icon("info-circle")
                                          )
                                   )
                                 )
                        ),
                        ############################################################################################
                        ########################## TAB PANEL FOR SOIL DATA  ########################################
                        ############################################################################################   
                        tabPanel(h4("Soil data"),
                                 fluidRow(
                                   column(10,
                                          h4(style="text-align: justify;","Provide details about the soil variables.
                                          Soil variables should refer to the topsoil 
                                          (i.e., 0-10, 0-20 or 0-30 cm depth), 
                                          and should be at least consistent with each other."))
                                 ),
                                 fluidRow(
                                   column(4, 
                                          uiOutput("clay")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Clay concentration (%) of the site. If no data is available, the 
                                            LUCAS database will be used, which refers to 0-20 cm depth.",
                                            icon("info-circle")
                                          )
                                   )
                                 )
                                 ,
                                 fluidRow(
                                   column(4, 
                                          uiOutput("silt")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Silt concentration (%) of the site. If no data is available, the 
                                            LUCAS database will be used, which refers to 0-20 cm depth.",
                                            icon("info-circle")
                                          )
                                   )
                                 )
                                 ,
                                 # fluidRow(
                                 #   column(4, 
                                 #          uiOutput("CaCO3")),
                                 #   column(1,
                                 #          span(
                                 #            `data-toggle` = "tooltip", `data-placement` = "below",
                                 #            title = "CaCO3 content (g/kg) of the site. If no data is available, the 
                                 #            EU database LUCAS 2018 will be used, which refers to 0-20 cm depth 
                                 #            (https://esdac.jrc.ec.europa.eu/projects/lucas).",
                                 #            icon("info-circle")
                                 #          )
                                 #   )
                                 # ),
                                 fluidRow(
                                   column(4, 
                                          uiOutput("SOC")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Soil organic carbon stock of the site at the beginning of the simulations (MgC/ha). 
                                            If no data is available, the 
                                            LUCAS database will be used, which refers to 0-20 cm depth.
                                            Note that data for Cyprus and Croatia are not available.",
                                            icon("info-circle")
                                          )
                                   )
                                 ),
                                 
                                 fluidRow(column(4, 
                                                 numericInput("soilthick", 
                                                              h4(style="text-align: left;","Soil layer thickness (cm)"),
                                                              value = 25)),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Thickness of the soil organic matter layer (cm). By default, this value is set to 25 cm.",
                                                   icon("info-circle")
                                                 )
                                          )
                                 ),
                                 fluidRow(
                                   column(4, 
                                          uiOutput("CNratio")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Carbon to nitrogen ratio of the site at the onset of the experiment. If no data is available, the 
                                            LUCAS database will be used, which refers to 0-20 cm depth. Note that data for Cyprus and Croatia are not available.",
                                            icon("info-circle")
                                          )
                                   )
                                 )
                                 ,
                                 fluidRow(
                                   column(4,
                                          uiOutput("bulkdensity")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Bulk density (Mg/m3) of the soil. 
                                            If no data is available, the LUCAS database will be used, 
                                            which refers to 0-20 cm depth.",
                                            icon("info-circle")
                                          ))
                                 )
                        )
                        ,
                        ############################################################################################
                        ########################## TAB PANEL FOR LITTER INPUT DATA #################################
                        ############################################################################################   
                        tabPanel(h4("Litter input data"), 
                                 fluidRow(
                                   column(10,
                                          h4(style="text-align: justify;",
                                             "Provide details about the plant litter inputs."))
                                 ), 
                                 fluidRow(
                                   
                                   column(4,
                                          selectInput("AWEN_select", h4(style="text-align: justify;",
                                                                        "Dominant tree species"), 
                                                      choices = setNames(seq(1:length(names(list_AWEN))),names(list_AWEN)),
                                                      selected = 1)),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Choose the dominant tree species. If the tree species does not 
                                            appear in the list or there is no dominant species, 
                                            you can either choose the most similar species or the default option.",
                                            icon("info-circle")
                                          ))
                                 ),
                                 fluidRow(
                                   column(5,
                                          HTML("<h4 style='text-align: justify;'>Annual aboveground C input (MgC ha<sup>-1</sup> yr<sup>-1</sup>)</h4>"),
                                          helpText("Upload a .csv or .txt file with annual aboveground C input.
                                          The file should have one column where each row refers to
                                                   the annual aboveground C input"),
                                          fileInput("upload_Cinput_ag", NULL, accept = c(".csv", ".txt"))),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Time series of the level of aboveground C input (MgC/ha/yr) to the soil. 
                                            If no data is available, ISIMIP multi-model average simulations will be used.",
                                            icon("info-circle")
                                          ))
                                 ),
                                 fluidRow(
                                   column(5,
                                          HTML("<h4 style='text-align: justify;'>Annual belowground C input (MgC ha<sup>-1</sup> yr<sup>-1</sup>)</h4>"),
                                          helpText("Upload a .csv or .txt file with annual belowground C input. 
                                                   The file should have one column where each row refers to the annual belowground C input"),
                                          fileInput("upload_Cinput_bg", NULL, accept = c(".csv", ".txt"))),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Time series of the level of belowground C input (MgC/ha/yr) to the soil. 
                                            If no data is available, ISIMIP multi-model average simulations will be used.",
                                            icon("info-circle")
                                          ))
                                 ), 
                                 fluidRow(column(4, 
                                                 uiOutput("A_pool")),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Acid hydrolysables compounds fraction of the litter inputs.
                                                   If no data is available, 
                                                   species-specific input are used that are derived
                                                   from the Yasso07 model manual.",
                                                   icon("info-circle")))
                                 ),
                                 
                                 fluidRow(column(4, 
                                                 uiOutput("W_pool")),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Water soluble compounds fraction the litter inputs.
                                                   If no data is available, 
                                                   species-specific input are used that are derived
                                                   from the Yasso07 model manual.",
                                                   icon("info-circle")))
                                 ), 
                                 fluidRow(column(4, 
                                                 uiOutput("E_pool")),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Ethanol solubles compounds fraction of the litter inputs.
                                                   If no data is available, 
                                                   species-specific input are used that are derived
                                                   from the Yasso07 model manual.",
                                                   icon("info-circle")))
                                 ), 
                                 fluidRow(column(4, 
                                                 uiOutput("N_pool")),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Neither soluble nor hydrolysable compound fraction
                                                   of the litter inputs.
                                                   If no data is available, 
                                                   species-specific input are used that are derived
                                                   from the Yasso07 model manual.",
                                                   icon("info-circle")))
                                 ),
                                 fluidRow(column(4, 
                                                 uiOutput("LNratio")),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Lignin to nitrogen ratio of the litter input. If no data is available, 
                                                   a species-specific value is calculated as the product of the carbon to nitrogen ratio
                                                   and the lignin to carbon ratio, where the carbon to nitrogen ratio of the litter input
                                                   is derived from the TRY database for each species, and the percentage of carbon in the litter input
                                                   is considered as 48.2% for all species (Ma et al., 2018).",
                                                   icon("info-circle")
                                                 ))
                                 ),
                                 fluidRow(column(4, 
                                                 uiOutput("SLratio")),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Ligning fraction in the litter input. If no data is available,
                                                   we consider that the lignin fraction is equal to the 
                                                   neither soluble nor hydrolysable compound fraction.",
                                                   icon("info-circle")
                                                 ))
                                 ),
                                 fluidRow(column(4, 
                                                 numericInput("WLS", 
                                                              h4(style="text-align: left;","Woody litter size (cm)"), 
                                                              value = 2)),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Woody litter size of the litter input (cm). If no data is available, 
                                                   a default value of 2 cm will be used (Tuomi et al., 2009).",
                                                   icon("info-circle")
                                                 ))
                                 )
                        )   
                        ,
                        ############################################################################################
                        ########################## TAB PANEL FOR CLIMATE DATA  #####################################
                        ############################################################################################   
                        tabPanel(h4("Climate data"),
                                 fluidRow(
                                   column(10,
                                          h4(style="text-align: justify;",
                                             "Upload files containing the climate variables for the site.
                                             Uploading only a portion of the files is not possible as 
                                             using data from different sources may lead to issues in the soil water budget.
                                             In cases where not all the data is available, 
                                             model outputs from the ISIMIP repository will be used for the simulations."))
                                 ),
                                 fluidRow(
                                   column(5,
                                          h4(style="text-align: left;","Daily surface temperature (˚C)"),
                                          helpText("Upload a .csv or .txt file with daily surface temperature data. 
                                                   The file should have one column where each row refers to the daily surface temperature"),
                                          fileInput("upload_temperature_fwd", NULL, accept = c(".csv", ".txt"))),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Daily surface temperature (˚C) during the simulation length.",
                                            icon("info-circle")
                                          )),
                                   column(4,
                                          numericInput("Temp_sp", 
                                                       HTML("<h4 style='text-align: justify;'>Spin-up surface temperature (˚C) <br><br><br> </h4>"),
                                                       value=NULL)),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Daily average surface temperature (˚C) 
                                            in the decades preceding the simulations. For example, 
                                            if simulations start on January 2020, this should be the
                                            average daily temperature between 1990 and 2020.",
                                            icon("info-circle")
                                          ))
                                 )
                                 ,
                                 fluidRow(
                                   column(5,
                                          h4(style="text-align: left;","Daily precipitation (mm)"),
                                          helpText("Upload a .csv or .txt file with daily precipitation data. 
                                                   The file should have one column where each row refers to the daily precipitations"),
                                          fileInput("upload_precipitation_fwd", NULL, accept = c(".csv", ".txt"))),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Daily precipitation (mm) during the simulation length.",
                                            icon("info-circle")
                                          )),
                                   column(4,
                                          numericInput("Prec_sp", 
                                                       HTML("<h4 style='text-align: justify;'>Spin-up precipitation (mm) <br><br><br> </h4>"), 
                                                       value = NULL)),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Daily average precipitation (mm) in the decades preceding the simulations.
                                            For example, 
                                            if simulations start on January 2020, this should be the
                                            average daily precipitation between 1990 and 2020.",
                                            icon("info-circle")
                                          ))
                                 ),
                                 fluidRow(
                                   column(5,
                                          HTML("<h4 style='text-align: justify;'>Monthly potential evapotranspiration (mm month<sup>-1</sup>)</h4>"),
                                          helpText("Upload a .csv or .txt file with monthly potential evapotranspiration. 
                                                   The file should have one column where each row refers to the monthly potential evapotranspiration"),
                                          fileInput("upload_potevap_fwd", NULL, accept = c(".csv", ".txt"))),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Monthly potential evapotranspiration (mm/month) during the simulation length.",
                                            icon("info-circle")
                                          )),
                                   column(4,
                                          numericInput("Potevap_sp", 
                                                       HTML("<h4 style='text-align: justify;'>Spin-up potential evapotranspiration (mm month<sup>-1</sup>) <br><br> </h4>"), 
                                                       value = NULL)),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Monthly average potential evapotranspiration (mm/month) 
                                            in the decades preceding the simulations. For example, 
                                            if simulations start on January 2020, this should be the
                                            average monthly potential evapotranspiration between 1990 and 2020.",
                                            icon("info-circle")
                                          ))
                                 ),
                                 fluidRow(
                                   column(5,
                                          HTML("<h4 style='text-align: justify;'>Daily volumetric soil water content (mm<sup>3</sup> mm<sup>-3</sup>)</h4>"),
                                          helpText("Upload a .csv or .txt file with daily volumetric soil water content. 
                                                   The file should have one column where each row refers to the daily volumetric soil water content"),
                                          fileInput("upload_vswc_fwd", NULL, accept = c(".csv", ".txt"))),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Daily volumetric soil water content (mm3/mm3) during the simulation length."),
                                          icon("info-circle")
                                   ),
                                   column(4,
                                          numericInput("Vswc_sp", 
                                                       HTML("<h4 style='text-align: justify;'>Spin-up volumentric soil water content (mm<sup>3</sup> mm<sup>-3</sup>) <br><br> </h4>"), 
                                                       value = NULL)),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Daily average volumentric soil water content (mm3/mm3) 
                                            in the decades preceding the simulations. For example, 
                                            if simulations start on January 2020, this should be the
                                            average daily volumetric soil water content between 1990 and 2020.",
                                            icon("info-circle")
                                          ))
                                 ),
                                 fluidRow(
                                   column(5,uiOutput("CH4_data")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Atmospheric CH4 concentration (ppb) 
                                            at the beginning of the simulations. 
                                            If no data is available, the marine global annual average 
                                            atmospheric CH4 concentration is derived from the GML database
                                            for the first year of the simulations. 
                                            If the simulations start before 1983 or after 2023, 
                                            the 1983 and 2023 average are used, respectively.",
                                            icon("info-circle")
                                          ))
                                 )
                                 
                        )#TabPanel
                        ,
                        ############################################################################################
                        ########################## TAB PANEL FOR LAND MANAGEMENT DATA ##############################
                        ############################################################################################   
                        tabPanel(h4("Land management"), 
                                 fluidRow(
                                   column(10,
                                          h4(style="text-align: justify;",
                                             "Provide details about the disturbance event, 
                                             such as fires, thinning, clear-cutting and deseases."))
                                 ),
                                 fluidRow(
                                   column(4, 
                                          uiOutput("year_disturbance")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Year number of the disturbance event, 
                                            since the beginning of the simulations.",
                                            icon("info-circle")
                                          ))
                                 ),
                                 fluidRow(
                                   column(4, 
                                          uiOutput("AG_biomass")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Amount of plant aboveground biomass in the year preceding
                                            the disturbance event (MgC/ha/yr). If no data is available, we will use
                                            simulated multi-model average aboveground biomass from the ISIMIP database.",
                                            icon("info-circle")
                                          ))
                                 ),
                                 fluidRow(
                                   column(4, 
                                          uiOutput("BG_biomass")),
                                   column(1,
                                          span(
                                            `data-toggle` = "tooltip", `data-placement` = "below",
                                            title = "Amount of plant belowground biomass in the year preceding the disturbance event (MgC/ha/yr). 
                                            If no data is availble, we will consider that the belowground biomass is equal to the aboveground biomass.",
                                            icon("info-circle")
                                          ))
                                 ),
                                 fluidRow(column(4, 
                                                 numericInput("Mort_rate", 
                                                              h4(style="text-align: left;","Mortality rate (%)"), 
                                                              value = 50,
                                                              min=0,max=100)),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Mortality rate (%) of the vegetation after a disturbance event, 
                                                   such as fires, thinning, clear-cutting and deseases. 
                                                   That is, the percentage of the vegetation that died during the event.
                                                   The provided rate will be used as a fixed parameter
                                                   for the duration of the simulations.",
                                                   icon("info-circle")
                                                 ))
                                 ),
                                 fluidRow(column(4, 
                                                 numericInput("Harv_rate", 
                                                              h4(style="text-align: left;","Harvest rate (%)"), 
                                                              value = 100,
                                                              min=0,max=100)),
                                          column(1,
                                                 span(
                                                   `data-toggle` = "tooltip", `data-placement` = "below",
                                                   title = "Harvest rate (%) of the dead aboveground biomass after a disturbance event.
                                                   That is, the percentage of dead aboveground biomass that is harvested after the event.
                                                   The provided rate will be used as a fixed parameter
                                                   for the duration of the simulations.",
                                                   icon("info-circle")
                                                 ))
                                 )
                        )#TabPanel
            )#TabSetPanel
    )#TabItem
    ,
    #-----------------------------------------------
    #--------Data visualization tab-----------------
    #----------------------------------------------
    tabItem(tabName = "Visualization",
            tags$style(HTML("th {font-family: sans-serif; font-weight: 500;} 
                            td {text-align: center;}")),#Centers table content
            HTML("<h2><span class='bolder-text'>Data visualization</span></h2>"),
            fluidRow(
              column(10,
                     h4("This page shows the pedo-climatic 
                     conditions of the site selected 
                     in the Data input panel. 
                     Make sure to enter all the data fields.
                        It takes a while to run due to the large amount of computations."))),
            br(),
            fluidRow(
              #Insert a button to start plotting
              # column(width = 12, 
              #        offset = 0,
              #        #actionButton(inputId = "button", label = "Go!"),
              #        shinyBS::bsButton(inputId = "button", label = "Go!"),
              #        br()
              # ),
              
              #---------------------------------------
              #--------Soil data table-----------------
              #---------------------------------------
              column(width = 10, 
                     offset = 0,
                     box(width=10,
                         collapsible = TRUE, # This allows collapsing
                         collapsed = FALSE, # Initial value
                         HTML("<h3><span class='bolder-text'>Soil data</span></h3>"),
                         tableOutput("table_soildata")
                     )#box
              ),#column
              #---------------------------------------
              #--------Climate graphs-----------------
              #---------------------------------------
              column(width = 10, 
                     offset = 0,
                     box(width=10,
                         collapsible = TRUE, # This allows collapsing
                         collapsed = FALSE, # Initial value
                         HTML("<h3><span class='bolder-text'>Climate data</span></h3>"),
                         plotOutput("climate_plots")
                     )#box
              ),#column
              #-----------------------------------------------------------------------------------
              #--------Histogram Litter input in RCP2.6 and RCP6.0 // fixed and varying----------
              #----------------------------------------------------------------------------------
              column(width = 10, 
                     offset = 0,
                     box(width=10,
                         collapsible = TRUE, # This allows collapsing
                         collapsed = FALSE, # Initial value
                         HTML("<h3><span class='bolder-text'>Litter data</span></h3>"),
                         plotOutput("plot_Cin")
                     )#box
              ),#column
              #-----------------------------------------------------------------------------------
              #--------Plot Litter input in Management scenarios-------------------------------
              #----------------------------------------------------------------------------------
              column(width = 10, 
                     offset = 0,
                     box(width=10,
                         collapsible = TRUE, # This allows collapsing
                         collapsed = FALSE, # Initial value
                         #HTML("<h3><span class='bolder-text'>Land management</span></h3>"),
                         plotOutput("plot_Cin_LM")
                     )#box
              )#column
            )#fluidRow
    ),
    
    #------------------------------------------------------------
    #--------Plots SIMULATIONS-----------------------------------
    #------------------------------------------------------------
    
    tabItem(tabName = "Plots_simulations",
            HTML("<h2><span class='bolder-text'>Climate, land-use and land 
                 management change scenarios</span></h2>"),
            HTML("<h4>This page shows the evolution of SOC stocks
            and CO<sub>2</sub> fluxes under different climate scenarios (RCP 2.6 vs RCP 6.0), 
            land-use scenarios (year-2005 fixed land-use vs varying land-use according to SSP2), 
            and land management scenarios (with or without disturbance).
                 It takes a while to run because multiple soil models are launched.</h4>"),
            br(),

            tabsetPanel(type = "tabs",
                        #------------------------------------------------------------
                        #--------Simulations with fixed land-use---------------------
                        #------------------------------------------------------------
                        tabPanel(h4("Fixed land-use"),
                                 fluidRow( 
                                   column(10,HTML("<h4 style='text-align: left;'> 
                                               Move the slider to see the effect of clay on 
                                      SOC stocks and CO<sub>2</sub> fluxes</h4>")),
                                   uiOutput("clay_slider"),
                                   box(width=10,
                                       #plotlyOutput("simulations_fixedLU"))#box
                                       plotOutput("simulations_fixedLU"))#box
                                 )),
                        #------------------------------------------------------------
                        #--------Simulations with land-use change---------------------
                        #------------------------------------------------------------
                        tabPanel(h4("Land-use change"),
                                 fluidRow(
                                   column(10,HTML("<h4 style='text-align: left;'> 
                                               Move the slider to see the effect of clay on 
                                      SOC stocks and CO<sub>2</sub> fluxes</h4>")),
                                   uiOutput("clay_slider2"),
                                   box(width=10,plotOutput("simulations_LUchange"))
                                 )),
                        #------------------------------------------------------------
                        #--------Simulations land management change------------------
                        #------------------------------------------------------------
                        tabPanel(h4("Land management"),
                                 fluidRow(
                                   column(10,HTML("<h4 style='text-align: left;'> 
                                               Move the sliders to see how changing the mortality 
                                        and harvest rates affect the SOC stocks and CO<sub>2</sub> fluxes</h4>")),
                                   #column(4,uiOutput("clay_slider3")),
                                   column(5,uiOutput("MR_slider")),
                                   column(5,uiOutput("HR_slider")),
                                   box(width=10,plotOutput("plot_land_management"))
                                 ))
            )#tabsetpanel
    ),#tabItem
    #----------------------------------------
    #--------Downloads---------------------
    #----------------------------------------
    tabItem(tabName = "Downloads",
            HTML("<h2><span class='bolder-text'>Download model outputs</span></h2>"),
            HTML("<h4>This page allows to download the model simulation outputs
                 for all the scenarios built.</h4>"),
            br(),
            tabsetPanel(type = "tabs",
                        #----------------------------------------
                        #--------Download fixed land use-------
                        #----------------------------------------
                        tabPanel(h4("Fixed land-use"),
                                 br(),
                                 br(),
                                 sidebarPanel(
                                   selectInput("Select_variable", 
                                               HTML("<h5><span class='bolder-text'>Variable</span></h5>"),
                                               choices = c("SOC",
                                                           "CO2",
                                                           "CH4",
                                                           "N2O")),
                                   selectInput("Select_RCP", 
                                               HTML("<h5><span class='bolder-text'>Climate scenario</span></h5>"),
                                               choices = c("RCP2.6",
                                                           "RCP6.0")),
                                   selectInput("Select_model", 
                                               HTML("<h5><span class='bolder-text'>Model</span></h5>"),
                                               choices = NULL
                                   ),
                                   br(),
                                   # Button
                                   downloadButton("downloadData_FixedLU", h5("Download"))
                                 ),
                                 mainPanel(
                                   DTOutput("outTable_FixedLU")
                                 )
                                 
                        ),
                        #----------------------------------------
                        #--------Download land use change--------
                        #----------------------------------------
                        tabPanel(h4("Land-use change"),
                                 br(),
                                 br(),
                                 sidebarPanel(
                                   selectInput("Select_variable2", 
                                               HTML("<h5><span class='bolder-text'>Variable</span></h5>"),
                                               choices = c("SOC",
                                                           "CO2",
                                                           "CH4",
                                                           "N2O")),
                                   selectInput("Select_RCP2", 
                                               HTML("<h5><span class='bolder-text'>Climate scenario</span></h5>"),
                                               choices = c("RCP2.6",
                                                           "RCP6.0")),
                                   selectInput("Select_model2", 
                                               HTML("<h5><span class='bolder-text'>Model</span></h5>"),
                                               choices = NULL
                                   ),
                                   br(),
                                   # Button
                                   downloadButton("downloadData_LUchange", h5("Download"))
                                 ),
                                 mainPanel(
                                   DTOutput("outTable_LUchange")
                                 )
                                 
                        ),
                        #----------------------------------------
                        #--------Download land management --------
                        #----------------------------------------
                        tabPanel(h4("Land management"),
                                 br(),
                                 br(),
                                 sidebarPanel(
                                   selectInput("Select_variable3", 
                                               HTML("<h5><span class='bolder-text'>Variable</span></h5>"),
                                               choices = c("SOC",
                                                           "CO2",
                                                           "CH4",
                                                           "N2O")),
                                   selectInput("Select_RCP3", 
                                               HTML("<h5><span class='bolder-text'>Management scenario</span></h5>"),
                                               choices = c("Control",
                                                           "Disturbance")),
                                   selectInput("Select_model3", 
                                               HTML("<h5><span class='bolder-text'>Model</span></h5>"),
                                               choices = NULL
                                   ),
                                   br(),
                                   # Button
                                   downloadButton("downloadData_LM", h5("Download"))
                                 ),
                                 mainPanel(
                                   DTOutput("outTable_LM")
                                 )
                                 
                        )
                        
            )
    ),
    #----------------------------------------
    #--------About panel---------------------
    #----------------------------------------
    tabItem(tabName = "About",
            fluidRow(
              column(10,HTML("<h2><span class='bolder-text'>About the multi-model ensemble</span></h2>")),
              column(10,
                     uiOutput("tabHOLI2"))),
            fluidRow(h4(" ")),
            fluidRow(
              column(10,tagList(
                div(
                  style = "display: flex; align-items: center;",
                  icon("youtube", style = "margin-right: 5px; font-size: 1.5em;"),
                  HTML("<h4><span class='bolder-text'> Tutorial</span></h4>")
                ),
                tags$video(src='webtool_tutorial_v1.mp4', type="video/mp4", 
                           width=2846/5, height=1684/5, controls="controls")
              )),
              column(5,
                     tagList(
                       div(
                         style = "display: flex; align-items: center; font-size: 1.5em;",
                         icon("book", style = "margin-right: 5px;"),
                         HTML("<h4><span class='bolder-text'>Documentation</span></h4>")
                       ), 
                       uiOutput("Download_documentation3")
                     )
              ),
              column(3,tagList(
                div(
                  style = "display: flex; align-items: center; font-size: 1.5em;",
                  icon("github", style = "margin-right: 5px;"),
                  HTML("<h4><span class='bolder-text'> GitHub Code</span></h4>")
                ),
                uiOutput("githubcode")
              )),
              column(10,
                     tagList(
                       div(
                         style = "display: flex; align-items: center; font-size: 1.5em;",
                         icon("envelope", style = "margin-right: 5px;"),
                         HTML("<h4><span class='bolder-text'>Contact</span></h4>")
                       ), 
                       h4("bruni@geologie.ens.fr")
                     )
              ),
              
            ),
            fluidRow(h4(" ")),
            fluidRow(
              column(10,HTML("<h4><span class='bolder-text'>References</span></h4>")),
              br(),
              column(10,uiOutput("tabICBM.1")),
              column(12,br()),
              column(10,uiOutput("tabLUCAS.1")),
              column(12,br()),
              column(10,uiOutput("tabLUCAS.2")),
              column(12,br()),
              column(10,uiOutput("tabLUCAS.3")),
              column(12,br()),
              column(10,uiOutput("tabRothC.1")),
              column(12,br()),
              column(10,uiOutput("tabDimassi.1")),
              column(12,br()),
              column(10,uiOutput("tabISIMIP")),
              column(12,br()),
              column(10,uiOutput("tabSG.1")),
              column(12,br()),
              # column(10,uiOutput("tabIPCC_AR5_ref")),
              # column(12,br()),
              # column(10,uiOutput("tabIPCC_AR6_ref")),
              # column(12,br()),
              column(10,uiOutput("tabTRY")),
              column(12,br()),
              column(10,uiOutput("tabGML")),
              column(12,br()),
              column(10,uiOutput("tabplant_cont")),
              column(12,br()),
              column(10,uiOutput("tabCentury.1")),
              column(12,br()),
              column(10,uiOutput("tabSierra")),
              column(12,br()),
              column(10,uiOutput("tabYasso.1")),
              column(12,br()),
              column(10,uiOutput("tabYasso20.1")),
              column(12,br()),
              column(10,uiOutput("tabLUCAS.4"))
            )
            
            
            
            
    )
  )
)

#----------------------------------------
#--------Sidebar---------------------
#----------------------------------------

sidebar<-dashboardSidebar(
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@100&display=swap")
  ),
  tags$style(HTML(css)),
  sidebarMenuOutput("menu")
)

#----------------------------------------
#--------ui-----------------------------
#----------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Multi-model ensemble"),
  sidebar,
  body,
  skin="yellow"
)

#--------------------------#--------------------------#--------------------------#--------------------------
#------server--------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------

server <- function(input, output,session) {
  
  #https://fontawesome.com/icons
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Description", tabName = "Models", icon = icon("house"),
               menuSubItem("Project",tabName = "Project",icon = icon('circle-info')),
               menuSubItem("Models",tabName = "Models_description",icon = icon('folder-tree')),
               menuSubItem("Scenarios building", tabName = "Scenarios_building",icon=icon('arrows-split-up-and-left'))
      ),
      menuItem("Data",tabName = "Data", icon = icon("database"), 
               menuSubItem("Data input",tabName = "Data1",icon = icon('list'))
               #,menuSubItem("Data check",tabName = "Data2",icon = icon('list-check'))
      ),
      menuItem("Outputs",tabName = "Plots", icon = icon('square-poll-vertical'),
               menuSubItem("Data visualization",tabName = "Visualization",icon = icon('chart-line')),
               menuSubItem("Scenarios",tabName = "Plots_simulations",icon = icon('arrows-split-up-and-left')),
               menuSubItem("Download",tabName = "Downloads",icon = icon('download'))),
      menuItem("About", tabName = "About", icon = icon("earth-europe"))
    )
  })
  
  
  #-----------------
  ########################################################################
  ############ REACTIVE OUTPUT VARIABLES FOR DATA INPUT ##################
  ########################################################################
  
  ########################################################################
  ############ REACTIVE OUTPUT FOR SIMULATION SETUP ######################
  ########################################################################
  output$coordsLAT <- renderUI({
    numericInput("coordsLAT", 
                 h4(style="text-align: left;","GPS latitude (DD)"), 
                 value = 47.74579)
  })
  
  output$coordsLON <- renderUI({
    numericInput("coordsLON", 
                 h4(style="text-align: left;","GPS longitude (DD)"), 
                 value = 17.03537)
  })
  
  ########################################
  #----------------------------------------
  #--------MAP ---------------------------
  #----------------------------------------
  
  observe({
    # Validate lon-lat pair
    validate_lon_lat(input$coordsLON, input$coordsLAT)
  })
  
  #------------------------------------------------------
  #------Check if chosen lon-lat is in the sea-----------
  #-------from the list of NA values from .nc files-------
  #------And send an error message-----------------------
  #------------------------------------------------------
  validate_lon_lat <- function(lon, lat) {
    
    validate(
      need(!is.na(lat) && !is.na(lon),
           "Please enter valid latitude and longitude.")
    )
    
    distance_threshold<-0.25
    distance_lat <- abs(lat-lon_lat_nan$Lat)
    distance_lon <- abs(lon-lon_lat_nan$Lon)
    
    # Check if the lon-lat pair exists in the dataframe
    if (any(distance_lat<=distance_threshold & distance_lon<=distance_threshold)) {
      output$errorMessage <- renderText({
        "Select a location on the terrestrial surface. If your chosen point is along the coast, try moving it more inland."
      })
    } else {
      output$errorMessage <- renderText({
        ""
      })
    }
  }
  ###########
  
  #------------------------------------------------------
  #------draw map-------------------------------------
  #------------------------------------------------------
  
  output$map_data <- renderLeaflet({
    # Validate input values
    
    validate(
      need(!is.null(input$coordsLAT) && !is.null(input$coordsLON),
           "Please enter valid latitude and longitude.")
    )
    # Extract latitude and longitude from input
    latitude <- as.numeric(input$coordsLAT)
    longitude <- as.numeric(input$coordsLON)
    
    # Create leaflet map centered at the specified location
    leaflet() %>%
      setView(lat = latitude, lng = longitude, zoom = 5) %>%
      addTiles() %>%
      addMarkers(lat = latitude, lng = longitude, popup = "Chosen Location")
  })
  ########################################
  
  #----------------------------------------
  #-------simulation length----------------
  #----------------------------------------
  output$simulationlength <-renderUI({
    numericInput("simulationlength", 
                 h4(style="text-align: left;","Simulation length (yr)"), 
                 value = 10)
  })
  
  #----------------------------------------
  #--------initial date---------------------
  #----------------------------------------
  
  output$dates<-renderUI({
    dateInput(inputId ="dates", 
              label =h4(style="text-align: left;","Initial simulation date (YYYY-MM-DD)"),
              value="2020-01-01",min="2020-01-01",max="2099-01-01",startview="decade")
  })
  

  #----------------------------------------
  #Retrieve values of reactive simulation length and geographic coordinates
  #They can be called like simulation_length() after
  #----------------------------------------
  simulation_length<-reactive(
    input$simulationlength)
  coordsLAT<-reactive(
    input$coordsLAT)
  coordsLON<-reactive(
    input$coordsLON)
  
  dates_in<-reactive(
    input$dates)
  
  ########################################################################
  ############ REACTIVE OUTPUT FOR SOIL DATA ############################
  ########################################################################
  
  
  #----------------------------------------
  #--------clay----------------------------
  #----------------------------------------
  output$clay <- renderUI({
    lon_index_clay <- which.min(abs(lon_clay - coordsLON()))
    lat_index_clay <- which.min(abs(lat_clay - coordsLAT()))
    clay_site = as.numeric(clay_data[lon_index_clay, lat_index_clay])
    
    # #Select the closest lon-lat
    # sort_clay_lon <- sort(abs(lon_clay - coordsLON()))
    # sort_clay_lat <- sort(abs(lat_clay - coordsLAT()))
    # 
    # lon_index_clay<-which(abs(lon_clay - coordsLON())==sort_clay_lon[1])
    # lat_index_clay<-which(abs(lat_clay - coordsLAT())==sort_clay_lat[1])
    # 
    # clay_site = as.numeric(clay_data[ lon_index_clay, lat_index_clay])
    # print("Before")
    # print(clay_site)
    # 
    # #If the variable is NA, select the second closest lon-lat,..., as long as lat and lon distance <0.5(lat and lon)
    # for(i in 1:100){
    #   if(is.na(clay_site) & sort_clay_lon[i] <0.5 & sort_clay_lat[i]<0.5){
    #     lon_index_clay <- which(abs(lon_clay - coordsLON())==sort_clay_lon[i+1])
    #     lat_index_clay <- which(abs(lat_clay - coordsLAT())==sort_clay_lat[i+1])
    #     clay_site = as.numeric(clay_data[lon_index_clay, lat_index_clay])
    #   }else{
    #     break
    #   }
    # }
    # print("After loop")
    # print(clay_site)
    
    #If variable is still NA, consider it too far
    numericInput(inputId = "clay", label = h4(style="text-align: left;","Clay (%)"),value=round(clay_site,2),min=0,max=100)
  })
  
  #----------------------------------------
  #--------silt----------------------------
  #----------------------------------------
  output$silt <- renderUI({
    #silt_site<-as.numeric(soil_data %>% filter(round(GPS_LAT,1)==round(coordsLAT(),1),round(GPS_LONG,1)==round(input$coordsLON,1)) %>%select(silt))
    lon_index_silt <- which.min(abs(lon_silt - coordsLON()))
    lat_index_silt <- which.min(abs(lat_silt - coordsLAT()))
    silt_site = as.numeric(silt_data[lon_index_silt, lat_index_silt])
    numericInput(inputId = "silt", label = h4(style="text-align: left;","Silt (%)"),value=round(silt_site,2),min=0,max=100)
    
  })
  
  #Check clay value, based on silt input
  observeEvent(input$silt,{
    updateNumericInput(session, inputId = "clay", max = 100-input$silt)
    iv1 <- InputValidator$new()
    iv1$add_rule("clay", sv_between(0, 100-input$silt))
    iv1$enable()
  })
  
  #check silt value, based on clay input
  observeEvent(input$clay,{
    updateNumericInput(session, inputId = "silt", max = 100-input$clay)
    iv2 <- InputValidator$new()
    iv2$add_rule("silt", sv_between(0, 100-input$clay))
    iv2$enable()
  })
  
  #Check max simulation length (=80)
  observeEvent(input$simulationlength,{
    updateNumericInput(session, inputId = "simulationlength", max = 80)
    iv1 <- InputValidator$new()
    iv1$add_rule("simulationlength", sv_between(1, 80))
    iv1$enable()
  })
  
  
  
  ##########---------------##############
  # output$CaCO3 <- renderUI({
  #   CaCO3_site<-as.numeric(soil_data %>% filter(round(GPS_LAT,1)==round(coordsLAT(),1),round(GPS_LONG,1)==round(input$coordsLON,1)) %>%select(CaCO3))
  #   numericInput(inputId = "CaCO3", label = "CaCO3 (g kg-1)",value=CaCO3_site)
  # })
  # 
  # iv1 <- InputValidator$new()
  # iv1$add_rule("CaCO3", sv_gt(0))
  # iv1$enable()
  
  #----------------------------------------
  #--------SOC stocks----------------------------
  #----------------------------------------
  
  output$SOC <- renderUI({
    
    lon_index_SOC <- which.min(abs(lon_SOC - coordsLON()))
    lat_index_SOC <- which.min(abs(lat_SOC - coordsLAT()))
    SOC_site = as.numeric(SOC_data[lon_index_SOC, lat_index_SOC])
    
    numericInput(inputId = "SOC", label = HTML("<h4 style='text-align: left;'> 
                                               Initial SOC stock (MgC ha<sup>-1</sup>) </h4>"),value=round(SOC_site,2))
  })
  
  #Set limit soc stock
  iv1 <- InputValidator$new()
  iv1$add_rule("SOC", sv_gt(0))
  iv1$enable()
  
  #----------------------------------------
  #--------CN ratio----------------------------
  #----------------------------------------
  output$CNratio <- renderUI({
    lon_index_CN <- which.min(abs(lon_CN - coordsLON()))
    lat_index_CN <- which.min(abs(lat_CN - coordsLAT()))
    CN_site = as.numeric(CN_data[lon_index_CN, lat_index_CN])  
    numericInput(inputId = "CNratio", label = h4(style="text-align: left;","C:N ratio"),value=round(CN_site,2))
  })
  
  #Set limit CN ratio
  iv1 <- InputValidator$new()
  iv1$add_rule("CNratio", sv_gt(0))
  iv1$enable()
  
  #----------------------------------------
  #--------Bulk density----------------------------
  #----------------------------------------
  output$bulkdensity <- renderUI({
    lon_index_BD <- which.min(abs(lon_BD - coordsLON()))
    lat_index_BD <- which.min(abs(lat_BD - coordsLAT()))
    BD_site<-as.numeric(BD_data[lon_index_BD, lat_index_BD])
    numericInput(inputId = "bulkdensity", label = HTML("<h4 style='text-align: left;'> 
                                               Bulk density (Mg m<sup>-3</sup>) </h4>"),value=round(BD_site,2))
  })
  #Set limit bulkdensity
  iv1 <- InputValidator$new()
  iv1$add_rule("bulkdensity", sv_gt(0))
  iv1$enable()
  
  
  #----------------------------------------
  #--------Atmosph CH4 data-----------------
  #----------------------------------------
  output$CH4_data<-renderUI({
    req(dates_in())
    year_in = as.numeric(format(dates_in(), "%Y"))
    
    # Check if we have data for the selected year
    if (year_in %in% df_CH4$year) {
      CH4_site <- df_CH4$mean[df_CH4$year == year_in]
    } else {
      # If not, use the closes year data
      closest_year <- which.min(abs(df_CH4$year - year_in))
      CH4_site <- df_CH4$mean[closest_year]
    }
    
    numericInput(inputId = "CH4_data", label = HTML("<h4 style='text-align: left;'> 
                                               Atmospheric CH<sub>4</sub> concentration (ppb)</h4>"),value=round(CH4_site,2))
  })
  
  #Set limit CH4 data
  iv1 <- InputValidator$new()
  iv1$add_rule("CH4_data", sv_gt(0))
  iv1$enable()
  
  ########################################################################
  ############ REACTIVE OUTPUT FOR LITTER INPUT DATA #####################
  ########################################################################
  
  #----------------------------------------
  #--------Lignin fraction-----------------
  #----------------------------------------
  output$SLratio <- renderUI({
    numericInput(inputId = "SLratio", label = h4(style="text-align: left;","Lignin fraction"),
                 value=round(list_AWEN[[as.numeric(input$AWEN_select)]][4],2))
  })
  #Set limit SL ratio
  iv1 <- InputValidator$new()
  iv1$add_rule("SLratio", sv_gt(0))
  iv1$enable()
  
  #----------------------------------------
  #--------Lignin to nitrogen fraction-----------------
  #----------------------------------------
  output$LNratio <- renderUI({
    numericInput(inputId = "LNratio", label = h4(style="text-align: left;","Lignin:nitrogen ratio"),
                 value=round(list_LN[[as.numeric(input$AWEN_select)]],2))
  })
  
  #Set limit LN ratio
  iv1 <- InputValidator$new()
  iv1$add_rule("LNratio", sv_gt(0))
  iv1$enable()
  
  #---------------------------------------------------------
  #--------Acid hydrolysables compounds fraction-----------------
  #---------------------------------------------------------
  output$A_pool <- renderUI({
    numericInput(inputId = "A_pool", label = h4(style="text-align: left;","Acid hydrolysables compounds fraction"),
                 value=round(list_AWEN[[as.numeric(input$AWEN_select)]][1],2))
  })
  #Set limit A pool
  iv1 <- InputValidator$new()
  iv1$add_rule("A_pool", sv_between(0, 1))
  iv1$enable()
  
  #---------------------------------------------------------
  #--------Water solubles  compounds fraction-----------------
  #---------------------------------------------------------
  output$W_pool <- renderUI({
    numericInput(inputId = "W_pool", label = h4(style="text-align: left;","Water solubles compounds fraction"),
                 value=round(list_AWEN[[as.numeric(input$AWEN_select)]][2],2))
  })
  
  #Set W pool
  iv1 <- InputValidator$new()
  iv1$add_rule("W_pool", sv_between(0, 1))
  iv1$enable()
  
  #---------------------------------------------------------
  #--------Ethanol solubles  compounds fraction-----------------
  #---------------------------------------------------------
  output$E_pool <- renderUI({
    numericInput(inputId = "E_pool", label = h4(style="text-align: left;","Ethanol solubles compounds fraction"),
                 value=round(list_AWEN[[as.numeric(input$AWEN_select)]][3],2))
  })
  #Set limits E pool
  iv1 <- InputValidator$new()
  iv1$add_rule("E_pool", sv_between(0, 1))
  iv1$enable()
  
  #-----------------------------------------------------------------------
  #--------Neither soluble nor hydrolysable compounds fraction-------------
  #-----------------------------------------------------------------------
  output$N_pool <- renderUI({
    numericInput(inputId = "N_pool", label = h4(style="text-align: left;","Neither soluble nor hydrolysable compounds fraction"),
                 value=round(list_AWEN[[as.numeric(input$AWEN_select)]][4],2))
  })
  
  #Set limits N pool
  iv1 <- InputValidator$new()
  iv1$add_rule("N_pool", sv_between(0, 1))
  iv1$enable()
  
  
  #-----------------------------------------------------------------------
  #--------Function to retrieve the C input from ISIMIP data-------------
  #-----------------------------------------------------------------------
  retreive_Cinput<-function(lon_rcp,lat_rcp,time_rcp,ncvar_rcp){
    
    #lon_rcp = lon_litter_rcp26
    #lat_rcp = lat_litter_rcp26
    #time_rcp = litter_time_rcp26
    #ncvar_rcp = litter_rcp26
    
    simulation_length<-min(simulation_length(),80)
    
    #Define lon-lat values to retrieve, based on user input
    lon_index_litter <- which.min(abs(lon_rcp - coordsLON()))
    lat_index_litter <- which.min(abs(lat_rcp - coordsLAT()))
    #----------------------------------------------------
    #No spinup for (rcp26)
    #----------------------------------------------------
    #----------------------------------------------------
    #Retrieve forward (rcp26)
    #----------------------------------------------------
    #Convert time to dates
    litter_time_plot <- as.Date.character(format(time_rcp, "%Y-%m-%d"))
    
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_litter <- which(format(time_rcp, "%Y-%m-%d") >= "2020-01-01"
                                   & format(time_rcp, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    litter_sel_lonlat_timestep_fwd = ncvar_rcp[lon_index_litter, lat_index_litter, time_index_fwd_litter]
    
    #Convert variable from kg/m2/sec to t/ha/yr
    litter_sel_lonlat_timestep_fwd<-litter_sel_lonlat_timestep_fwd*(60*60*24*365.25)*10 #kg/m2/sec =>t/ha/yr
    
    #Select time series for fwd
    litter_time_sel_fwd = litter_time_plot[time_index_fwd_litter]
    litter_time_sel_plot_fwd = as.Date.character(format(litter_time_sel_fwd, "%Y-%m-%d"))
    
    Litter_time_fwd = data.frame("Date"=litter_time_sel_plot_fwd,"Litter"=litter_sel_lonlat_timestep_fwd)
    
    #Select intial date
    litter_sel_lonlat_timestep_fwd_sel = subset(Litter_time_fwd,Litter_time_fwd$Date>=dates_in())
    max_length = min(simulation_length,length(Litter_time_fwd$Date))
    #print("max_length")
    #print(max_length)
    #Select number of years
    litter_sel_lonlat_timestep_fwd_sel = litter_sel_lonlat_timestep_fwd_sel[1:max_length,]
    
    #Cinput_site=mean(litter_sel_lonlat_timestep_fwd_sel$Litter)
    Cinput_site=litter_sel_lonlat_timestep_fwd_sel$Litter
    #print(Cinput_site)
    
    
    return(Cinput_site)
  }
  
  # output$Cinput_ag <- renderUI({
  #   retreive_Cinput_ag_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_ag)
  #   retreive_Cinput_ag_rcp26_fix_mean = mean(retreive_Cinput_ag_rcp26_fix)
  #   numericInput(inputId = "Cinput_ag", label =HTML("<h4 style='text-align: left;'> 
  #                                              Average aboveground C input (MgC ha<sup>-1</sup> yr<sup>-1</sup>) </h4>"),value=round(retreive_Cinput_ag_rcp26_fix_mean,2))
  # })
  # iv1 <- InputValidator$new()
  # iv1$add_rule("Cinput_ag", sv_gt(0))
  # iv1$enable()
  # 
  # output$Cinput_bg <- renderUI({
  #   retreive_Cinput_bg_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_bg)
  #   retreive_Cinput_bg_rcp26_fix_mean = mean(retreive_Cinput_bg_rcp26_fix)
  #   numericInput(inputId = "Cinput_bg", label =HTML("<h4 style='text-align: left;'> 
  #                                              Average belowground C input (MgC ha<sup>-1</sup> yr<sup>-1</sup>) </h4>"),value=round(retreive_Cinput_bg_rcp26_fix_mean,2))
  # })
  # 
  # iv1 <- InputValidator$new()
  # iv1$add_rule("Cinput_bg", sv_gt(0))
  # iv1$enable()
  
  #------------------
  #Chek if Cinput_ag is uploaded
  #------------------
  file_Cinput_ag_uploaded <- reactive({
    !is.null(input$upload_Cinput_ag)
  })
  #------------------
  #Chek if Cinput_ag is uploaded
  #------------------
  file_Cinput_bg_uploaded <- reactive({
    !is.null(input$upload_Cinput_bg)
  })

  #----------------------------------------------------------------------------------
  #--------Function to retrieve the ag C input from either user or ISIMIP data-------------
  #----------------------------------------------------------------------------------
  data_Cinput_ag <- reactive({
    
    if(file_Cinput_ag_uploaded()){
      
      print("####################################################")
      print("####################################################")
      print("The user has input AG C input TIME SERIES data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      file <- input$upload_Cinput_ag
      ext <- tools::file_ext(file$datapath)
      
      #req(file)
      #validate(need(ext == "csv", "Please upload a csv file"))
      if (ext=="csv"){
        read.csv(file$datapath, header = FALSE)[1:simulation_length()]
      } else if (ext=="txt"){
        read.table(file$datapath,sep=",", header = FALSE)$V1[1:simulation_length()]
      }else(warning("The file should be in a csv or txt format"))
      #print(y)
      
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input AG C input TIME SERIES data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      
      retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_ag)
    }
    
  })
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the bg C input from either user or ISIMIP data-------------
  #----------------------------------------------------------------------------------
  
  data_Cinput_bg <- reactive({
    
    if(file_Cinput_bg_uploaded()){
      print("####################################################")
      print("####################################################")
      print("The user has input BG C input TIME SERIES data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      file <- input$upload_Cinput_bg
      ext <- tools::file_ext(file$datapath)
      
      #req(file)
      #validate(need(ext == "csv", "Please upload a csv file"))
      if (ext=="csv"){
        read.csv(file$datapath, header = FALSE)[1:simulation_length()]
      } else if (ext=="txt"){
        print("bg C input")
        print(read.table(file$datapath,sep=",", header = FALSE)$V1[1:simulation_length()])
        read.table(file$datapath,sep=",", header = FALSE)$V1[1:simulation_length()]
      }else(warning("The file should be in a csv or txt format"))
      #print(y)
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input BG C input TIME SERIES data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_bg) 
    }
    
    # file <- input$upload_Cinput_bg
    # ext <- tools::file_ext(file$datapath)
    # 
    # #req(file)
    # #validate(need(ext == "csv", "Please upload a csv file"))
    # if (ext=="csv"){
    #   y <- read.csv(file$datapath, header = FALSE)
    # } else if (ext=="txt"){
    #   y <- read.table(file$datapath,sep=",")
    # }else(warning("The file should be in a csv or txt format"))
    # #print(y)
  })
  
  
  ########################################################################
  ############ REACTIVE OUTPUT MANAGEMENT DATA #####################
  ########################################################################
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the C biomass (cveg) from  ISIMIP data-------------
  #----------------------------------------------------------------------------------
  retreive_Cbiomass<-function(lon_rcp,lat_rcp,time_rcp,ncvar_rcp,year_mortality_event){
    
    #lon_rcp = lon_veg_litter_rcp26
    #lat_rcp = lat_veg_litter_rcp26
    #time_rcp = veg_clitter_time_rcp26
    #ncvar_rcp = veg_clitter_rcp60
    
    simulation_length<-min(simulation_length(),80)
    
    #Define lon-lat values to retrieve, based on user input
    lon_index_litter <- which.min(abs(lon_rcp - coordsLON()))
    lat_index_litter <- which.min(abs(lat_rcp - coordsLAT()))
    #----------------------------------------------------
    #No spinup for (rcp26)
    #----------------------------------------------------
    #----------------------------------------------------
    #Retrieve forward (rcp26)
    #----------------------------------------------------
    #Convert time to dates
    litter_time_plot <- as.Date.character(format(time_rcp, "%Y-%m-%d"))
    
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_litter <- which(format(time_rcp, "%Y-%m-%d") >= "2020-01-01"
                                   & format(time_rcp, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    litter_sel_lonlat_timestep_fwd = ncvar_rcp[lon_index_litter, lat_index_litter, time_index_fwd_litter]
    
    #Convert variable from kg/m2 to t/ha
    litter_sel_lonlat_timestep_fwd<-litter_sel_lonlat_timestep_fwd*10 #kg/m2 =>t/ha
    
    #Select time series for fwd
    litter_time_sel_fwd = litter_time_plot[time_index_fwd_litter]
    litter_time_sel_plot_fwd = as.Date.character(format(litter_time_sel_fwd, "%Y-%m-%d"))
    
    Litter_time_fwd = data.frame("Date"=litter_time_sel_plot_fwd,"Litter"=litter_sel_lonlat_timestep_fwd)
    
    #Select intial date
    litter_sel_lonlat_timestep_fwd_sel = subset(Litter_time_fwd,Litter_time_fwd$Date>=dates_in())
    max_length = min(simulation_length,length(Litter_time_fwd$Date))
    #Select number of years
    litter_sel_lonlat_timestep_fwd_sel = litter_sel_lonlat_timestep_fwd_sel[1:max_length,]
    
    
    #Select year of mortality event
    Cbiomass_site=litter_sel_lonlat_timestep_fwd_sel$Litter[year_mortality_event]
    
    print("Printing from function retreive_Cbiomass")
    print("Cbiomass_site for selected year:")
    print(Cbiomass_site)
    
    return(Cbiomass_site)
  }
  
  #-----------------------------------------
  #--------YEar or disturbance-------------
  #----------------------------------------
  output$year_disturbance<-renderUI({
    numericInput(inputId = "year_disturbance", label = HTML("<h4 style='text-align: left;'> 
                                               Year of disturbance event (number of year) </h4>"),value=1)
  })
  
  #------------------------------------------------------------
  #--------AG biomass (initialize with ISMIP data)-------------
  #-----------------------------------------------------------
  output$AG_biomass <- renderUI({
    req(input$year_disturbance)
    AG_biomass_initial_value = retreive_Cbiomass(lon_veg_clitter_rcp26,lat_veg_clitter_rcp26,veg_clitter_time_rcp26,veg_clitter_rcp26,input$year_disturbance)
    AG_biomass_initial_value=as.numeric(AG_biomass_initial_value)
    numericInput(inputId = "AG_biomass", label = HTML("<h4 style='text-align: left;'> 
                                               Aboveground biomass (MgC ha<sup>-1</sup> yr<sup>-1</sup>) </h4>"),value=round(AG_biomass_initial_value,2))
  })
  
  #------------------------------------------------------------
  #--------BG biomass (initialize with ISMIP data)-------------
  #-----------------------------------------------------------
  
  output$BG_biomass <- renderUI({
    req(input$AG_biomass)  
    print("input$AG_biomass")
    print(input$AG_biomass)
    print(round(mean(as.numeric(input$AG_biomass)),2))
    numericInput(inputId = "BG_biomass", label = HTML("<h4 style='text-align: left;'> 
                                               Belowground biomass (MgC ha<sup>-1</sup> yr<sup>-1</sup>) </h4>"),value=round(as.numeric(input$AG_biomass),2))
  })
  
  
  
  #------------------------------------------------------------
  #--------Function to calculate C input from mortality event-------------
  #-----------------------------------------------------------
  calculate_Cin_mortality_event<-function(Cinput_ag,Cinput_bg,Cbiomass_cveg_ag,Cbiomass_cveg_bg,mortality_index, harvest_index){

    #===MORTALITY EVENT====
    #print("Retrieving C input for disturbance scenario")
    ###Supposing mortality event at year 1
    mort_year = input$year_disturbance
    ##########################################
    #Carbon input from survival vegetation
    ##########################################
    #AGin (C input from aboveground survival vegetation) after mortality event
    #retreive_Cinput_ag_rcp26_fix_NOTDIED = Cinput_ag[c(mort_year:length(Cinput_ag))]*(mortality_index/100.) #Amount of abouveground litter input that remains after mortality
    retreive_Cinput_ag_rcp26_fix_NOTDIED = Cinput_ag[c(mort_year:length(Cinput_ag))]*(1-mortality_index/100.) #Amount of abouveground litter input that remains after mortality
    #BGin_notdied (C input from belowground survival vegetation) after mortality event
    retreive_Cinput_bg_rcp26_fix_NOTDIED = 0.333*(1.92*(100.*retreive_Cinput_ag_rcp26_fix_NOTDIED)+130.)*0.01 #belowground C input of the NON died vegetation #where *100 is the conversion factor from tC/ha to (g/m2)
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#  
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #>
    
    #AG_cveg
    Cbiomass_rcp26_ag = Cbiomass_cveg_ag
    #AGdied
    #Cbiomass_rcp26_ag_DIED = Cbiomass_rcp26_ag*(input$Mort_rate/100.) #Amount of aboveground biomass died after disturbance event
    Cbiomass_rcp26_ag_DIED = Cbiomass_rcp26_ag*(mortality_index/100.) #Amount of aboveground biomass died after disturbance event
    #AGkept
    #Cbiomass_rcp26_ag_KEPT = Cbiomass_rcp26_ag_DIED*(1-input$Harv_rate/100.) #Amount of aboveground biomass kept on the soil after harvest
    Cbiomass_rcp26_ag_KEPT = Cbiomass_rcp26_ag_DIED*(1-harvest_index/100.) #Amount of aboveground biomass kept on the soil after harvest
    
    #FOL_TOTbiomass
    Cbiomas_rcp26_ag_FOLIAR = (Cbiomass_cveg_ag+Cbiomass_cveg_bg)*0.15 #foliar biomass as a fraction of TOT biomass, see Konôpka et al., 2021 for different tree species
    #FOLdied
    #Cbiomas_rcp26_ag_DIED_FOLIAR = Cbiomas_rcp26_ag_FOLIAR*(input$Mort_rate/100.) #foliar biomass of the died vegetation
    Cbiomas_rcp26_ag_DIED_FOLIAR = Cbiomas_rcp26_ag_FOLIAR*(mortality_index/100.) #foliar biomass of the died vegetation
    Cbiomass_rcp26_bg_DIED = 0.333*(1.92*(100.*Cbiomas_rcp26_ag_DIED_FOLIAR)+130.)*0.01 #belowground biomass of the died vegetation #where *100 is the conversion factor from tC/ha to (g/m2)
    
    
    #if(input$Mort_rate>0){
    if(mortality_index>0){
      #AGinTOT (time series)
      Cinput_disturbance_ag = Cinput_ag
      #After disturbance
      Cinput_disturbance_ag[c(mort_year:length(Cinput_disturbance_ag))] = retreive_Cinput_ag_rcp26_fix_NOTDIED
      #Add additional AG biomass at year of disturbance
      Cinput_disturbance_ag[mort_year] = Cinput_disturbance_ag[mort_year]+Cbiomass_rcp26_ag_KEPT

      #BGinTOT (time series)
      Cinput_disturbance_bg = Cinput_bg
      
      #After disturbance
      Cinput_disturbance_bg[c(mort_year:length(Cinput_disturbance_bg))] =  retreive_Cinput_bg_rcp26_fix_NOTDIED
      #Add additional BG biomass at year of disturbance
      Cinput_disturbance_bg[mort_year] = Cinput_disturbance_bg[mort_year]+Cbiomass_rcp26_bg_DIED

    }else{
      #AGinTOT (time series)
      Cinput_disturbance_ag = Cinput_ag
      #BGinTOT (time series)
      Cinput_disturbance_bg = Cinput_bg
    }
    
    
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    #Cin TOT in case of disturbance (time series)
    #Cinput_disturbance = Cinput_disturbance_ag+Cinput_disturbance_bg
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    
    return(list(Cinput_disturbance_ag,Cinput_disturbance_bg))
  }
  
  #--------------------------------------------------------------------------------
  #Add limits to LON and LAT (based on minimin-maximum lon-lat in pedoclim files)
  #---------------------------------------------------------------------------------
  iv1 <- InputValidator$new()
  iv1$add_rule("coordsLON", sv_between(-14.75,34.75))
  iv1$enable()
  
  iv1 <- InputValidator$new()
  iv1$add_rule("coordsLAT", sv_between(32.47,71.95))
  iv1$enable()
  
  
  #--------------------------------------------------------------------------------
  #Functions to read user input climate data
  #---------------------------------------------------------------------------------
  
  values <- reactiveValues(
    set_temp_user_data = NULL,
    set_prec_user_data = NULL,
    set_potevap_user_data = NULL,
    set_vswc_user_data = NULL
  )
  
  #------------------------------------------------------------
  #--------Checks if upload data is not null-------------
  #-----------------------------------------------------------
  file_data_temperature <- reactive({
    !is.null(input$upload_temperature_fwd)
  })
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the temperature from either user or ISIMIP data-------------
  #----------------------------------------------------------------------------------
  
  data_temperature <- reactive({
    
    if(file_data_temperature()){
      
      print("####################################################")
      print("####################################################")
      print("The user has input temperature data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      values$set_temp_user_data <- TRUE
      print(values$set_temp_user_data)
      print("===")
      file <- input$upload_temperature_fwd
      ext <- tools::file_ext(file$datapath)
      
      if (ext=="csv"){
        y <- read.csv(file$datapath, header = FALSE)
      } else if (ext=="txt"){
        y <- read.table(file$datapath,sep=",")
      }else(warning("The file should be in a csv or txt format"))
      
      upload_user_temp_data_site(y)
      
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input temperature data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      
      values$set_temp_user_data <- FALSE
      retreive_clim_data_site()[[8]]
    }
    
  })
  
  
  #------------------------------------------------------------
  #--------Checks if upload data is not null-------------
  #-----------------------------------------------------------
  file_data_precipitation <- reactive({
    !is.null(input$upload_precipitation_fwd)
  })
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the precipitation from either user or ISIMIP data-------------
  #----------------------------------------------------------------------------------
  
  data_precipitation <- reactive({
    
    if(file_data_precipitation()){
      
      print("####################################################")
      print("####################################################")
      print("The user has input precipitation data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      values$set_prec_user_data <- TRUE
      
      file <- input$upload_precipitation_fwd
      ext <- tools::file_ext(file$datapath)
      
      if (ext=="csv"){
        y <- read.csv(file$datapath, header = FALSE)
      } else if (ext=="txt"){
        y <- read.table(file$datapath,sep=",")
      }else(warning("The file should be in a csv or txt format"))
      
      upload_user_prec_data_site(y)
      
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input precipitation data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      
      values$set_prec_user_data <- FALSE
      retreive_clim_data_site()[[5]]
    }
    
  })
  
  #------------------------------------------------------------
  #--------Checks if upload data is not null-------------
  #-----------------------------------------------------------
  file_data_potevap <- reactive({
    !is.null(input$upload_potevap_fwd)
  })
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the potevap from either user or ISIMIP data-------------
  #----------------------------------------------------------------------------------
  data_potevap <- reactive({
    
    if(file_data_potevap()){
      
      print("####################################################")
      print("####################################################")
      print("The user has input potevap data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      values$set_potevap_user_data <- TRUE
      
      file <- input$upload_potevap_fwd
      ext <- tools::file_ext(file$datapath)
      
      if (ext=="csv"){
        y <- read.csv(file$datapath, header = FALSE)
      } else if (ext=="txt"){
        y <- read.table(file$datapath,sep=",")
      }else(warning("The file should be in a csv or txt format"))
      
      upload_user_potevap_data_site(y)
      
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input potevap data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      
      values$set_potevap_user_data <- FALSE
      retreive_clim_data_site()[[2]]
    }
    
  })
  
  #------------------------------------------------------------
  #--------Checks if upload data is not null-------------
  #-----------------------------------------------------------
  file_data_vswc <- reactive({
    !is.null(input$upload_vswc_fwd)
  })
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the vswc from either user or ISIMIP data-------------
  #----------------------------------------------------------------------------------
  data_vswc <- reactive({
    
    if(file_data_vswc()){
      
      print("####################################################")
      print("####################################################")
      print("The user has input vswc data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      values$set_vswc_user_data <- TRUE
      
      file <- input$upload_vswc_fwd
      ext <- tools::file_ext(file$datapath)
      
      if (ext=="csv"){
        y <- read.csv(file$datapath, header = FALSE)
      } else if (ext=="txt"){
        y <- read.table(file$datapath,sep=",")
      }else(warning("The file should be in a csv or txt format"))
      
      upload_user_vswc_data_site(y)
      
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input vswc data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      
      values$set_vswc_user_data <- FALSE
      retreive_clim_data_site()[[11]]
    }
    
  })
  
  
  #------------------------------------------------------------
  #--------Checks if input data is not NA-------------
  #-----------------------------------------------------------
  user_potevap_spinup <- reactive({
    #print(input$Potevap_sp)
    !is.na(input$Potevap_sp)
  })
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the SPINUP potevap from either user or ISIMIP data---
  #----------------------------------------------------------------------------------
  data_potevap_spinup <- reactive({
    print("user_potevap_spinup()")
    print(user_potevap_spinup())
    if(user_potevap_spinup()){
      
      print("####################################################")
      print("####################################################")
      print("The user has input potevap SPINUP data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      upload_user_spinup_potevap_data_site()
      
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input potevap data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      #print(retreive_clim_data_site()[[1]])
      retreive_clim_data_site()[[1]]
    }
    
  })
  
  
  #------------------------------------------------------------
  #--------Checks if input data is not NA-------------
  #-----------------------------------------------------------
  user_prec_spinup <- reactive({
    !is.na(input$Prec_sp)
  })
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the SPINUP precip from either user or ISIMIP data---
  #----------------------------------------------------------------------------------
  data_prec_spinup <- reactive({
    
    if(user_prec_spinup()){
      
      print("####################################################")
      print("####################################################")
      print("The user has input prec SPINUP data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      upload_user_spinup_prec_data_site()
      
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input prec data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      
      retreive_clim_data_site()[[4]]
    }
    
  })
  
  
  #------------------------------------------------------------
  #--------Checks if input data is not NA-------------
  #-----------------------------------------------------------
  user_temp_spinup <- reactive({
    !is.na(input$Temp_sp)
  })
  
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the SPINUP temperature from either user or ISIMIP data---
  #----------------------------------------------------------------------------------
  data_temp_spinup <- reactive({
    
    if(user_temp_spinup()){
      
      print("####################################################")
      print("####################################################")
      print("The user has input temp SPINUP data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      upload_user_spinup_temp_data_site()
      
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input temp data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      
      retreive_clim_data_site()[[7]]
    }
    
  })
  
  #------------------------------------------------------------
  #--------Checks if input data is not NA-------------
  #-----------------------------------------------------------
  user_vswc_spinup <- reactive({
    !is.na(input$Vswc_sp)
  })
  
  
  #----------------------------------------------------------------------------------
  #--------Function to retrieve the SPINUP vswc from either user or ISIMIP data---
  #----------------------------------------------------------------------------------
  
  data_vswc_spinup <- reactive({
    
    if(user_vswc_spinup()){
      
      print("####################################################")
      print("####################################################")
      print("The user has input vswc SPINUP data, so we're using their data")
      print("####################################################")
      print("####################################################")
      
      upload_user_spinup_vswc_data_site()
      
    }else{
      print("####################################################")
      print("####################################################")
      print("The user has NOT input vswc data, so we're using ISIMIP")
      print("####################################################")
      print("####################################################")
      
      retreive_clim_data_site()[[10]]
    }
    
  })
  
  
  #-----------------------------
  #--------Mortality rate ---
  #--------------------------------
  observeEvent(input$Mort_rate,{
    updateNumericInput(session, inputId = "Mort_rate", min =0)
    iv1 <- InputValidator$new()
    iv1$add_rule("Mort_rate", sv_between(0, 100))
    iv1$enable()
  })
  
  #-----------------------------
  #--------Harvest rate ---
  #--------------------------------
  observeEvent(input$Harv_rate,{
    updateNumericInput(session, inputId = "Harv_rate")
    iv1 <- InputValidator$new()
    iv1$add_rule("Harv_rate", sv_between(0, 100))
    iv1$enable()
  })
  
  #--------------------------------------------------------------
  #--------Sets harvest rate to 0 if mortality rate is 0 ---
  #-----------------------------------------------------------------
  observeEvent(input$Mort_rate,{
    if((!is.na(input$Mort_rate) & input$Mort_rate==0)){
      updateNumericInput(session, inputId = "Harv_rate",value=0)
    }
  })
  
  
  #/////////////////////////////////
  # Generate plots of the data ----
  #////////////////////////////////
  
  
  #--------------------------------------------------------------------------------
  #Function to set up user input climate data for model simulations
  #---------------------------------------------------------------------------------
  
  #--------------------------------------------------------------------------------
  #potevap
  #---------------------------------------------------------------------------------
  upload_user_potevap_data_site<-function(file_user){
    
    print("upload_user_potevap_data_site")
    req(dates_in())
    initial_date = dates_in()
    
    #--
    #req(data_potevap())
    
    #file_potevap = data_potevap()
    file_potevap = file_user
    colnames(file_potevap)="Potevap"
    
    #simulation_length<-min(simulation_length(),length(file_potevap$Potevap)/12)
    
    dates_file_month= seq(initial_date, by = "month", length.out = length(file_potevap$Potevap))
    
    #---------------------------------------------------
    #potevap
    Potevap_month = data.frame("Date"=dates_file_month,"Potevap"=file_potevap)
    #Select intial date
    #Potevap_month = subset(Potevap_month,Potevap_month$Date>=initial_date)
    #Calculate number of years data
    diff_years <- as.numeric(format(range(dates_file_month)[2], "%Y")) - as.numeric(format(range(dates_file_month)[1], "%Y"))+1
    #Simulation length becomes the minimum between the user selected simu length and the data range
    duration_simulations = min(simulation_length(),diff_years)
    
    #Select number of years
    Potevap_month = Potevap_month[1:(duration_simulations*12),]
    
    return(Potevap_month)
  }
  #--------------------------------------------------------------------------------
  #temp
  #---------------------------------------------------------------------------------
  
  upload_user_temp_data_site<-function(file_user){
    
    print("upload_user_temp_data_site")
    req(dates_in())
    initial_date = dates_in()
    
    #file_temperature = data_temperature()
    file_temperature = file_user
    colnames(file_temperature)="Temp"
    
    dates_file_day = seq(initial_date, by = "day", length.out = length(file_temperature$Temp))
    
    #Temp
    Temp_day = data.frame("Date"=dates_file_day,"Temp"=file_temperature)
    #Select intial date
    #Temp_day = subset(Temp_day,Temp_day$Date>=initial_date)
    
    #Calculate number of years data
    diff_years <- as.numeric(format(range(dates_file_day)[2], "%Y")) - as.numeric(format(range(dates_file_day)[1], "%Y"))+1
    #Simulation length becomes the minimum between the user selected simu length and the data range
    duration_simulations = min(simulation_length(),diff_years)
    
    #Select number of years
    Temp_day = Temp_day[1:(duration_simulations*365),]  
    
    
    return(Temp_day)
  }
  
  #--------------------------------------------------------------------------------
  #prec
  #---------------------------------------------------------------------------------
  upload_user_prec_data_site<-function(file_user){
    
    print("upload_user_prec_data_site")
    req(dates_in())
    initial_date = dates_in()
    
    #--
    
    #file_precipitation = data_precipitation()
    file_precipitation = file_user
    colnames(file_precipitation)="Precip" 
    
    dates_file_day = seq(initial_date, by = "day", length.out = length(file_precipitation$Precip))
    
    #Precip
    Precip_day = data.frame("Date"=dates_file_day,"Precip"=file_precipitation)
    #Select intial date
    #Precip_day = subset(Precip_day,Precip_day$Date>=initial_date)
    
    #Calculate number of years data
    diff_years <- as.numeric(format(range(dates_file_day)[2], "%Y")) - as.numeric(format(range(dates_file_day)[1], "%Y"))+1
    #Simulation length becomes the minimum between the user selected simu length and the data range
    duration_simulations = min(simulation_length(),diff_years)
    
    #Select number of years
    Precip_day = Precip_day[1:(duration_simulations*365),]  
    
    return(Precip_day)
  }
  
  #--------------------------------------------------------------------------------
  #vswc
  #---------------------------------------------------------------------------------
  upload_user_vswc_data_site<-function(file_user){
    
    print("upload_user_vswc_data_site")
    req(dates_in())
    initial_date = dates_in()
    
    
    #file_vswc = data_vswc()
    file_vswc = file_user
    colnames(file_vswc)="Vswc"      
    
    dates_file_day = seq(initial_date, by = "day", length.out = length(file_vswc$Vswc))
    
    #Vswc
    Vswc_day = data.frame("Date"=dates_file_day,"Vswc"=file_vswc)
    #Select intial date
    #Vswc_day = subset(Vswc_day,Vswc_day$Date>=initial_date)
    
    #Calculate number of years data
    diff_years <- as.numeric(format(range(dates_file_day)[2], "%Y")) - as.numeric(format(range(dates_file_day)[1], "%Y"))+1
    #Simulation length becomes the minimum between the user selected simu length and the data range
    duration_simulations = min(simulation_length(),diff_years)
    
    #Select number of years
    Vswc_day = Vswc_day[1:(duration_simulations*365),] 
    
    
    return(Vswc_day)
  }
  
  #--------------------------------------------------------------------------------
  #Functions to create SPINUP data from user inputdata
  #---------------------------------------------------------------------------------

  spinup_lenght = 500 #in case of user input
  
  #--------------------------------------------------------------------------------
  #potevap
  #---------------------------------------------------------------------------------
  upload_user_spinup_potevap_data_site<-function(){
    
    print("upload_user_spinup_potevap_data_site")
    req(dates_in())
    initial_date = dates_in()
    
    
    spinup_year = as.numeric(format(initial_date, "%Y"))-spinup_lenght
    spinup_initial_date = as.Date(paste0(as.character(spinup_year),"-01-01"))
    
    dates_file_month= seq(spinup_initial_date, by = "month", length.out = spinup_lenght*12)
    
    
    ###############
    #SPINUP
    ###############
    
    #Potevap
    Potevap_month_spinup_user<-data.frame(Date = dates_file_month, 
                                          Potevap = rep(input$Potevap_sp,length(dates_file_month)))
    
    
    
    return(Potevap_month_spinup_user)
  }
  
  #--------------------------------------------------------------------------------
  #temperature
  #---------------------------------------------------------------------------------
  upload_user_spinup_temp_data_site<-function(){
    
    print("upload_user_spinup_temp_data_site")
    req(dates_in())
    initial_date = dates_in()
    
    spinup_year = as.numeric(format(initial_date, "%Y"))-spinup_lenght
    spinup_initial_date = as.Date(paste0(as.character(spinup_year),"-01-01"))
    
    dates_file_day = seq(spinup_initial_date, by = "day", length.out = spinup_lenght*365.25)
    
    
    #Temperature
    Temp_day_spinup_user<-data.frame(Date = dates_file_day, 
                                     Temp = rep(input$Temp_sp,length(dates_file_day)))
    
    
    return(Temp_day_spinup_user)
  }
  
  #--------------------------------------------------------------------------------
  #precipitation
  #---------------------------------------------------------------------------------
  
  upload_user_spinup_prec_data_site<-function(){
    
    print("upload_user_spinup_prec_data_site")
    req(dates_in())
    initial_date = dates_in()
    
    
    spinup_year = as.numeric(format(initial_date, "%Y"))-spinup_lenght
    spinup_initial_date = as.Date(paste0(as.character(spinup_year),"-01-01"))
    
    
    dates_file_day = seq(spinup_initial_date, by = "day", length.out = spinup_lenght*365.25)
    
    
    #Precipitation
    Prec_day_spinup_user<-data.frame(Date = dates_file_day, 
                                     Precip = rep(input$Prec_sp,length(dates_file_day)))
    
    
    
    return(Prec_day_spinup_user)
  }
  
  #--------------------------------------------------------------------------------
  #vswc
  #---------------------------------------------------------------------------------
  
  upload_user_spinup_vswc_data_site<-function(){
    
    print("upload_user_spinup_vswc_data_site")
    req(dates_in())
    initial_date = dates_in()
    
    spinup_year = as.numeric(format(initial_date, "%Y"))-spinup_lenght
    spinup_initial_date = as.Date(paste0(as.character(spinup_year),"-01-01"))
    
    
    dates_file_day = seq(spinup_initial_date, by = "day", length.out = spinup_lenght*365.25)
    
    
    #Vswc
    Vswc_day_spinup_user<-data.frame(Date = dates_file_day, 
                                     Vswc = rep(input$Vswc_sp,length(dates_file_day)))
    
    
    
    return(Vswc_day_spinup_user)
  }
  
  
  #--------------------------------------------------------------------------------
  ##This function retrieves ALL the climate data from ISIMIP for the selected site
  #---------------------------------------------------------------------------------
  retreive_clim_data_site<-function(){
    ptm <- proc.time()
    
    
    #---------------------
    #Define forward simulation length [years]
    #as number of data/time_step
    #####################
    # simulation_length=input$simulationlength
    #if(simulation_length>80){simulation_length=80}
    #Set max simulation length to 80
    simulation_length<-min(simulation_length(),80)
    
    #####################
    
    #Choose how many plots to draw in the same figure (num of rows, num of columns)
    #par(mfrow=c(2,1),oma=c(2, 0, 0, 5))
    #---------------------
    ###########################
    #Read forcing climate data
    ###########################
    
    #ISIMIP data
    #----------------
    #Potential evapotransp data
    #----------------
    #Potevapotranspiration RCP26
    #----------------------------------------------------
    #Define lon-lat values to retrieve, based on user input
    lon_index_potevap_rcp26 <- which.min(abs(lon_potevap_rcp26 - coordsLON()))
    lat_index_potevap_rcp26 <- which.min(abs(lat_potevap_rcp26 - coordsLAT()))
    #----------------------------------------------------
    #Retrieve spinup (rcp26)
    #----------------------------------------------------
    #Select variable values from 2006 to 2020 for spinup
    time_index_spinup_potevap_rcp26 <- which(format(potevap_time_rcp26, "%Y-%m-%d") >= "2006-01-01" &
                                               format(potevap_time_rcp26, "%Y-%m-%d") <= "2020-12-31")
    #For X timesteps
    potevap_sel_lonlat_timestep_rcp26_spinup = potevap_rcp26[lon_index_potevap_rcp26, lat_index_potevap_rcp26, time_index_spinup_potevap_rcp26]
    
    #Convert variable from kg m-2 s-1 to mm/month
    potevap_sel_lonlat_timestep_rcp26_spinup<-potevap_sel_lonlat_timestep_rcp26_spinup*(60*60*24)*rep(c(31,28,31,30,31,30,31,31,30,31,30,31),length(potevap_sel_lonlat_timestep_rcp26_spinup)/12)
    
    #Select time series for spinup
    potevap_time_sel_spinup_rcp26 = potevap_time_plot_rcp26[time_index_spinup_potevap_rcp26]
    potevap_time_sel_plot_spinup_rcp26 = as.Date.character(format(potevap_time_sel_spinup_rcp26, "%Y-%m-01"))
    
    Potevap_month_spinup_rcp26 = data.frame("Date"=potevap_time_sel_plot_spinup_rcp26,"Potevap"=potevap_sel_lonlat_timestep_rcp26_spinup)
    
    #----------------------------------------------------
    #Retrieve forward (rcp26)
    #----------------------------------------------------
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_potevap_rcp26 <- which(format(potevap_time_rcp26, "%Y-%m-%d") >= "2020-01-01"
                                          & format(potevap_time_rcp26, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    potevap_sel_lonlat_timestep_rcp26_fwd = potevap_rcp26[lon_index_potevap_rcp26, lat_index_potevap_rcp26, time_index_fwd_potevap_rcp26]
    
    #Convert variable from kg m-2 s-1 to mm/month
    potevap_sel_lonlat_timestep_rcp26_fwd<-potevap_sel_lonlat_timestep_rcp26_fwd*(60*60*24)*rep(c(31,28,31,30,31,30,31,31,30,31,30,31),length(potevap_sel_lonlat_timestep_rcp26_fwd)/12)
    
    
    #Select time series for fwd
    potevap_time_sel_fwd_rcp26 = potevap_time_plot_rcp26[time_index_fwd_potevap_rcp26]
    potevap_time_sel_plot_fwd_rcp26 = as.Date.character(format(potevap_time_sel_fwd_rcp26, "%Y-%m-01"))
    
    Potevap_month_fwd_rcp26 = data.frame("Date"=potevap_time_sel_plot_fwd_rcp26,"Potevap"=potevap_sel_lonlat_timestep_rcp26_fwd)
    
    
    #Select intial date
    Potevap_month_fwd_rcp26_sel = subset(Potevap_month_fwd_rcp26,Potevap_month_fwd_rcp26$Date>=dates_in())
    #Select number of years
    max_length = min(simulation_length*12,length(Potevap_month_fwd_rcp26_sel$Date))
    Potevap_month_fwd_rcp26_sel = Potevap_month_fwd_rcp26_sel[1:max_length,]
    
    # print("Potevap")
    # print(head(Potevap_month_fwd_rcp26_sel))
    # print(tail(Potevap_month_fwd_rcp26_sel))
    print("POTEVAP 26 OK")
    #----------------------------------------------------
    #Potevapotranspiration RCP60
    #----------------------------------------------------
    #Define lon-lat values to retrieve, based on user input
    lon_index_potevap_rcp60 <- which.min(abs(lon_potevap_rcp60 - coordsLON()))
    lat_index_potevap_rcp60 <- which.min(abs(lat_potevap_rcp60 - coordsLAT()))
    #----------------------------------------------------
    #No spinup for (rcp60)
    #----------------------------------------------------
    #----------------------------------------------------
    #Retrieve forward (rcp60)
    #----------------------------------------------------
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_potevap_rcp60 <- which(format(potevap_time_rcp60, "%Y-%m-%d") >= "2020-01-01"
                                          & format(potevap_time_rcp60, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    potevap_sel_lonlat_timestep_rcp60_fwd = potevap_rcp60[lon_index_potevap_rcp60, lat_index_potevap_rcp60, time_index_fwd_potevap_rcp60]
    
    #Convert variable from kg m-2 s-1 to mm/month
    potevap_sel_lonlat_timestep_rcp60_fwd<-potevap_sel_lonlat_timestep_rcp60_fwd*(60*60*24)*rep(c(31,28,31,30,31,30,31,31,30,31,30,31),length(potevap_sel_lonlat_timestep_rcp60_fwd)/12)
    
    
    #Select time series for fwd
    potevap_time_sel_fwd_rcp60 = potevap_time_plot_rcp60[time_index_fwd_potevap_rcp60]
    potevap_time_sel_plot_fwd_rcp60 = as.Date.character(format(potevap_time_sel_fwd_rcp60, "%Y-%m-01"))
    
    Potevap_month_fwd_rcp60 = data.frame("Date"=potevap_time_sel_plot_fwd_rcp60,"Potevap"=potevap_sel_lonlat_timestep_rcp60_fwd)
    
    #Select intial date
    Potevap_month_fwd_rcp60_sel = subset(Potevap_month_fwd_rcp60,Potevap_month_fwd_rcp60$Date>=dates_in())
    max_length = min(simulation_length*12,length(Potevap_month_fwd_rcp60_sel$Date))
    #Select number of years
    Potevap_month_fwd_rcp60_sel = Potevap_month_fwd_rcp60_sel[1:max_length,]
    
    # print("potevap month")
    # print(tail(Potevap_month_fwd_rcp60,20))
    
    print("POTEVAP 60 OK")
    
    #----------------
    #Temperature data
    #----------------
    #Temperature RCP26
    
    #----------------------------------------------------
    #Define lon-lat values to retrieve, based on user input
    lon_index_temp_rcp26 <- which.min(abs(lon_temp_rcp26 - coordsLON()))
    lat_index_temp_rcp26 <- which.min(abs(lat_temp_rcp26 - coordsLAT()))
    #----------------------------------------------------
    #Retrieve spinup (rcp26)
    #----------------------------------------------------
    #Select variable values from 2006 to 2020 for spinup
    time_index_spinup_temp_rcp26 <- which(format(temp_time_rcp26, "%Y-%m-%d") >= "2006-01-01" &
                                            format(temp_time_rcp26, "%Y-%m-%d") <= "2020-12-31")
    #For X timesteps
    temp_sel_lonlat_timestep_rcp26_spinup = temp_rcp26[lon_index_temp_rcp26, lat_index_temp_rcp26, time_index_spinup_temp_rcp26]
    
    #Convert variable from K to C
    temp_sel_lonlat_timestep_rcp26_spinup<-temp_sel_lonlat_timestep_rcp26_spinup-273.15 #K to C
    
    #Select time series for spinup
    temp_time_sel_spinup_rcp26 = temp_time_plot_rcp26[time_index_spinup_temp_rcp26]
    temp_time_sel_plot_spinup_rcp26 = as.Date.character(format(temp_time_sel_spinup_rcp26, "%Y-%m-01"))
    
    Temp_month_spinup_rcp26 = data.frame("Date"=temp_time_sel_plot_spinup_rcp26,"Temp"=temp_sel_lonlat_timestep_rcp26_spinup)
    
    #----------------------------------------------------
    #Retrieve forward (rcp26)
    #----------------------------------------------------
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_temp_rcp26 <- which(format(temp_time_rcp26, "%Y-%m-%d") >= "2020-01-01" 
                                       & format(temp_time_rcp26, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    temp_sel_lonlat_timestep_rcp26_fwd = temp_rcp26[lon_index_temp_rcp26, lat_index_temp_rcp26, time_index_fwd_temp_rcp26]
    
    #Convert variable from K to C
    temp_sel_lonlat_timestep_rcp26_fwd<-temp_sel_lonlat_timestep_rcp26_fwd-273.15 #K to C
    
    
    #Select time series for fwd
    temp_time_sel_fwd_rcp26 = temp_time_plot_rcp26[time_index_fwd_temp_rcp26]
    temp_time_sel_plot_fwd_rcp26 = as.Date.character(format(temp_time_sel_fwd_rcp26, "%Y-%m-01"))
    
    Temp_month_fwd_rcp26 = data.frame("Date"=temp_time_sel_plot_fwd_rcp26,"Temp"=temp_sel_lonlat_timestep_rcp26_fwd)
    
    
    #Select intial date
    Temp_month_fwd_rcp26_sel = subset(Temp_month_fwd_rcp26,Temp_month_fwd_rcp26$Date>=dates_in())
    max_length = min(simulation_length*12,length(Temp_month_fwd_rcp26_sel$Date))
    #Select number of years
    Temp_month_fwd_rcp26_sel = Temp_month_fwd_rcp26_sel[1:max_length,]
    
    
    
    #----------------------------------------------------
    #Temperature RCP60
    #----------------------------------------------------
    #Define lon-lat values to retrieve, based on user input
    lon_index_temp_rcp60 <- which.min(abs(lon_temp_rcp60 - coordsLON()))
    lat_index_temp_rcp60 <- which.min(abs(lat_temp_rcp60 - coordsLAT()))
    #----------------------------------------------------
    #No spinup for rcp60
    #----------------------------------------------------
    #----------------------------------------------------
    #Retrieve forward (rcp60)
    #----------------------------------------------------
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_temp_rcp60 <- which(format(temp_time_rcp60, "%Y-%m-%d") >= "2020-01-01"
                                       & format(temp_time_rcp60, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    temp_sel_lonlat_timestep_rcp60_fwd = temp_rcp60[lon_index_temp_rcp60, lat_index_temp_rcp60, time_index_fwd_temp_rcp60]
    
    #Convert variable from K to C
    temp_sel_lonlat_timestep_rcp60_fwd<-temp_sel_lonlat_timestep_rcp60_fwd-273.15 #K to C
    
    
    #Select time series for fwd
    temp_time_sel_fwd_rcp60 = temp_time_plot_rcp60[time_index_fwd_temp_rcp60]
    temp_time_sel_plot_fwd_rcp60 = as.Date.character(format(temp_time_sel_fwd_rcp60, "%Y-%m-01"))
    
    Temp_month_fwd_rcp60 = data.frame("Date"=temp_time_sel_plot_fwd_rcp60,"Temp"=temp_sel_lonlat_timestep_rcp60_fwd)
    
    #Select intial date
    Temp_month_fwd_rcp60_sel = subset(Temp_month_fwd_rcp60,Temp_month_fwd_rcp60$Date>=dates_in())
    max_length = min(simulation_length*12,length(Temp_month_fwd_rcp60_sel$Date))
    #Select number of years
    Temp_month_fwd_rcp60_sel = Temp_month_fwd_rcp60_sel[1:max_length,]
    
    
    print("TEMP OK")
    
    #----------------
    #Precipitation data 
    #----------------
    #Precipitation RCP26
    #----------------------------------------------------
    #Define lon-lat values to retrieve, based on user input
    lon_index_prec_rcp26 <- which.min(abs(lon_prec_rcp26 - coordsLON()))
    lat_index_prec_rcp26 <- which.min(abs(lat_prec_rcp26 - coordsLAT()))
    #----------------------------------------------------
    #Retrieve spinup (rcp26)
    #----------------------------------------------------
    #Select variable values from 2006 to 2020 for spinup
    time_index_spinup_prec_rcp26 <- which(format(prec_time_rcp26, "%Y-%m-%d") >= "2006-01-01" &
                                            format(prec_time_rcp26, "%Y-%m-%d") <= "2020-12-31")
    #For X timesteps
    prec_sel_lonlat_timestep_rcp26_spinup = prec_rcp26[lon_index_prec_rcp26, lat_index_prec_rcp26, time_index_spinup_prec_rcp26]
    
    #Convert variable from kg/m2/s (monthly mean) to mm/month
    prec_sel_lonlat_timestep_rcp26_spinup<-prec_sel_lonlat_timestep_rcp26_spinup*(60*60*24)*rep(c(31,28,31,30,31,30,31,31,30,31,30,31),length(prec_sel_lonlat_timestep_rcp26_spinup)/12)
    
    #----------
    #Select time series for spinup
    prec_time_sel_spinup_rcp26 = prec_time_plot_rcp26[time_index_spinup_prec_rcp26]
    #prec_time_sel_plot_spinup_rcp26 = as.Date.character(format(prec_time_sel_spinup_rcp26, "%Y-%m-%d"))
    prec_time_sel_plot_spinup_rcp26 = as.Date.character(format(prec_time_sel_spinup_rcp26, "%Y-%m-01"))
    
    Prec_month_spinup_rcp26 = data.frame("Date"=prec_time_sel_plot_spinup_rcp26,"Precip"=prec_sel_lonlat_timestep_rcp26_spinup)
    
    
    #----------------------------------------------------
    #Retrieve forward (rcp26)
    #----------------------------------------------------
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_prec_rcp26 <- which(format(prec_time_rcp26, "%Y-%m-%d") >= "2020-01-01"
                                       & format(prec_time_rcp26, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    prec_sel_lonlat_timestep_rcp26_fwd = prec_rcp26[lon_index_prec_rcp26, lat_index_prec_rcp26, time_index_fwd_prec_rcp26]
    
    #Convert variable from kg/m2/s (monthly mean) to mm/month
    prec_sel_lonlat_timestep_rcp26_fwd<-prec_sel_lonlat_timestep_rcp26_fwd*(60*60*24)*rep(c(31,28,31,30,31,30,31,31,30,31,30,31),length(prec_sel_lonlat_timestep_rcp26_fwd)/12)
    
    
    #Select time series for fwd
    prec_time_sel_fwd_rcp26 = prec_time_plot_rcp26[time_index_fwd_prec_rcp26]
    prec_time_sel_plot_fwd_rcp26 = as.Date.character(format(prec_time_sel_fwd_rcp26, "%Y-%m-01"))
    
    Prec_month_fwd_rcp26 = data.frame("Date"=prec_time_sel_plot_fwd_rcp26,"Precip"=prec_sel_lonlat_timestep_rcp26_fwd)
    
    
    #Select intial date
    Prec_month_fwd_rcp26_sel = subset(Prec_month_fwd_rcp26,Prec_month_fwd_rcp26$Date>=dates_in())
    max_length = min(simulation_length*12,length(Prec_month_fwd_rcp26_sel$Date))
    #Select number of years
    Prec_month_fwd_rcp26_sel = Prec_month_fwd_rcp26_sel[1:max_length,]
    
    #---------------------------------------------------
    #Precipitation RCP60
    #----------------------------------------------------
    #Define lon-lat values to retrieve, based on user input
    lon_index_prec_rcp60 <- which.min(abs(lon_prec_rcp60 - coordsLON()))
    lat_index_prec_rcp60 <- which.min(abs(lat_prec_rcp60 - coordsLAT()))
    #----------------------------------------------------
    #No spinup for rcp60
    #----------------------------------------------------
    #----------------------------------------------------
    #Retrieve forward (rcp60)
    #----------------------------------------------------
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_prec_rcp60 <- which(format(prec_time_rcp60, "%Y-%m-%d") >= "2020-01-01"
                                       & format(prec_time_rcp60, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    prec_sel_lonlat_timestep_rcp60_fwd = prec_rcp60[lon_index_prec_rcp60, lat_index_prec_rcp60, time_index_fwd_prec_rcp60]
    
    #Convert variable from kg/m2/s (monthly mean) to mm/month
    prec_sel_lonlat_timestep_rcp60_fwd<-prec_sel_lonlat_timestep_rcp60_fwd*(60*60*24)*rep(c(31,28,31,30,31,30,31,31,30,31,30,31),length(prec_sel_lonlat_timestep_rcp60_fwd)/12)
    
    #Select time series for fwd
    prec_time_sel_fwd_rcp60 = prec_time_plot_rcp60[time_index_fwd_prec_rcp60]
    prec_time_sel_plot_fwd_rcp60 = as.Date.character(format(prec_time_sel_fwd_rcp60, "%Y-%m-01"))
    
    Prec_month_fwd_rcp60 = data.frame("Date"=prec_time_sel_plot_fwd_rcp60,"Precip"=prec_sel_lonlat_timestep_rcp60_fwd)
    
    
    #Select intial date
    Prec_month_fwd_rcp60_sel = subset(Prec_month_fwd_rcp60,Prec_month_fwd_rcp60$Date>=dates_in())
    max_length = min(simulation_length*12,length(Prec_month_fwd_rcp60_sel$Date))
    #Select number of years
    Prec_month_fwd_rcp60_sel = Prec_month_fwd_rcp60_sel[1:max_length,]
    
    
    print("PREC OK")
    
    #----------------
    #Soil moisture (top 18 cm)=>(m3/m3)
    #----------------
    
    #Soil moisture RCP26
    
    #----------------------------------------------------
    #Define lon-lat values to retrieve, based on user input
    lon_index_vswc_rcp26 <- which.min(abs(lon_vswc_rcp26 - coordsLON()))
    lat_index_vswc_rcp26 <- which.min(abs(lat_vswc_rcp26 - coordsLAT()))
    #----------------------------------------------------
    #Retrieve spinup (rcp26)
    #----------------------------------------------------
    #Select variable values from 2006 to 2020 for spinup
    time_index_spinup_vswc_rcp26 <- which(format(vswc_time_rcp26, "%Y-%m-%d") >= "2006-01-01" &
                                            format(vswc_time_rcp26, "%Y-%m-%d") <= "2020-12-31")
    #For X timesteps
    vswc_sel_lonlat_timestep_rcp26_spinup = vswc_rcp26[lon_index_vswc_rcp26, lat_index_vswc_rcp26, time_index_spinup_vswc_rcp26]#mm3/mm3 top 18cm
    
    
    #Select time series for spinup
    vswc_time_sel_spinup_rcp26 = vswc_time_plot_rcp26[time_index_spinup_vswc_rcp26]
    vswc_time_sel_plot_spinup_rcp26 = as.Date.character(format(vswc_time_sel_spinup_rcp26, "%Y-%m-01"))
    
    Vswc_month_spinup_rcp26 = data.frame("Date"=vswc_time_sel_plot_spinup_rcp26,"Vswc"=vswc_sel_lonlat_timestep_rcp26_spinup)
    
    #----------------------------------------------------
    #Retrieve forward (rcp26)
    #----------------------------------------------------
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_vswc_rcp26 <- which(format(vswc_time_rcp26, "%Y-%m-%d") >= "2020-01-01"
                                       & format(vswc_time_rcp26, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    vswc_sel_lonlat_timestep_rcp26_fwd = vswc_rcp26[lon_index_vswc_rcp26, lat_index_vswc_rcp26, time_index_fwd_vswc_rcp26] #mm3/mm3 top 18cm
    
    
    #Select time series for fwd
    vswc_time_sel_fwd_rcp26 = vswc_time_plot_rcp26[time_index_fwd_vswc_rcp26]
    vswc_time_sel_plot_fwd_rcp26 = as.Date.character(format(vswc_time_sel_fwd_rcp26, "%Y-%m-01"))
    
    Vswc_month_fwd_rcp26 = data.frame("Date"=vswc_time_sel_plot_fwd_rcp26,"Vswc"=vswc_sel_lonlat_timestep_rcp26_fwd)
    
    
    #Select intial date
    Vswc_month_fwd_rcp26_sel = subset(Vswc_month_fwd_rcp26,Vswc_month_fwd_rcp26$Date>=dates_in())
    max_length = min(simulation_length*12,length(Vswc_month_fwd_rcp26_sel$Date))
    #Select number of years
    Vswc_month_fwd_rcp26_sel = Vswc_month_fwd_rcp26_sel[1:max_length,]
    
    #----------------------------------------------------
    #Soil moisture RCP60
    #----------------------------------------------------
    #Define lon-lat values to retrieve, based on user input
    lon_index_vswc_rcp60 <- which.min(abs(lon_vswc_rcp60 - coordsLON()))
    lat_index_vswc_rcp60 <- which.min(abs(lat_vswc_rcp60 - coordsLAT()))
    #----------------------------------------------------
    #No spinup for (rcp60)
    #----------------------------------------------------
    #----------------------------------------------------
    #Retrieve forward (rcp60)
    #----------------------------------------------------
    #Select variable values from 2020 to 2099 for forward
    time_index_fwd_vswc_rcp60 <- which(format(vswc_time_rcp60, "%Y-%m-%d") >= "2020-01-01"
                                       & format(vswc_time_rcp60, "%Y-%m-%d")< "2100-01-01")
    #For X timesteps
    vswc_sel_lonlat_timestep_rcp60_fwd = vswc_rcp60[lon_index_vswc_rcp60, lat_index_vswc_rcp60, time_index_fwd_vswc_rcp60]#mm3/mm3 top 18cm
    
    
    #Select time series for fwd
    vswc_time_sel_fwd_rcp60 = vswc_time_plot_rcp60[time_index_fwd_vswc_rcp60]
    vswc_time_sel_plot_fwd_rcp60 = as.Date.character(format(vswc_time_sel_fwd_rcp60, "%Y-%m-01"))
    
    Vswc_month_fwd_rcp60 = data.frame("Date"=vswc_time_sel_plot_fwd_rcp60,"Vswc"=vswc_sel_lonlat_timestep_rcp60_fwd)
    
    # print("soilmoist month")
    # print(tail(Vswc_month_fwd_rcp60,20))
    
    
    
    #Select intial date
    Vswc_month_fwd_rcp60_sel = subset(Vswc_month_fwd_rcp60,Vswc_month_fwd_rcp60$Date>=dates_in())
    max_length = min(simulation_length*12,length(Vswc_month_fwd_rcp60_sel$Date))
    #Select number of years
    Vswc_month_fwd_rcp60_sel = Vswc_month_fwd_rcp60_sel[1:max_length,]
    
    
    print("SOIL MOIST OK")
    
    #====
    #Transform monthly to daily data
    #====
    #This function repeats the month's value for each day of the month
    #That way we have daily data, although with a monthly variability
    
    #Spinup
    Temp_day_spinup<-do.call("rbind", lapply(1:nrow(Temp_month_spinup_rcp26), function(i) 
      data.frame(Date = seq(Temp_month_spinup_rcp26$Date[i], 
                            (seq(Temp_month_spinup_rcp26$Date[i],length=2,by="months") - 1)[2], by = "1 days"), 
                 Temp = Temp_month_spinup_rcp26$Temp[i])))
    
    # print("temp day ok")
    # print(tail(Temp_day_spinup))
    
    mean_days_in_months <- mean(c(31,28,31,30,31,30,31,31,30,31,30,31))
    
    Precip_day_spinup<-do.call("rbind", lapply(1:nrow(Prec_month_spinup_rcp26), function(i) 
      data.frame(Date = seq(Prec_month_spinup_rcp26$Date[i], 
                            (seq(Prec_month_spinup_rcp26$Date[i],length=2,by="months") - 1)[2], by = "1 days"), 
                 Precip = Prec_month_spinup_rcp26$Precip[i]/mean_days_in_months)))
    
    Vswc_day_spinup<-do.call("rbind", lapply(1:nrow(Vswc_month_spinup_rcp26), function(i) 
      data.frame(Date = seq(Vswc_month_spinup_rcp26$Date[i], 
                            (seq(Vswc_month_spinup_rcp26$Date[i],length=2,by="months") - 1)[2], by = "1 days"), 
                 Vswc = Vswc_month_spinup_rcp26$Vswc[i])))
    
    
    # print("prec day ok")
    # print(tail(Precip_day_spinup))
    
    #Fwd slected
    
    Temp_day_fwd_rcp26_sel<-do.call("rbind", lapply(1:nrow(Temp_month_fwd_rcp26_sel), function(i) 
      data.frame(Date = seq(Temp_month_fwd_rcp26_sel$Date[i]- day(Temp_month_fwd_rcp26_sel$Date[i]) +1,#from day 1
                            Temp_month_fwd_rcp26_sel$Date[i]- day(Temp_month_fwd_rcp26_sel$Date[i]) +as.numeric(days_in_month(Temp_month_fwd_rcp26_sel$Date[i])),#to last day month
                            by = "1 days"), 
                 Temp = Temp_month_fwd_rcp26_sel$Temp[i])))
    # print("Temp")
    # print(head(Temp_day_fwd_rcp26_sel))
    # print(tail(Temp_day_fwd_rcp26_sel))
    
    Precip_day_fwd_rcp26_sel<-do.call("rbind", lapply(1:nrow(Prec_month_fwd_rcp26_sel), function(i) 
      data.frame(Date = seq(Prec_month_fwd_rcp26_sel$Date[i]- day(Prec_month_fwd_rcp26_sel$Date[i]) +1,#from day 1
                            Prec_month_fwd_rcp26_sel$Date[i]- day(Prec_month_fwd_rcp26_sel$Date[i]) +as.numeric(days_in_month(Prec_month_fwd_rcp26_sel$Date[i])),#to last day month
                            by = "1 days"), 
                 Precip = Prec_month_fwd_rcp26_sel$Precip[i]/mean_days_in_months)))
    
    Vswc_day_fwd_rcp26_sel<-do.call("rbind", lapply(1:nrow(Vswc_month_fwd_rcp26_sel), function(i) 
      data.frame(Date = seq(Vswc_month_fwd_rcp26_sel$Date[i]- day(Vswc_month_fwd_rcp26_sel$Date[i]) +1,#from day 1
                            Vswc_month_fwd_rcp26_sel$Date[i]- day(Vswc_month_fwd_rcp26_sel$Date[i]) +as.numeric(days_in_month(Vswc_month_fwd_rcp26_sel$Date[i])),#to last day month
                            by = "1 days"), 
                 Vswc = Vswc_month_fwd_rcp26_sel$Vswc[i])))
    
    
    #Fwd climate change scenario RCP26
    
    Temp_day_fwd_rcp26<-do.call("rbind", lapply(1:nrow(Temp_month_fwd_rcp26), function(i)
      data.frame(Date = seq(Temp_month_fwd_rcp26$Date[i]- day(Temp_month_fwd_rcp26$Date[i]) +1,#from day 1
                            Temp_month_fwd_rcp26$Date[i]- day(Temp_month_fwd_rcp26$Date[i]) +as.numeric(days_in_month(Temp_month_fwd_rcp26$Date[i])),#to last day month
                            by = "1 days"),
                 Temp = Temp_month_fwd_rcp26$Temp[i])))
    
    
    # print("temp day ")
    # print(tail(Temp_day_fwd_rcp26,20))
    
    Precip_day_fwd_rcp26<-do.call("rbind", lapply(1:nrow(Prec_month_fwd_rcp26), function(i) 
      data.frame(Date = seq(Prec_month_fwd_rcp26$Date[i]- day(Prec_month_fwd_rcp26$Date[i]) +1,#from day 1
                            Prec_month_fwd_rcp26$Date[i]- day(Prec_month_fwd_rcp26$Date[i]) +as.numeric(days_in_month(Prec_month_fwd_rcp26$Date[i])),#to last day month
                            by = "1 days"), 
                 Precip = Prec_month_fwd_rcp26$Precip[i]/mean_days_in_months)))
    # print("precip day ")
    # print(tail(Precip_day_fwd_rcp26,20))
    
    Vswc_day_fwd_rcp26<-do.call("rbind", lapply(1:nrow(Vswc_month_fwd_rcp26), function(i) 
      data.frame(Date = seq(Vswc_month_fwd_rcp26$Date[i]- day(Vswc_month_fwd_rcp26$Date[i]) +1,#from day 1
                            Vswc_month_fwd_rcp26$Date[i]- day(Vswc_month_fwd_rcp26$Date[i]) +as.numeric(days_in_month(Vswc_month_fwd_rcp26$Date[i])),#to last day month
                            by = "1 days"), 
                 Vswc = Vswc_month_fwd_rcp26$Vswc[i])))
    
    #Fwd climate change scenario RCP60
    
    Temp_day_fwd_rcp60<-do.call("rbind", lapply(1:nrow(Temp_month_fwd_rcp60), function(i) 
      data.frame(Date = seq(Temp_month_fwd_rcp60$Date[i]- day(Temp_month_fwd_rcp60$Date[i]) +1,#from day 1
                            Temp_month_fwd_rcp60$Date[i]- day(Temp_month_fwd_rcp60$Date[i]) +as.numeric(days_in_month(Temp_month_fwd_rcp60$Date[i])),#to last day month
                            by = "1 days"), 
                 Temp = Temp_month_fwd_rcp60$Temp[i])))
    
    Precip_day_fwd_rcp60<-do.call("rbind", lapply(1:nrow(Prec_month_fwd_rcp60), function(i) 
      data.frame(Date = seq(Prec_month_fwd_rcp60$Date[i]- day(Prec_month_fwd_rcp60$Date[i]) +1,#from day 1
                            Prec_month_fwd_rcp60$Date[i]- day(Prec_month_fwd_rcp60$Date[i]) +as.numeric(days_in_month(Prec_month_fwd_rcp60$Date[i])),#to last day month
                            by = "1 days"), 
                 Precip = Prec_month_fwd_rcp60$Precip[i]/mean_days_in_months)))
    
    Vswc_day_fwd_rcp60<-do.call("rbind", lapply(1:nrow(Vswc_month_fwd_rcp60), function(i) 
      data.frame(Date = seq(Vswc_month_fwd_rcp60$Date[i]- day(Vswc_month_fwd_rcp60$Date[i]) +1,#from day 1
                            Vswc_month_fwd_rcp60$Date[i]- day(Vswc_month_fwd_rcp60$Date[i]) +as.numeric(days_in_month(Vswc_month_fwd_rcp60$Date[i])),#to last day month
                            by = "1 days"), 
                 Vswc = Vswc_month_fwd_rcp60$Vswc[i])))
    
    
    #Select by simulation length
    
    Temp_day_fwd_rcp60_sel<-do.call("rbind", lapply(1:nrow(Temp_month_fwd_rcp60_sel), function(i) 
      data.frame(Date = seq(Temp_month_fwd_rcp60_sel$Date[i]- day(Temp_month_fwd_rcp60_sel$Date[i]) +1,#from day 1
                            Temp_month_fwd_rcp60_sel$Date[i]- day(Temp_month_fwd_rcp60_sel$Date[i]) +as.numeric(days_in_month(Temp_month_fwd_rcp60_sel$Date[i])),#to last day month
                            by = "1 days"), 
                 Temp = Temp_month_fwd_rcp60_sel$Temp[i])))
    
    
    Precip_day_fwd_rcp60_sel<-do.call("rbind", lapply(1:nrow(Prec_month_fwd_rcp60_sel), function(i) 
      data.frame(Date = seq(Prec_month_fwd_rcp60_sel$Date[i]- day(Prec_month_fwd_rcp60_sel$Date[i]) +1,#from day 1
                            Prec_month_fwd_rcp60_sel$Date[i]- day(Prec_month_fwd_rcp60_sel$Date[i]) +as.numeric(days_in_month(Prec_month_fwd_rcp60_sel$Date[i])),#to last day month
                            by = "1 days"), 
                 Precip = Prec_month_fwd_rcp60_sel$Precip[i]/mean_days_in_months)))
    
    Vswc_day_fwd_rcp60_sel<-do.call("rbind", lapply(1:nrow(Vswc_month_fwd_rcp60_sel), function(i) 
      data.frame(Date = seq(Vswc_month_fwd_rcp60_sel$Date[i]- day(Vswc_month_fwd_rcp60_sel$Date[i]) +1,#from day 1
                            Vswc_month_fwd_rcp60_sel$Date[i]- day(Vswc_month_fwd_rcp60_sel$Date[i]) +as.numeric(days_in_month(Vswc_month_fwd_rcp60_sel$Date[i])),#to last day month
                            by = "1 days"), 
                 Vswc = Vswc_month_fwd_rcp60_sel$Vswc[i])))
    
    print("data read ok")
    
    print("Elapsed time (minutes)")
    print((proc.time() - ptm)/60) #minutes
    
    
    ###############################
    #Return a list of 20 elements
    ##############################
    #spinup - fwd rcp2.6 - fwd rcp 6.0
    #1-3: potential evapotranspiration (mm/month) - sel
    #4-6: precipitation (mm), daily -sel
    #7-9: temperature (C), daily -sel
    #10-12: soil moisture (mm3/mm3) top 10cm, daily -sel
    
    #13-14: potential evapotranspiration (mm/month) - all
    #15-16: precipitation (mm), daily -all
    #17-18: temperature (C), daily -all
    #19-20: soil moisture (mm3/mm3) top 10cm, daily -all
    
    return(list(Potevap_month_spinup_rcp26,Potevap_month_fwd_rcp26_sel,Potevap_month_fwd_rcp60_sel,
                Precip_day_spinup,Precip_day_fwd_rcp26_sel,Precip_day_fwd_rcp60_sel,
                Temp_day_spinup,Temp_day_fwd_rcp26_sel,Temp_day_fwd_rcp60_sel,
                Vswc_day_spinup,Vswc_day_fwd_rcp26_sel,Vswc_day_fwd_rcp60_sel,
                Potevap_month_fwd_rcp26,Potevap_month_fwd_rcp60,
                Precip_day_fwd_rcp26,Precip_day_fwd_rcp60,
                Temp_day_fwd_rcp26,Temp_day_fwd_rcp60,
                Vswc_day_fwd_rcp26,Vswc_day_fwd_rcp60))
  }
  
  
  #  observeEvent(input$button,{
  
  #--------------------------------------------------------------------------------
  ##Creates the plots to visualize climate data
  #---------------------------------------------------------------------------------
  
  observe({
    output$climate_plots<-renderPlot({
      #User climate data, or if not input, ISIMIP RCP2.6
      print("enter climate_plots")
      
      #Fwd
      Potevap_month = data_potevap()
      Temp_day = data_temperature()
      Precip_day = data_precipitation()
      Vswc_day = data_vswc()
      
      #If these variables are set to true, it means that the user has input climate data
      #Then we plot both the user and ISIMIP data together
      if((values$set_temp_user_data & 
          values$set_prec_user_data &
          values$set_potevap_user_data &
          values$set_vswc_user_data)){
        
        print("plotting both user and isimip data together")
        
        ##################
        ### USERD DATA ##
        ##################
        #Convert daily variables to monthly for plots
        years_months <- format(Temp_day$Date, "%Y-%m")
        Temp_month <- aggregate(Temp_day$Temp ~ years_months, data = Temp_day , mean)
        colnames(Temp_month)=c("Date","Temp")
        Temp_month$Date<-as.Date(paste(Temp_month$Date, "01", sep = "-"))
        
        years_months <- format(Precip_day$Date, "%Y-%m")
        Precip_month <- aggregate(Precip_day$Precip ~ years_months, data = Precip_day , sum)
        colnames(Precip_month)=c("Date","Precip")
        Precip_month$Date<-as.Date(paste(Precip_month$Date, "01", sep = "-"))
        
        years_months <- format(Vswc_day$Date, "%Y-%m")
        Vswc_month <- aggregate(Vswc_day$Vswc ~ years_months, data = Vswc_day , mean)
        colnames(Vswc_month)=c("Date","Vswc")
        Vswc_month$Date<-as.Date(paste(Vswc_month$Date, "01", sep = "-"))
        
        
        ##################
        ### ISIMIP DATA ##
        ##################
        
        #Run retreive_clim_data_site function to retrieve climate data for site
        data_clim = retreive_clim_data_site()
        #Potevap_month_spinup_rcp26=data_clim[[1]]
        Potevap_month_fwd_rcp26_sel=data_clim[[2]]
        Potevap_month_fwd_rcp60_sel=data_clim[[3]]
        #Precip_day_spinup=data_clim[[4]]
        Precip_day_fwd_rcp26_sel=data_clim[[5]]
        Precip_day_fwd_rcp60_sel=data_clim[[6]]
        #Temp_day_spinup=data_clim[[7]]
        Temp_day_fwd_rcp26_sel=data_clim[[8]]
        Temp_day_fwd_rcp60_sel=data_clim[[9]]
        #Vswc_day_spinup=data_clim[[10]]
        Vswc_day_fwd_rcp26_sel=data_clim[[11]]
        Vswc_day_fwd_rcp60_sel=data_clim[[12]]
        
        #Convert daily variables to monthly for plots
        years_months <- format(Temp_day_fwd_rcp26_sel$Date, "%Y-%m")
        Temp_month_fwd_rcp26_sel <- aggregate(Temp_day_fwd_rcp26_sel$Temp ~ years_months, data = Temp_day_fwd_rcp26_sel , mean)
        colnames(Temp_month_fwd_rcp26_sel)=c("Date","Temp")
        Temp_month_fwd_rcp26_sel$Date<-as.Date(paste(Temp_month_fwd_rcp26_sel$Date, "01", sep = "-"))
        
        Temp_month_fwd_rcp60_sel <- aggregate(Temp_day_fwd_rcp60_sel$Temp ~ years_months, data = Temp_day_fwd_rcp60_sel , mean)
        colnames(Temp_month_fwd_rcp60_sel)=c("Date","Temp")
        Temp_month_fwd_rcp60_sel$Date<-as.Date(paste(Temp_month_fwd_rcp60_sel$Date, "01", sep = "-"))
        
        years_months <- format(Precip_day_fwd_rcp26_sel$Date, "%Y-%m")
        Precip_month_fwd_rcp26_sel <- aggregate(Precip_day_fwd_rcp26_sel$Precip ~ years_months, data = Precip_day_fwd_rcp26_sel , sum)
        colnames(Precip_month_fwd_rcp26_sel)=c("Date","Precip")
        Precip_month_fwd_rcp26_sel$Date<-as.Date(paste(Precip_month_fwd_rcp26_sel$Date, "01", sep = "-"))
        
        Precip_month_fwd_rcp60_sel <- aggregate(Precip_day_fwd_rcp60_sel$Precip ~ years_months, data = Precip_day_fwd_rcp60_sel , sum)
        colnames(Precip_month_fwd_rcp60_sel)=c("Date","Precip")
        Precip_month_fwd_rcp60_sel$Date<-as.Date(paste(Precip_month_fwd_rcp60_sel$Date, "01", sep = "-"))
        
        years_months <- format(Vswc_day_fwd_rcp26_sel$Date, "%Y-%m")
        Vswc_month_fwd_rcp26_sel <- aggregate(Vswc_day_fwd_rcp26_sel$Vswc ~ years_months, data = Vswc_day_fwd_rcp26_sel , mean)
        colnames(Vswc_month_fwd_rcp26_sel)=c("Date","Vswc")
        Vswc_month_fwd_rcp26_sel$Date<-as.Date(paste(Vswc_month_fwd_rcp26_sel$Date, "01", sep = "-"))
        
        Vswc_month_fwd_rcp60_sel <- aggregate(Vswc_day_fwd_rcp60_sel$Vswc ~ years_months, data = Vswc_day_fwd_rcp60_sel , mean)
        colnames(Vswc_month_fwd_rcp60_sel)=c("Date","Vswc")
        Vswc_month_fwd_rcp60_sel$Date<-as.Date(paste(Vswc_month_fwd_rcp60_sel$Date, "01", sep = "-"))
        
        
        min_temp <- min(as.numeric(cbind(Temp_month_fwd_rcp26_sel$Temp,
                                         Temp_month_fwd_rcp60_sel$Temp,
                                         Temp_month$Temp)))
        max_temp <- max(as.numeric(cbind(Temp_month_fwd_rcp26_sel$Temp,
                                         Temp_month_fwd_rcp60_sel$Temp,
                                         Temp_month$Temp)))
        
        min_prec <- min(as.numeric(cbind(Precip_month_fwd_rcp26_sel$Precip,
                                         Precip_month_fwd_rcp60_sel$Precip,
                                         Precip_month$Precip)))
        max_prec <- max(as.numeric(cbind(Precip_month_fwd_rcp26_sel$Precip,
                                         Precip_month_fwd_rcp60_sel$Precip,
                                         Precip_month$Precip)))
        
        min_potevap <- min(as.numeric(cbind(Potevap_month_fwd_rcp26_sel$Potevap,
                                            Potevap_month_fwd_rcp60_sel$Potevap,
                                            Potevap_month$Potevap)))
        max_potevap <- max(as.numeric(cbind(Potevap_month_fwd_rcp26_sel$Potevap,
                                            Potevap_month_fwd_rcp60_sel$Potevap,
                                            Potevap_month$Potevap)))
        
        min_vswc <- min(as.numeric(cbind(Vswc_month_fwd_rcp26_sel$Vswc,
                                         Vswc_month_fwd_rcp60_sel$Vswc,
                                         Vswc_month$Vswc)))
        max_vswc <- max(as.numeric(cbind(Vswc_month_fwd_rcp26_sel$Vswc,
                                         Vswc_month_fwd_rcp60_sel$Vswc,
                                         Vswc_month$Vswc)))
        #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
        #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
        
        
        ####################
        #PLOT CLIMATE
        ###################
        par(mfrow=c(2,2),mar=c(4, 5, 5, 3))
        #par(mfrow=c(4,1),mar=c(4, 5, 4, 1),cex = 1.5)
        cex_clim = 1.5
        #Plot climate variables
        
        
        plot(Temp_month_fwd_rcp26_sel$Date,Temp_month_fwd_rcp26_sel$Temp,type="l", 
             ylab="˚C",xlab="Years",col="blue",cex.lab=cex_clim,cex.axis=cex_clim,cex.main=cex_clim,
             main="Temperature",ylim=c(min_temp-2,max_temp+2))
        lines(Temp_month_fwd_rcp60_sel$Date,Temp_month_fwd_rcp60_sel$Temp,col="red")
        lines(Temp_month$Date,Temp_month$Temp,col="black")
        
        legend("bottomright", c("RCP 2.6", "RCP 6.0","User"),
               lty=1,lwd=c(3,3), col=c("blue","red","black"), cex=1.2,inset=c(-0.05,1.3),xpd=TRUE, horiz=TRUE,bty="n")
        
        plot(Precip_month_fwd_rcp26_sel$Date,Precip_month_fwd_rcp26_sel$Precip,type="l", 
             ylab=expression("mm"~{month}^{-1}~" "),xlab="Years",col="blue",cex.lab=cex_clim,cex.axis=cex_clim,cex.main=cex_clim,
             main="Precipitation",ylim=c(min_prec-2,max_prec+2))
        lines(Precip_month_fwd_rcp60_sel$Date,Precip_month_fwd_rcp60_sel$Precip,col="red")
        lines(Precip_month$Date,Precip_month$Precip,col="black")
        
        plot(Vswc_month_fwd_rcp26_sel$Date,Vswc_month_fwd_rcp26_sel$Vswc,type="l", 
             ylab=expression(" "~{mm}^{3}~{mm}^{-3}~" "),xlab="Years",col="blue",cex.lab=cex_clim,cex.axis=cex_clim,cex.main=cex_clim,
             main="Soil moisture",ylim=c(min_vswc-0.02,max_vswc+0.02))
        lines(Vswc_month_fwd_rcp60_sel$Date,Vswc_month_fwd_rcp60_sel$Vswc,col="red")
        lines(Vswc_month$Date,Vswc_month$Vswc,type="l",col="black")
        
        
        plot(Potevap_month_fwd_rcp26_sel$Date,Potevap_month_fwd_rcp26_sel$Potevap,type="l", 
             ylab=expression("mm"~{month}^{-1}~" "),xlab="Years",col="blue",cex.lab=cex_clim,cex.axis=cex_clim,cex.main=cex_clim,
             main="Potential evapotranspiration",ylim=c(min_potevap-2,max_potevap+2))
        lines(Potevap_month_fwd_rcp60_sel$Date,Potevap_month_fwd_rcp60_sel$Potevap,col="red")  
        lines(Potevap_month$Date,Potevap_month$Potevap,col="black")
        
        
      }else{
        
        print("NO user climate data, only ISIMIP")
        
        #Run retreive_clim_data_site function to retrieve climate data for site
        data_clim = retreive_clim_data_site()
        #Potevap_month_spinup_rcp26=data_clim[[1]]
        Potevap_month_fwd_rcp26_sel=data_clim[[2]]
        Potevap_month_fwd_rcp60_sel=data_clim[[3]]
        #Precip_day_spinup=data_clim[[4]]
        Precip_day_fwd_rcp26_sel=data_clim[[5]]
        Precip_day_fwd_rcp60_sel=data_clim[[6]]
        #Temp_day_spinup=data_clim[[7]]
        Temp_day_fwd_rcp26_sel=data_clim[[8]]
        Temp_day_fwd_rcp60_sel=data_clim[[9]]
        #Vswc_day_spinup=data_clim[[10]]
        Vswc_day_fwd_rcp26_sel=data_clim[[11]]
        Vswc_day_fwd_rcp60_sel=data_clim[[12]]
        
        
        #Convert daily variables to monthly for plots
        years_months <- format(Temp_day_fwd_rcp26_sel$Date, "%Y-%m")
        Temp_month_fwd_rcp26_sel <- aggregate(Temp_day_fwd_rcp26_sel$Temp ~ years_months, data = Temp_day_fwd_rcp26_sel , mean)
        colnames(Temp_month_fwd_rcp26_sel)=c("Date","Temp")
        Temp_month_fwd_rcp26_sel$Date<-as.Date(paste(Temp_month_fwd_rcp26_sel$Date, "01", sep = "-"))
        
        Temp_month_fwd_rcp60_sel <- aggregate(Temp_day_fwd_rcp60_sel$Temp ~ years_months, data = Temp_day_fwd_rcp60_sel , mean)
        colnames(Temp_month_fwd_rcp60_sel)=c("Date","Temp")
        Temp_month_fwd_rcp60_sel$Date<-as.Date(paste(Temp_month_fwd_rcp60_sel$Date, "01", sep = "-"))
        
        years_months <- format(Precip_day_fwd_rcp26_sel$Date, "%Y-%m")
        Precip_month_fwd_rcp26_sel <- aggregate(Precip_day_fwd_rcp26_sel$Precip ~ years_months, data = Precip_day_fwd_rcp26_sel , sum)
        colnames(Precip_month_fwd_rcp26_sel)=c("Date","Precip")
        Precip_month_fwd_rcp26_sel$Date<-as.Date(paste(Precip_month_fwd_rcp26_sel$Date, "01", sep = "-"))
        
        Precip_month_fwd_rcp60_sel <- aggregate(Precip_day_fwd_rcp60_sel$Precip ~ years_months, data = Precip_day_fwd_rcp60_sel , sum)
        colnames(Precip_month_fwd_rcp60_sel)=c("Date","Precip")
        Precip_month_fwd_rcp60_sel$Date<-as.Date(paste(Precip_month_fwd_rcp60_sel$Date, "01", sep = "-"))
        
        years_months <- format(Vswc_day_fwd_rcp26_sel$Date, "%Y-%m")
        Vswc_month_fwd_rcp26_sel <- aggregate(Vswc_day_fwd_rcp26_sel$Vswc ~ years_months, data = Vswc_day_fwd_rcp26_sel , mean)
        colnames(Vswc_month_fwd_rcp26_sel)=c("Date","Vswc")
        Vswc_month_fwd_rcp26_sel$Date<-as.Date(paste(Vswc_month_fwd_rcp26_sel$Date, "01", sep = "-"))
        
        Vswc_month_fwd_rcp60_sel <- aggregate(Vswc_day_fwd_rcp60_sel$Vswc ~ years_months, data = Vswc_day_fwd_rcp60_sel , mean)
        colnames(Vswc_month_fwd_rcp60_sel)=c("Date","Vswc")
        Vswc_month_fwd_rcp60_sel$Date<-as.Date(paste(Vswc_month_fwd_rcp60_sel$Date, "01", sep = "-"))
        
        #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
        #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
        
        
        ####################
        #PLOT CLIMATE
        ###################
        par(mfrow=c(2,2),mar=c(4, 5, 5, 3))
        #par(mfrow=c(4,1),mar=c(4, 5, 4, 1),cex = 1.5)
        cex_clim = 1.5
        #Plot climate variables
        
        plot(Temp_month_fwd_rcp26_sel$Date,Temp_month_fwd_rcp26_sel$Temp,type="l", 
             ylab="˚C",xlab="Years",col="blue",cex.lab=cex_clim,cex.axis=cex_clim,cex.main=cex_clim,
             main="Temperature")
        lines(Temp_month_fwd_rcp60_sel$Date,Temp_month_fwd_rcp60_sel$Temp,col="red")
        
        legend("bottomright", c("RCP 2.6", "RCP 6.0"),
               lty=1,lwd=c(3,3), col=c("blue","red"), cex=1.5,inset=c(0,1.3),xpd=TRUE, horiz=TRUE,bty="n")
        
        plot(Precip_month_fwd_rcp26_sel$Date,Precip_month_fwd_rcp26_sel$Precip,type="l", 
             ylab=expression("mm"~{month}^{-1}~" "),xlab="Years",col="blue",cex.lab=cex_clim,cex.axis=cex_clim,cex.main=cex_clim,
             main="Precipitation")
        lines(Precip_month_fwd_rcp60_sel$Date,Precip_month_fwd_rcp60_sel$Precip,col="red")
        
        plot(Vswc_month_fwd_rcp26_sel$Date,Vswc_month_fwd_rcp26_sel$Vswc,type="l", 
             ylab=expression(" "~{mm}^{3}~{mm}^{-3}~" "),xlab="Years",col="blue",cex.lab=cex_clim,cex.axis=cex_clim,cex.main=cex_clim,
             main="Soil moisture")
        lines(Vswc_month_fwd_rcp60_sel$Date,Vswc_month_fwd_rcp60_sel$Vswc,col="red")
        
        plot(Potevap_month_fwd_rcp26_sel$Date,Potevap_month_fwd_rcp26_sel$Potevap,type="l", 
             ylab=expression("mm"~{month}^{-1}~" "),xlab="Years",col="blue",cex.lab=cex_clim,cex.axis=cex_clim,cex.main=cex_clim,
             main="Potential evapotranspiration")
        lines(Potevap_month_fwd_rcp60_sel$Date,Potevap_month_fwd_rcp60_sel$Potevap,col="red")   
      }
      
    })
    
  })
  
  #------------------------------------------------------------------------------------------------
  ##Creates the plots to visualize litter data (only ISIMIP), for RCP2.6 RCP6.0 CC // fixed and varying LU
  #--------------------------------------------------------------------------------------------------
  
  observe({
    output$plot_Cin<- renderPlot({
      
      print("Entering plot_Cin")
      #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
      #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
      print("Retrieving C input data for site")
      
      
      retreive_Cinput_ag_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_ag)
      retreive_Cinput_bg_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_bg)
      retreive_Cinput_rcp26_fix = retreive_Cinput_ag_rcp26_fix+retreive_Cinput_bg_rcp26_fix
      retreive_Cinput_rcp26_fix_mean = mean(retreive_Cinput_rcp26_fix)
      
      retreive_Cinput_ag_rcp60_fix = retreive_Cinput(lon_litter_rcp60,lat_litter_rcp60,litter_time_rcp60,litter_rcp60_ag)
      retreive_Cinput_bg_rcp60_fix = retreive_Cinput(lon_litter_rcp60,lat_litter_rcp60,litter_time_rcp60,litter_rcp60_bg)
      retreive_Cinput_rcp60_fix=retreive_Cinput_ag_rcp60_fix+retreive_Cinput_bg_rcp60_fix
      retreive_Cinput_rcp60_fix_mean = mean(retreive_Cinput_rcp60_fix)
      
      retreive_Cinput_ag_rcp26_var = retreive_Cinput(lon_litter_LU_rcp26,lat_litter_LU_rcp26,litter_LU_time_rcp26,litter_LU_rcp26_ag)
      retreive_Cinput_bg_rcp26_var = retreive_Cinput(lon_litter_LU_rcp26,lat_litter_LU_rcp26,litter_LU_time_rcp26,litter_LU_rcp26_bg)
      retreive_Cinput_rcp26_var = retreive_Cinput_ag_rcp26_var+retreive_Cinput_bg_rcp26_var
      retreive_Cinput_rcp26_var_mean = mean(retreive_Cinput_rcp26_var)
      
      retreive_Cinput_ag_rcp60_var = retreive_Cinput(lon_litter_LU_rcp60,lat_litter_LU_rcp60,litter_LU_time_rcp60,litter_LU_rcp60_ag)
      retreive_Cinput_bg_rcp60_var = retreive_Cinput(lon_litter_LU_rcp60,lat_litter_LU_rcp60,litter_LU_time_rcp60,litter_LU_rcp60_bg)
      retreive_Cinput_rcp60_var = retreive_Cinput_ag_rcp60_var+retreive_Cinput_bg_rcp60_var
      retreive_Cinput_rcp60_var_mean = mean(retreive_Cinput_rcp60_var)
      
      #Put together
      Cinput_tog_fix = c(retreive_Cinput_rcp26_fix_mean,retreive_Cinput_rcp60_fix_mean)
      Cinput_tog_var = c(retreive_Cinput_rcp26_var_mean, retreive_Cinput_rcp60_var_mean)
      
      #Plot bars
      
      par(mfrow=c(1,2),mar=c(4, 5, 4, 3),cex = 1.5)
      
      barplot(Cinput_tog_fix,
              main = "Fixed land-use",
              xlab = "Climate scenarios",
              ylab = expression("Average C input (MgC"~{ha}^{-1}~{yr}^{-1}~")"),
              names.arg = c("RCP 2.6", "RCP 6.0"),
              col = c("darkred","blue"),
              horiz = FALSE)
      
      barplot(Cinput_tog_var,
              main = "Varying land-use",
              xlab = "Climate scenarios",
              ylab = expression("Average C input (MgC"~{ha}^{-1}~{yr}^{-1}~")"),
              names.arg = c("RCP 2.6 ", "RCP 6.0"),
              col = c("darkred","blue"),
              horiz = FALSE)
      
      
      
    })
  })
  
  
  #--------------------------------------------------------------------------------
  ##Creates the plots to visualize evolution of C input in land management scenarios
  #---------------------------------------------------------------------------------
  observe({
    output$plot_Cin_LM<- renderPlot({
      req(input$Mort_rate)
      req(input$Harv_rate)
      print("Entering plot_Cin_LM")
      #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
      #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
      #Plot the C input evolution in the land management scenarios
      #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
      
      print("Retrieving C input for control scenario")
      # retreive_Cinput_ag_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_ag)
      # retreive_Cinput_bg_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_bg)
      retreive_Cinput_ag_rcp26_fix=data_Cinput_ag()
      retreive_Cinput_bg_rcp26_fix=data_Cinput_bg()
      retreive_Cinput_rcp26_fix = retreive_Cinput_ag_rcp26_fix+retreive_Cinput_bg_rcp26_fix
      
      print("Retrieving C input for disturbance scenario")
      #Disturbance scenario
      #retreive_Cbiomass_rcp26 = retreive_Cbiomass(lon_veg_clitter_rcp26,lat_veg_clitter_rcp26,veg_clitter_time_rcp26,veg_clitter_rcp26,input$year_disturbance)
      retreive_Cbiomass_rcp26_AG<-input$AG_biomass
      retreive_Cbiomass_rcp26_BG<-input$BG_biomass
      Cinput_disturbance_vec<- calculate_Cin_mortality_event(retreive_Cinput_ag_rcp26_fix,retreive_Cinput_bg_rcp26_fix,
                                                             retreive_Cbiomass_rcp26_AG,retreive_Cbiomass_rcp26_BG, input$Mort_rate,input$Harv_rate)
      Cinput_disturbance_ag<-Cinput_disturbance_vec[[1]]
      Cinput_disturbance_bg<-Cinput_disturbance_vec[[2]]
      Cinput_disturbance<-Cinput_disturbance_ag+Cinput_disturbance_bg
      
      cex_clim=1.2
      
      par(mfrow=c(1,1),mar=c(4, 5, 4, 3),cex = cex_clim)
      #years_plot <- seq(1:length(retreive_Cinput_rcp26_fix))
      yearly_dates <- seq(dates_in(), by = "years", length.out = simulation_length()) 
      
      min_cin <- min(as.numeric(cbind(retreive_Cinput_rcp26_fix,
                                      Cinput_disturbance)))
      max_cin <- max(as.numeric(cbind(retreive_Cinput_rcp26_fix,
                                      Cinput_disturbance)))
      
      
      #Control scenario
      plot(yearly_dates,retreive_Cinput_rcp26_fix,type="l",col="black",lwd=3,
           xlab="Years",ylab=expression("C input (MgC"~{ha}^{-1}~{yr}^{-1}~")"),
           ylim=c(min_cin,max_cin),main="Land management",
           cex.lab=cex_clim,cex.axis=cex_clim,cex.main=1.5)
      lines(yearly_dates,Cinput_disturbance,type="l",col="darkgreen",lwd=3)
      
      legend("bottomleft", c("Control","Disturbance"),
             lty=1,lwd=c(3,3), col=c("black","darkgreen"), cex=cex_clim,inset=c(0,0.97),
             xpd=TRUE, horiz=TRUE,bty="n")
      
      # Add text at x = text_at
      distance_text = 2
      begin_arrow = yearly_dates[input$year_disturbance]
      end_arrow = as.Date(paste0(as.character(as.numeric(format(begin_arrow, "%Y"))+distance_text),"-01-01"))
      
      text(end_arrow, Cinput_disturbance[input$year_disturbance]-0.3, 
           "Mortality event", pos = 4, col = "black",cex=cex_clim)
      abline(v = begin_arrow, lty = 2, col = "black")
      
      # Add an arrow pointing to the text
      arrows(end_arrow, Cinput_disturbance[input$year_disturbance]-0.3,
             begin_arrow, Cinput_disturbance[input$year_disturbance], angle = 30, length = 0.1, col = "black")
      
      print("plot_Cin_LM done")
      
    })
  })
  
  
  
  #  })#ObserveEvent
  #--------------------------------------------------------------------------------
  ## Creates table to visualize soil data
  #---------------------------------------------------------------------------------
  
  output$table_soildata <- renderTable({
    sand_in <- round(100-input$clay-input$silt,2)
    
    soil_data_in = data.frame("Soil thickness" = c("cm",input$soilthick), "Clay"=c("%",input$clay),
                              "Silt"=c("%",input$silt),
                              "Sand"=c("%",sand_in),
                              "Bulk density"=c("Mg/m3",input$bulkdensity),
                              "C:N ratio" = c(" ",input$CNratio),
                              "Initial SOC stock" = c("MgC/ha",input$SOC),
                              check.names=FALSE)
    
  })
  
  #--------------------------------------------------------------------------------
  ##Creates table to visualize database references
  #---------------------------------------------------------------------------------
  output$data_ref_list<- renderDT({
    
    database_list = data.frame("Database" = c("ISIMIP","TRY","LUCAS","GML"),
                               "Reference" = c(urlISIMIP_table,urlTRY_table,urlLUCAS.1_table,urlGML_table),
                               "Variables" = c("Climate, Biomass","Litter quality","Soil","Atmospheric CH4"),
                               check.names=FALSE)
    
    datatable(database_list, options = list(searching = FALSE,
                                            paging = FALSE,
                                            language = list(info = "")),
              rownames = FALSE, escape = FALSE)
    
  })
  

  
  #####################
  # REactives for simulations
  
  #--------------------------------------------------------------------------------
  ##Creates clay slider for fixed land use simulations
  #---------------------------------------------------------------------------------
  output$clay_slider <- renderUI({
    req(input$clay)
    req(input$silt)
    sliderInput("clay_slider", h5(style="text-align: left;","Clay (%)"), min = 0, max = 100-input$silt, value = input$clay)
    
  })
  #--------------------------------------------------------------------------------
  ##Creates clay slider for vary land use simulations
  #---------------------------------------------------------------------------------
  output$clay_slider2 <- renderUI({
    req(input$clay_slider)
    req(input$silt)
    sliderInput("clay_slider2", h5(style="text-align: left;","Clay (%)"), min = 0, max = 100-input$silt, value = input$clay_slider)
    
  })
  
  #--------------------------------------------------------------------------------
  ##Creates mortality rate slider
  #---------------------------------------------------------------------------------
  
  output$MR_slider <- renderUI({
    req(input$Mort_rate)
    sliderInput("MR_slider", h5(style="text-align: left;","Mortality rate (%)"), min = 0, max = 100, value = input$Mort_rate)
    
  })
  
  #--------------------------------------------------------------------------------
  ##Creates harvest rate slider
  #---------------------------------------------------------------------------------
  output$HR_slider <- renderUI({
    req(input$Harv_rate)
    sliderInput("HR_slider", h5(style="text-align: left;","Harvest rate (%)"), min = 0, max = 100, value = input$Harv_rate)
    
  })
  
  
  #--------------------------------------------------------------------------------
  ##Sets harvest rate slider to 0 if  MR slider is 0
  #---------------------------------------------------------------------------------
  observeEvent(input$MR_slider,{
    
    if((!is.na(input$MR_slider) & input$MR_slider==0)){
      updateSliderInput(session, inputId = "HR_slider",value=0)
    }
    
  })
  
  
  #--------------------------------------------------------------------------------
  ##Function to extract output data from simulations
  #---------------------------------------------------------------------------------
  get_data_from_multimodel_run<-function(multimodel_run,timestep_plot){
    #multimodel_run = test_mm
    #timestep_plot = t_fwd_col
    
    ROTHCRCPi = multimodel_run[[1]]
    ROTHC_CRCPi = as.data.frame(cbind(timestep_plot,ROTHCRCPi[[2]]))
    ROTHC_CO2RCPi = as.data.frame(cbind(timestep_plot,ROTHCRCPi[[1]]))
    colnames(ROTHC_CRCPi)<-c("Time","DPM", "RPM", "BIO", "HUM", "IOM")
    colnames(ROTHC_CO2RCPi)<-c("Time","DPM", "RPM", "BIO", "HUM", "IOM")
    
    ICBMRCPi = multimodel_run[[2]]
    ICBM_CRCPi = as.data.frame(cbind(timestep_plot,ICBMRCPi[[2]]))
    ICBM_CO2RCPi = as.data.frame(cbind(timestep_plot,ICBMRCPi[[1]]))
    colnames(ICBM_CRCPi)<-c("Time","Young", "Old")
    colnames(ICBM_CO2RCPi)<-c("Time","Young", "Old")
    
    CENTRCPi = multimodel_run[[3]]
    CENT_CRCPi = as.data.frame(cbind(timestep_plot,CENTRCPi[[2]]))
    CENT_CO2RCPi = as.data.frame(cbind(timestep_plot,CENTRCPi[[1]]))
    colnames(CENT_CRCPi)<-c("Time","SurSTRUC", "SurMET", "BelSTRUC",
                            "BelMET", "Active", "Slow", "Passive")
    colnames(CENT_CO2RCPi)<-c("Time","SurSTRUC", "SurMET", "BelSTRUC",
                              "BelMET", "Active", "Slow", "Passive")
    
    YASSORCPi = multimodel_run[[4]]
    YASSO_CRCPi = as.data.frame(cbind(timestep_plot,YASSORCPi[[2]]))
    YASSO_CO2RCPi = as.data.frame(cbind(timestep_plot,YASSORCPi[[1]]))
    colnames(YASSO_CRCPi)<-c("Time","A", "W", "E", "N", "H")
    colnames(YASSO_CO2RCPi)<-c("Time","A", "W", "E", "N", "H")
    
    YASSO20RCPi = multimodel_run[[5]]
    YASSO20_CRCPi = as.data.frame(cbind(timestep_plot,YASSO20RCPi[[2]]))
    YASSO20_CO2RCPi = as.data.frame(cbind(timestep_plot,YASSO20RCPi[[1]]))
    colnames(YASSO20_CRCPi)<-c("Time","Active", "Stable")
    colnames(YASSO20_CO2RCPi)<-c("Time","Active", "Stable")
    
    
    SGRCPi = multimodel_run[[6]]
    SG_CO2RCPi = as.data.frame(cbind(timestep_plot,SGRCPi$CO2_flux))
    SG_CH4RCPi = as.data.frame(cbind(timestep_plot,SGRCPi$CH4_flux))
    SG_N2ORCPi = as.data.frame(cbind(timestep_plot,SGRCPi$N2O_flux))
    colnames(SG_CO2RCPi)<-c("Time","CO2")
    colnames(SG_CH4RCPi)<-c("Time","CH4")
    colnames(SG_N2ORCPi)<-c("Time","N2O")
    
    
    #----------------------------
    #Calculate total SOC stock RCPi
    totC_Roth_C_RCPi<-rowSums(ROTHC_CRCPi[,2:dim(ROTHC_CRCPi)[2]])
    totC_ICBM_C_RCPi<-rowSums(ICBM_CRCPi[,2:dim(ICBM_CRCPi)[2]])
    totC_Century_C_RCPi<-rowSums(CENT_CRCPi[,2:dim(CENT_CRCPi)[2]])
    totC_Yasso07_C_RCPi<-rowSums(YASSO_CRCPi[,2:dim(YASSO_CRCPi)[2]])
    totC_YASSO20_C_RCPi<-rowSums(YASSO20_CRCPi[,2:dim(YASSO20_CRCPi)[2]])
    
    #Calculate multi-model mean of total SOC stock
    mmmean_totCRCPi <- rowMeans(cbind(totC_Roth_C_RCPi,totC_ICBM_C_RCPi,
                                      totC_Century_C_RCPi,totC_Yasso07_C_RCPi,
                                      totC_YASSO20_C_RCPi))
    
    minCRCPi <- min(as.numeric(cbind(totC_Roth_C_RCPi,totC_ICBM_C_RCPi,
                                     totC_Century_C_RCPi,totC_Yasso07_C_RCPi,
                                     totC_YASSO20_C_RCPi)))
    
    maxCRCPi <- max(as.numeric(cbind(totC_Roth_C_RCPi,totC_ICBM_C_RCPi,
                                     totC_Century_C_RCPi,totC_Yasso07_C_RCPi,
                                     totC_YASSO20_C_RCPi)))
    
    #------------------------
    #Calculate total fluxes RCPi
    totF_Roth_C_RCPi<-rowSums(ROTHC_CO2RCPi[,2:dim(ROTHC_CO2RCPi)[2]])
    totF_ICBM_C_RCPi<-rowSums(ICBM_CO2RCPi[,2:dim(ICBM_CO2RCPi)[2]])
    totF_Century_C_RCPi<-rowSums(CENT_CO2RCPi[,2:dim(CENT_CO2RCPi)[2]])
    totF_Yasso07_C_RCPi<-rowSums(YASSO_CO2RCPi[,2:dim(YASSO_CO2RCPi)[2]])
    totF_YASSO20_C_RCPi<-rowSums(YASSO20_CO2RCPi[,2:dim(YASSO20_CO2RCPi)[2]])
    
    totF_SG_C_RCPi<-SG_CO2RCPi[,2:dim(SG_CO2RCPi)[2]]
    totCH4_SG_C_RCPi<-SG_CH4RCPi[,2:dim(SG_CH4RCPi)[2]]
    totN2O_SG_C_RCPi<-SG_N2ORCPi[,2:dim(SG_N2ORCPi)[2]]
    
    #Calculate multi-model mean of total SOC stock
    mmmean_totFRCPi <- rowMeans(cbind(totF_Roth_C_RCPi,totF_ICBM_C_RCPi,
                                      totF_Century_C_RCPi,totF_Yasso07_C_RCPi,
                                      totF_YASSO20_C_RCPi,
                                      totF_SG_C_RCPi))
    
    minFRCPi <- min(as.numeric(cbind(totF_Roth_C_RCPi,totF_ICBM_C_RCPi,
                                     totF_Century_C_RCPi,totF_Yasso07_C_RCPi,
                                     totF_YASSO20_C_RCPi,
                                     totF_SG_C_RCPi)))
    
    maxFRCPi <- max(as.numeric(cbind(totF_Roth_C_RCPi,totF_ICBM_C_RCPi,
                                     totF_Century_C_RCPi,totF_Yasso07_C_RCPi,
                                     totF_YASSO20_C_RCPi,
                                     totF_SG_C_RCPi)))
    
    return(list(minCRCPi,maxCRCPi,mmmean_totCRCPi,
                totC_Roth_C_RCPi,totC_ICBM_C_RCPi,totC_Century_C_RCPi,totC_Yasso07_C_RCPi,totC_YASSO20_C_RCPi,
                minFRCPi,maxFRCPi,mmmean_totFRCPi,
                totF_Roth_C_RCPi,totF_ICBM_C_RCPi,totF_Century_C_RCPi,totF_Yasso07_C_RCPi,totF_YASSO20_C_RCPi,
                totF_SG_C_RCPi,totCH4_SG_C_RCPi,totN2O_SG_C_RCPi))
  }
  
  
  #--------------------------------------------------------------------------------
  #Function to create dataframes for downlaod from simulation outputs
  #---------------------------------------------------------------------------------
  create_dataframe_for_download<-function(t_fwd,
                                          output_of_get_data_from_multimodel_run_scenario1,
                                          output_of_get_data_from_multimodel_run_scenario2,
                                          scenario1_name,
                                          scenario2_name){
    
    #t_fwd = number of the year (or date)
    #scenario1_name and scenario2_name = strings with the name of the scenarios (RCP2.6 and RCP6.0 for climate scenarios 
    #and Control and Disturbance for land management scenario)
    
    #mmmean_totCRCP26
    #totC_Roth_C_RCP26
    #totC_ICBM_C_RCP26
    #totC_Century_C_RCP26
    #totC_Yasso07_C_RCP26
    #totC_Yasso20_C_RCP26
    #mmmean_totCRCP60
    #totC_Roth_C_RCP60
    #totC_ICBM_C_RCP60
    #totC_Century_C_RCP60
    #totC_Yasso07_C_RCP60
    #totC_Yasso20_C_RCP60
    #
    #mmmean_totFRCP26
    #totF_Roth_C_RCP26
    #totF_ICBM_C_RCP26
    #totF_Century_C_RCP26
    #totF_Yasso07_C_RCP26
    #totF_Yasso20_C_RCP26
    #totF_SG_C_RCP26_df
    #totCH4_SG_C_RCP26_df
    #totN2O_SG_C_RCP26_df
    
    #mmmean_totFRCP60
    #totF_Roth_C_RCP60
    #totF_ICBM_C_RCP60
    #totF_Century_C_RCP60
    #totF_Yasso07_C_RCP60
    #totF_Yasso20_C_RCP60
    #totF_SG_C_RCP60_df
    #totCH4_SG_C_RCP60_df
    #totN2O_SG_C_RCP60_df
    
    ##scenario1
    mmmean_totCscenario1=output_of_get_data_from_multimodel_run_scenario1[[3]]
    totC_Roth_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[4]]
    totC_ICBM_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[5]]
    totC_Century_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[6]]
    totC_Yasso07_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[7]]
    totC_Yasso20_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[8]]
    
    mmmean_totFscenario1=output_of_get_data_from_multimodel_run_scenario1[[11]]
    totF_Roth_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[12]]
    totF_ICBM_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[13]]
    
    totF_Century_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[14]]
    totF_Yasso07_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[15]]
    totF_Yasso20_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[16]]
    
    totF_SG_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[17]]
    totCH4_SG_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[18]]
    totN2O_SG_C_scenario1=output_of_get_data_from_multimodel_run_scenario1[[19]]
    
    ##scenario2
    mmmean_totCscenario2=output_of_get_data_from_multimodel_run_scenario2[[3]]
    totC_Roth_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[4]]
    totC_ICBM_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[5]]
    totC_Century_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[6]]
    totC_Yasso07_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[7]]
    totC_Yasso20_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[8]]
    
    mmmean_totFscenario2=output_of_get_data_from_multimodel_run_scenario2[[11]]
    totF_Roth_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[12]]
    totF_ICBM_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[13]]
    
    totF_Century_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[14]]
    totF_Yasso07_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[15]]
    totF_Yasso20_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[16]]
    
    totF_SG_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[17]]
    totCH4_SG_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[18]]
    totN2O_SG_C_scenario2=output_of_get_data_from_multimodel_run_scenario2[[19]]
    
    
    ###########
    #Build dataframe for download
    length_simulations<-length(mmmean_totCscenario1)
    #RCP26
    mmmean_totCscenario1<-data.frame(Value=mmmean_totCscenario1,Year=t_fwd,
                                     Model = rep("Multi-model mean",length_simulations),
                                     Variable = rep("SOC",length_simulations),
                                     Scenario = rep(scenario1_name,length_simulations),
                                     Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    
    totC_Roth_C_scenario1_df<-data.frame(Value=totC_Roth_C_scenario1,Year=t_fwd,
                                         Model = rep("Roth-C",length_simulations),
                                         Variable = rep("SOC",length_simulations),
                                         Scenario = rep(scenario1_name,length_simulations),
                                         Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    totC_ICBM_C_scenario1_df<-data.frame(Value=totC_ICBM_C_scenario1,Year=t_fwd,
                                         Model = rep("ICBM",length_simulations),
                                         Variable = rep("SOC",length_simulations),
                                         Scenario = rep(scenario1_name,length_simulations),
                                         Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    
    totC_Century_C_scenario1_df<-data.frame(Value=totC_Century_C_scenario1,Year=t_fwd,
                                            Model = rep("Century",length_simulations),
                                            Variable = rep("SOC",length_simulations),
                                            Scenario = rep(scenario1_name,length_simulations),
                                            Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    totC_Yasso07_C_scenario1_df<-data.frame(Value=totC_Yasso07_C_scenario1,Year=t_fwd,
                                            Model = rep("Yasso07",length_simulations),
                                            Variable = rep("SOC",length_simulations),
                                            Scenario = rep(scenario1_name,length_simulations),
                                            Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    
    totC_Yasso20_C_scenario1_df<-data.frame(Value=totC_Yasso20_C_scenario1,Year=t_fwd,
                                            Model = rep("Yasso20",length_simulations),
                                            Variable = rep("SOC",length_simulations),
                                            Scenario = rep(scenario1_name,length_simulations),
                                            Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    
    mmmean_totCscenario2_df<-data.frame(Value=mmmean_totCscenario2,Year=t_fwd,
                                        Model = rep("Multi-model mean",length_simulations),
                                        Variable = rep("SOC",length_simulations),
                                        Scenario = rep(scenario2_name,length_simulations),
                                        Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    #scenario2
    
    totC_Roth_C_scenario2_df<-data.frame(Value=totC_Roth_C_scenario2,Year=t_fwd,
                                         Model = rep("Roth-C",length_simulations),
                                         Variable = rep("SOC",length_simulations),
                                         Scenario = rep(scenario2_name,length_simulations),
                                         Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    totC_ICBM_C_scenario2_df<-data.frame(Value=totC_ICBM_C_scenario2,Year=t_fwd,
                                         Model = rep("ICBM",length_simulations),
                                         Variable = rep("SOC",length_simulations),
                                         Scenario = rep(scenario2_name,length_simulations),
                                         Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    totC_Century_C_scenario2_df<-data.frame(Value=totC_Century_C_scenario2,Year=t_fwd,
                                            Model = rep("Century",length_simulations),
                                            Variable = rep("SOC",length_simulations),
                                            Scenario = rep(scenario2_name,length_simulations),
                                            Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    totC_Yasso07_C_scenario2_df<-data.frame(Value=totC_Yasso07_C_scenario2,Year=t_fwd,
                                            Model = rep("Yasso07",length_simulations),
                                            Variable = rep("SOC",length_simulations),
                                            Scenario = rep(scenario2_name,length_simulations),
                                            Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    totC_Yasso20_C_scenario2_df<-data.frame(Value=totC_Yasso20_C_scenario2,Year=t_fwd,
                                            Model = rep("Yasso20",length_simulations),
                                            Variable = rep("SOC",length_simulations),
                                            Scenario = rep(scenario2_name,length_simulations),
                                            Unit = rep("Stocks (MgC/ha)",length_simulations)
    )
    
    ######
    #CO2
    #####
    #scenario1
    mmmean_totFscenario1_df<-data.frame(Value=mmmean_totFscenario1,Year=t_fwd,
                                        Model = rep("Multi-model mean",length_simulations),
                                        Variable = rep("CO2",length_simulations),
                                        Scenario = rep(scenario1_name,length_simulations),
                                        Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    
    totF_Roth_C_scenario1_df<-data.frame(Value=totF_Roth_C_scenario1,Year=t_fwd,
                                         Model = rep("Roth-C",length_simulations),
                                         Variable = rep("CO2",length_simulations),
                                         Scenario = rep(scenario1_name,length_simulations),
                                         Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    totF_ICBM_C_scenario1_df<-data.frame(Value=totF_ICBM_C_scenario1,Year=t_fwd,
                                         Model = rep("ICBM",length_simulations),
                                         Variable = rep("CO2",length_simulations),
                                         Scenario = rep(scenario1_name,length_simulations),
                                         Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    
    totF_Century_C_scenario1_df<-data.frame(Value=totF_Century_C_scenario1,Year=t_fwd,
                                            Model = rep("Century",length_simulations),
                                            Variable = rep("CO2",length_simulations),
                                            Scenario = rep(scenario1_name,length_simulations),
                                            Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    totF_Yasso07_C_scenario1_df<-data.frame(Value=totF_Yasso07_C_scenario1,Year=t_fwd,
                                            Model = rep("Yasso07",length_simulations),
                                            Variable = rep("CO2",length_simulations),
                                            Scenario = rep(scenario1_name,length_simulations),
                                            Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    
    totF_Yasso20_C_scenario1_df<-data.frame(Value=totF_Yasso20_C_scenario1,Year=t_fwd,
                                            Model = rep("Yasso20",length_simulations),
                                            Variable = rep("CO2",length_simulations),
                                            Scenario = rep(scenario1_name,length_simulations),
                                            Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    totF_SG_C_scenario1_df<-data.frame(Value=totF_SG_C_scenario1,Year=t_fwd,
                                       Model = rep("SG",length_simulations),
                                       Variable = rep("CO2",length_simulations),
                                       Scenario = rep(scenario1_name,length_simulations),
                                       Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    #CH4
    totCH4_SG_C_scenario1_df<-data.frame(Value=totCH4_SG_C_scenario1,Year=t_fwd,
                                         Model = rep("SG",length_simulations),
                                         Variable = rep("CH4",length_simulations),
                                         Scenario = rep(scenario1_name,length_simulations),
                                         Unit = rep("Uptake (MgC/ha/yr)",length_simulations)
    )
    #N2O
    totN2O_SG_C_scenario1_df<-data.frame(Value=totN2O_SG_C_scenario1,Year=t_fwd,
                                         Model = rep("SG",length_simulations),
                                         Variable = rep("N2O",length_simulations),
                                         Scenario = rep(scenario1_name,length_simulations),
                                         Unit = rep("Efflux (MgC/ha/yr)",length_simulations))
    
    #scenario2
    
    mmmean_totFscenario2_df<-data.frame(Value=mmmean_totFscenario2,Year=t_fwd,
                                        Model = rep("Multi-model mean",length_simulations),
                                        Variable = rep("CO2",length_simulations),
                                        Scenario = rep(scenario2_name,length_simulations),
                                        Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    
    totF_Roth_C_scenario2_df<-data.frame(Value=totF_Roth_C_scenario2,Year=t_fwd,
                                         Model = rep("Roth-C",length_simulations),
                                         Variable = rep("CO2",length_simulations),
                                         Scenario = rep(scenario2_name,length_simulations),
                                         Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    totF_ICBM_C_scenario2_df<-data.frame(Value=totF_ICBM_C_scenario2,Year=t_fwd,
                                         Model = rep("ICBM",length_simulations),
                                         Variable = rep("CO2",length_simulations),
                                         Scenario = rep(scenario2_name,length_simulations),
                                         Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    totF_Century_C_scenario2_df<-data.frame(Value=totF_Century_C_scenario2,Year=t_fwd,
                                            Model = rep("Century",length_simulations),
                                            Variable = rep("CO2",length_simulations),
                                            Scenario = rep(scenario2_name,length_simulations),
                                            Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    totF_Yasso07_C_scenario2_df<-data.frame(Value=totF_Yasso07_C_scenario2,Year=t_fwd,
                                            Model = rep("Yasso07",length_simulations),
                                            Variable = rep("CO2",length_simulations),
                                            Scenario = rep(scenario2_name,length_simulations),
                                            Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    totF_Yasso20_C_scenario2_df<-data.frame(Value=totF_Yasso20_C_scenario2,Year=t_fwd,
                                            Model = rep("Yasso20",length_simulations),
                                            Variable = rep("CO2",length_simulations),
                                            Scenario = rep(scenario2_name,length_simulations),
                                            Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    totF_SG_C_scenario2_df<-data.frame(Value=totF_SG_C_scenario2,Year=t_fwd,
                                       Model = rep("SG",length_simulations),
                                       Variable = rep("CO2",length_simulations),
                                       Scenario = rep(scenario2_name,length_simulations),
                                       Unit = rep("Efflux (MgC/ha/yr)",length_simulations)
    )
    
    #CH4
    totCH4_SG_C_scenario2_df<-data.frame(Value=totCH4_SG_C_scenario2,Year=t_fwd,
                                         Model = rep("SG",length_simulations),
                                         Variable = rep("CH4",length_simulations),
                                         Scenario = rep(scenario2_name,length_simulations),
                                         Unit = rep("Uptake (MgC/ha/yr)",length_simulations)
    )
    #N2O
    totN2O_SG_C_scenario2_df<-data.frame(Value=totN2O_SG_C_scenario2,Year=t_fwd,
                                         Model = rep("SG",length_simulations),
                                         Variable = rep("N2O",length_simulations),
                                         Scenario = rep(scenario2_name,length_simulations),
                                         Unit = rep("Efflux (MgC/ha/yr)",length_simulations))
    
    
    data_download<-data.frame(
      rbind(mmmean_totCscenario1,
            totC_Roth_C_scenario1_df,
            totC_ICBM_C_scenario1_df,
            totC_Century_C_scenario1_df,
            totC_Yasso07_C_scenario1_df,
            totC_Yasso20_C_scenario1_df,
            
            mmmean_totCscenario2_df,
            totC_Roth_C_scenario2_df,
            totC_ICBM_C_scenario2_df,
            totC_Century_C_scenario2_df,
            totC_Yasso07_C_scenario2_df,
            totC_Yasso20_C_scenario2_df,
            
            mmmean_totFscenario1_df,
            totF_Roth_C_scenario1_df,
            totF_ICBM_C_scenario1_df,
            totF_Century_C_scenario1_df,
            totF_Yasso07_C_scenario1_df,
            totF_Yasso20_C_scenario1_df,
            totF_SG_C_scenario1_df,
            
            totCH4_SG_C_scenario1_df,
            totN2O_SG_C_scenario1_df,
            
            mmmean_totFscenario2_df,
            totF_Roth_C_scenario2_df,
            totF_ICBM_C_scenario2_df,
            totF_Century_C_scenario2_df,
            totF_Yasso07_C_scenario2_df,
            totF_Yasso20_C_scenario2_df,
            totF_SG_C_scenario2_df,
            
            totCH4_SG_C_scenario2_df,
            totN2O_SG_C_scenario2_df)
    )
    
    #Round SOC and CO2 variables
    data_download$Value <- ifelse(data_download$Variable=="SOC"|data_download$Variable=="CO2",
                                  round(data_download$Value,2),
                                  round(data_download$Value,8))
    #Reorder dataframe
    data_download<- data_download[,c("Model","Variable","Unit","Scenario","Year","Value")]
    
    # print("***********")
    # print("***********")
    # print("data_download")
    # print(data_download)
    # print("***********")
    # print("***********")
    
    return(data_download)
  }
  
  
  #--------------------------------------------------------------------------------
  #RUNS THE SIMULATIONS FOR FIXED LAND USE SCENARIO
  #---------------------------------------------------------------------------------
  output$simulations_fixedLU<- renderPlot({
    req(input$clay_slider)
    print(" ")
    print("####################################################")
    print("####################################################")
    print("Entering simulations_fixedLU")
    print("####################################################")
    print("####################################################")
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #Run retreive_clim_data_site function to retrieve climate data for site
    print("Retrieving climate data for site for climate scenarios")
    data_clim = retreive_clim_data_site()
    # Potevap_month_spinup_rcp26=data_clim[[1]]
    # Potevap_month_fwd_rcp26=data_clim[[13]]
    # Potevap_month_fwd_rcp60=data_clim[[14]]
    # Precip_day_spinup=data_clim[[4]]
    # Precip_day_fwd_rcp26=data_clim[[15]]
    # Precip_day_fwd_rcp60=data_clim[[16]]
    # Temp_day_spinup=data_clim[[7]]
    # Temp_day_fwd_rcp26=data_clim[[17]]
    # Temp_day_fwd_rcp60=data_clim[[18]]
    # Vswc_day_spinup=data_clim[[10]]
    # Vswc_day_fwd_rcp26=data_clim[[19]]
    # Vswc_day_fwd_rcp60=data_clim[[20]]
    
    Potevap_month_spinup_rcp26=data_clim[[1]]
    Potevap_month_fwd_rcp26_sel=data_clim[[2]]
    Potevap_month_fwd_rcp60_sel=data_clim[[3]]
    Precip_day_spinup=data_clim[[4]]
    Precip_day_fwd_rcp26_sel=data_clim[[5]]
    Precip_day_fwd_rcp60_sel=data_clim[[6]]
    Temp_day_spinup=data_clim[[7]]
    Temp_day_fwd_rcp26_sel=data_clim[[8]]
    Temp_day_fwd_rcp60_sel=data_clim[[9]]
    Vswc_day_spinup=data_clim[[10]]
    Vswc_day_fwd_rcp26_sel=data_clim[[11]]
    Vswc_day_fwd_rcp60_sel=data_clim[[12]]
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #Run retreive_Cinput function to retrieve Cinput for climate scenarios
    print(" ")
    print("Retrieving C input data for site")
    retreive_Cinput_ag_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_ag)
    retreive_Cinput_bg_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_bg)
    retreive_Cinput_rcp26_fix = retreive_Cinput_ag_rcp26_fix+retreive_Cinput_bg_rcp26_fix
    
    retreive_Cinput_ag_rcp60_fix = retreive_Cinput(lon_litter_rcp60,lat_litter_rcp60,litter_time_rcp60,litter_rcp60_ag)
    retreive_Cinput_bg_rcp60_fix = retreive_Cinput(lon_litter_rcp60,lat_litter_rcp60,litter_time_rcp60,litter_rcp60_bg)
    retreive_Cinput_rcp60_fix=retreive_Cinput_ag_rcp60_fix+retreive_Cinput_bg_rcp60_fix
    
    AWEN_input = c(as.numeric(input$A_pool),
                   as.numeric(input$W_pool),
                   as.numeric(input$E_pool),
                   as.numeric(input$N_pool),
                   0)
    
    #Suppose W and E litter compounds are more labile
    decomposable_plant_material <-as.numeric(input$W_pool)+as.numeric(input$E_pool)
    #Suppose A and N litter compounds are more resistant
    resistant_plant_material <-as.numeric(input$A_pool)+as.numeric(input$N_pool)
    #Decomposable to resistant ratio
    DR_in = decomposable_plant_material/resistant_plant_material
    
    
    #CHANGE
    computation_time_step_fwd = 1
    #simulation_length_CCscenario = length(Potevap_month_fwd_rcp26$Date)/12
    simulation_length_CCscenario = simulation_length()
    
    ######
    print("####################################################")
    print("Preparing to run RCP26")
    print("####################################################")
    ######
    test_mmRCP26 <-Call_MULTIMODEL_i1(plot_figures=plot_figures,simulation_length=simulation_length_CCscenario, spinup_length=spinup_length,
                                      computation_time_step_fwd=computation_time_step_fwd,
                                      start_date_simulations=dates_in(),
                                      temperature_spinup=Temp_day_spinup, precipitation_spinup=Precip_day_spinup, potential_evapotranspiration_spinup=Potevap_month_spinup_rcp26,
                                      soilmoisture_spinup=as.numeric(Vswc_day_spinup$Vswc),
                                      temperature_fwd=Temp_day_fwd_rcp26_sel, precipitation_fwd=Precip_day_fwd_rcp26_sel, potential_evapotranspiration_fwd=Potevap_month_fwd_rcp26_sel,soilmoisture_fwd=as.numeric(Vswc_day_fwd_rcp26_sel$Vswc),
                                      SOC_0=input$SOC,
                                      C_input_ag_spinup=as.numeric(mean(retreive_Cinput_ag_rcp26_fix)),C_input_bg_spinup=as.numeric(mean(retreive_Cinput_bg_rcp26_fix)),
                                      C_input_ag_fwd=as.numeric(retreive_Cinput_ag_rcp26_fix),C_input_bg_fwd=as.numeric(retreive_Cinput_bg_rcp26_fix),
                                      clay_p=input$clay_slider,silt_p=input$silt,soil_thickness=input$soilthick,pH_p=ph_site,
                                      lignin_to_nitrogen=input$LNratio,structural_in_lignin=input$SLratio,woodylittersize=input$WLS,AWEN_in=AWEN_input,decomp_to_resist_ratio=DR_in,
                                      CN_Ratio=input$CNratio, Bulk_Density=input$bulkdensity, WFPS=water_filled_pore_space, CH4_Conc=input$CH4_data,
                                      decomposition_param_RothC=ksRothC,
                                      decomposition_param_ICBM=param_ICBM,
                                      decomposition_param_Century=ksCent,
                                      decomposition_param_Yasso07=paramYasso07,
                                      decomposition_param_Yasso20=paramYasso20)
    
    print("End of multimodel simulation RCP2.6")
    print(" ")
    print("...Plotting RCP2.6 completed...")
    
    t_fwd_col = seq.int(1,simulation_length_CCscenario,by=computation_time_step_fwd)
    
    
    data_mmRCP26 <- get_data_from_multimodel_run(test_mmRCP26,t_fwd_col)
    minCRCP26=data_mmRCP26[[1]]
    maxCRCP26=data_mmRCP26[[2]]
    mmmean_totCRCP26=data_mmRCP26[[3]]
    totC_Roth_C_RCP26=data_mmRCP26[[4]]
    totC_ICBM_C_RCP26=data_mmRCP26[[5]]
    totC_Century_C_RCP26=data_mmRCP26[[6]]
    totC_Yasso07_C_RCP26=data_mmRCP26[[7]]
    totC_Yasso20_C_RCP26=data_mmRCP26[[8]]
    minFRCP26=data_mmRCP26[[9]]
    maxFRCP26=data_mmRCP26[[10]]
    mmmean_totFRCP26=data_mmRCP26[[11]]
    totF_Roth_C_RCP26=data_mmRCP26[[12]]
    totF_ICBM_C_RCP26=data_mmRCP26[[13]]
    
    totF_Century_C_RCP26=data_mmRCP26[[14]]
    totF_Yasso07_C_RCP26=data_mmRCP26[[15]]
    totF_Yasso20_C_RCP26=data_mmRCP26[[16]]
    
    totF_SG_C_RCP26=data_mmRCP26[[17]]
    totCH4_SG_C_RCP26=data_mmRCP26[[18]]
    totN2O_SG_C_RCP26=data_mmRCP26[[19]]
    
    
    ###########################    ###########################
    print("Preparing to run RCP60")
    ###########################    ###########################
    #This will launch the function defined in "Holisoils_multimodel_v1.R"
    test_mmRCP60 <-Call_MULTIMODEL_i1(plot_figures=plot_figures,simulation_length=simulation_length_CCscenario, spinup_length=spinup_length,
                                      computation_time_step_fwd=computation_time_step_fwd,
                                      #computation_time_step_fwd=1, computation_time_step_spinup=1, tolti, perche ogni modello ha il suo gia predefinito
                                      start_date_simulations=Prec_month_spinup_rcp26$Date[1],
                                      temperature_spinup=Temp_day_spinup, precipitation_spinup=Precip_day_spinup, potential_evapotranspiration_spinup=Potevap_month_spinup_rcp26,
                                      soilmoisture_spinup=as.numeric(Vswc_day_spinup$Vswc),
                                      #soilmoisture_spinup=as.numeric(Vswc_day_spinup$Vswc),
                                      temperature_fwd=Temp_day_fwd_rcp60_sel, precipitation_fwd=Precip_day_fwd_rcp60_sel, potential_evapotranspiration_fwd=Potevap_month_fwd_rcp60_sel,soilmoisture_fwd=as.numeric(Vswc_day_fwd_rcp60_sel$Vswc),
                                      #soilmoisture_fwd=as.numeric(Vswc_day_fwd$Vswc),
                                      SOC_0=input$SOC,
                                      C_input_ag_spinup=as.numeric(mean(retreive_Cinput_ag_rcp60_fix)),C_input_bg_spinup=as.numeric(mean(retreive_Cinput_bg_rcp60_fix)),
                                      C_input_ag_fwd=as.numeric(retreive_Cinput_ag_rcp60_fix),C_input_bg_fwd=as.numeric(retreive_Cinput_bg_rcp60_fix),
                                      clay_p=input$clay_slider,silt_p=input$silt,soil_thickness=input$soilthick,pH_p=ph_site,
                                      lignin_to_nitrogen=input$LNratio,structural_in_lignin=input$SLratio,woodylittersize=input$WLS,AWEN_in=AWEN_input,decomp_to_resist_ratio=DR_in,
                                      CN_Ratio=input$CNratio, Bulk_Density=input$bulkdensity, WFPS=water_filled_pore_space, CH4_Conc=input$CH4_data,
                                      decomposition_param_RothC=ksRothC,
                                      decomposition_param_ICBM=param_ICBM,
                                      decomposition_param_Century=ksCent,
                                      decomposition_param_Yasso07=paramYasso07,
                                      decomposition_param_Yasso20=paramYasso20)
    
    print("End of multimodel simulation RCP6.0")
    print(" ")
    print("...Plotting RCP6.0 completed...")
    
    data_mmRCP60 <- get_data_from_multimodel_run(test_mmRCP60,t_fwd_col)
    minCRCP60=data_mmRCP60[[1]]
    maxCRCP60=data_mmRCP60[[2]]
    mmmean_totCRCP60=data_mmRCP60[[3]]
    totC_Roth_C_RCP60=data_mmRCP60[[4]]
    totC_ICBM_C_RCP60=data_mmRCP60[[5]]
    totC_Century_C_RCP60=data_mmRCP60[[6]]
    totC_Yasso07_C_RCP60=data_mmRCP60[[7]]
    totC_Yasso20_C_RCP60=data_mmRCP60[[8]]
    minFRCP60=data_mmRCP60[[9]]
    maxFRCP60=data_mmRCP60[[10]]
    mmmean_totFRCP60=data_mmRCP60[[11]]
    totF_Roth_C_RCP60=data_mmRCP60[[12]]
    totF_ICBM_C_RCP60=data_mmRCP60[[13]]
    totF_Century_C_RCP60=data_mmRCP60[[14]]
    totF_Yasso07_C_RCP60=data_mmRCP60[[15]]
    totF_Yasso20_C_RCP60=data_mmRCP60[[16]]
    
    totF_SG_C_RCP60=data_mmRCP60[[17]]
    totCH4_SG_C_RCP60=data_mmRCP60[[18]]
    totN2O_SG_C_RCP60=data_mmRCP60[[19]]
    
    
    ###########################
    #Plot STOCKS COMPARISON CC
    ###########################
    
    #par(mfrow=c(3,1),mar=c(6, 5, 5, 3))
    
    express_plotC = expression("SOC stocks (MgC"~{ha}^{-1}~")")
    
    minC <- min(cbind(minCRCP26,minCRCP60))
    maxC <- max(cbind(maxCRCP26,maxCRCP60))
    
    t_fwd = seq(1,simulation_length_CCscenario,by=computation_time_step_fwd)
    
    plot(t_fwd, mmmean_totCRCP26, type="l", lty=1, lwd=3, xlab="Time (years)",ylab=" ",col="blue",ylim=c(minC-5,maxC+5))
    lines(t_fwd,totC_Roth_C_RCP26,type="l", lty=1, lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totC_ICBM_C_RCP26,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totC_Century_C_RCP26,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totC_Yasso07_C_RCP26,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totC_Yasso20_C_RCP26,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    title(ylab=express_plotC,main="Multi-model SOC stocks",mgp=c(2,1,0))
    
    lines(t_fwd,mmmean_totCRCP60,type="l", lty=1,lwd=3, col="red")
    lines(t_fwd,totC_Roth_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totC_ICBM_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totC_Century_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totC_Yasso07_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totC_Yasso20_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    
    legend(par('usr')[2], par('usr')[4], c("RCP 2.6", "RCP 6.0"),
           lty=1,lwd=c(3,3), col=c("blue","red"), cex=0.8,xpd=NA,bty="n")
    
    
    #PLOT FLUXES COMPARISON CC
    express_plotF = expression(CO[2]~"flux (MgC"~{ha}^{-1}*{year}^{-1}~")")
    
    minF <- min(cbind(minFRCP26,minFRCP60))
    maxF <- max(cbind(maxFRCP26,maxFRCP60))
    
    plot(t_fwd, mmmean_totFRCP26, type="l", lty=1, lwd=3, xlab="Time (years)",ylab=" ",col="blue",ylim=c(minF-2,maxF+2))
    lines(t_fwd,totF_Roth_C_RCP26,type="l", lty=1, lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_ICBM_C_RCP26,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_Century_C_RCP26,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_Yasso07_C_RCP26,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_Yasso20_C_RCP26,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_SG_C_RCP26,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    title(ylab=express_plotF,main= expression(bold("Multi-model "~CO[2]~"fluxes")),mgp=c(2,1,0))
    
    lines(t_fwd,mmmean_totFRCP60,type="l", lty=1,lwd=3, col="red")
    lines(t_fwd,totF_Roth_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_ICBM_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_Century_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_Yasso07_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_Yasso20_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_SG_C_RCP60,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    
    legend(par('usr')[2], par('usr')[4], c("RCP 2.6", "RCP 6.0"),
           lty=1,lwd=c(3,3), col=c("blue","red"), cex=0.8,xpd=NA,bty="n")
    
    
    ########
    # #PLOT CH4 and N2O SG
    # express_plot_GHG=expression(N[2]~"O fluxes, "~CH[4]~"uptake (MgC"~{ha}^{-1}*{year}^{-1}~")")
    # 
    # max_GHG_RCP26 <-max(as.numeric(cbind(totCH4_SG_C_RCP26,totN2O_SG_C_RCP26)))
    # max_GHG_RCP60 <-max(as.numeric(cbind(totCH4_SG_C_RCP60,totN2O_SG_C_RCP60)))
    # max_GHG <- max(cbind(max_GHG_RCP26,max_GHG_RCP60))
    # 
    # plot(t_fwd, totCH4_SG_C_RCP26, type="l", lty=4, lwd=1, xlab="Time (years)",ylab=" ",col="blue",
    #      ylim=c(-0.01,max_GHG+0.01))
    # lines(t_fwd, totN2O_SG_C_RCP26, type="l", lty=3, lwd=1,col="blue")
    # lines(t_fwd,totCH4_SG_C_RCP60,type="l", lty=4, lwd=1, col=alpha("red"))
    # lines(t_fwd,totN2O_SG_C_RCP60,type="l", lty=3, lwd=1, col=alpha("red"))
    # 
    # title(ylab=express_plot_GHG,main= "Other GHG fluxes",mgp=c(2,1,0))
    # 
    # legend(par('usr')[2], par('usr')[4], c(expression(CH[4]~"uptake"),expression(N[2]~"O fluxes")),
    #        lty=c(4,3),lwd=c(1,1), col="black", cex=0.8,xpd=NA,bty="n")
    
    
    
    data_download_FixedLU <-create_dataframe_for_download(t_fwd,
                                                          data_mmRCP26,
                                                          data_mmRCP60,
                                                          "RCP2.6",
                                                          "RCP6.0")
    # Filter data based on user input
    filtered_data <- reactive({
      
      subset(data_download_FixedLU,data_download_FixedLU$Variable== input$Select_variable
             & data_download_FixedLU$Scenario== input$Select_RCP
             & data_download_FixedLU$Model== input$Select_model)
    })
    
    observe({
      category_choices <- subset(data_download_FixedLU,data_download_FixedLU$Variable== input$Select_variable)
      updateSelectInput(session, "Select_model", choices = unique(category_choices$Model))
    })
    
    #Show data in table
    output$outTable_FixedLU <- renderDT({
      datatable(filtered_data(), options = list(pageLength = 10),
                rownames = FALSE)
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData_FixedLU <- downloadHandler(
      filename = function() {
        paste("data_", input$Select_variable, "_", input$Select_RCP, "_", input$Select_model, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data(), file, row.names = FALSE)
      }
    )
    
    
  }) #renderPlot
  
  
  #--------------------------------------------------------------------------------
  #RUNS THE SIMULATIONS FOR  LAND USE CHANGE SCENARIO
  #---------------------------------------------------------------------------------
  
  output$simulations_LUchange<- renderPlot({
    print("Entering simulations_LUchange")
    req(input$clay_slider2)
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #Run retreive_clim_data_site function to retrieve climate data for site
    print("Retrieving climate data for site for climate scenarios")
    data_clim = retreive_clim_data_site()
    # Potevap_month_spinup_rcp26=data_clim[[1]]
    # Potevap_month_fwd_rcp26=data_clim[[13]]
    # Potevap_month_fwd_rcp60=data_clim[[14]]
    # Precip_day_spinup=data_clim[[4]]
    # Precip_day_fwd_rcp26=data_clim[[15]]
    # Precip_day_fwd_rcp60=data_clim[[16]]
    # Temp_day_spinup=data_clim[[7]]
    # Temp_day_fwd_rcp26=data_clim[[17]]
    # Temp_day_fwd_rcp60=data_clim[[18]]
    # Vswc_day_spinup=data_clim[[10]]
    # Vswc_day_fwd_rcp26=data_clim[[19]]
    # Vswc_day_fwd_rcp60=data_clim[[20]]
    
    Potevap_month_spinup_rcp26=data_clim[[1]]
    Potevap_month_fwd_rcp26_sel=data_clim[[2]]
    Potevap_month_fwd_rcp60_sel=data_clim[[3]]
    Precip_day_spinup=data_clim[[4]]
    Precip_day_fwd_rcp26_sel=data_clim[[5]]
    Precip_day_fwd_rcp60_sel=data_clim[[6]]
    Temp_day_spinup=data_clim[[7]]
    Temp_day_fwd_rcp26_sel=data_clim[[8]]
    Temp_day_fwd_rcp60_sel=data_clim[[9]]
    Vswc_day_spinup=data_clim[[10]]
    Vswc_day_fwd_rcp26_sel=data_clim[[11]]
    Vswc_day_fwd_rcp60_sel=data_clim[[12]]
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #Run retreive_Cinput function to retrieve Cinput for climate scenarios
    print("Retrieving climate data for site for climate scenarios")
    retreive_Cinput_ag_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_ag)
    retreive_Cinput_bg_rcp26_fix = retreive_Cinput(lon_litter_rcp26,lat_litter_rcp26,litter_time_rcp26,litter_rcp26_bg)
    retreive_Cinput_rcp26_fix = retreive_Cinput_ag_rcp26_fix+retreive_Cinput_bg_rcp26_fix
    #retreive_Cinput_rcp26_fix_mean = mean(retreive_Cinput_rcp26_fix)
    
    retreive_Cinput_ag_rcp60_fix = retreive_Cinput(lon_litter_rcp60,lat_litter_rcp60,litter_time_rcp60,litter_rcp60_ag)
    retreive_Cinput_bg_rcp60_fix = retreive_Cinput(lon_litter_rcp60,lat_litter_rcp60,litter_time_rcp60,litter_rcp60_bg)
    retreive_Cinput_rcp60_fix=retreive_Cinput_ag_rcp60_fix+retreive_Cinput_bg_rcp60_fix
    #retreive_Cinput_rcp60_fix_mean = mean(retreive_Cinput_rcp60_fix)
    
    retreive_Cinput_ag_rcp26_var = retreive_Cinput(lon_litter_LU_rcp26,lat_litter_LU_rcp26,litter_LU_time_rcp26,litter_LU_rcp26_ag)
    retreive_Cinput_bg_rcp26_var = retreive_Cinput(lon_litter_LU_rcp26,lat_litter_LU_rcp26,litter_LU_time_rcp26,litter_LU_rcp26_bg)
    retreive_Cinput_rcp26_var = retreive_Cinput_ag_rcp26_var+retreive_Cinput_bg_rcp26_var
    #retreive_Cinput_rcp26_var_mean = mean(retreive_Cinput_rcp26_var)
    
    retreive_Cinput_ag_rcp60_var = retreive_Cinput(lon_litter_LU_rcp60,lat_litter_LU_rcp60,litter_LU_time_rcp60,litter_LU_rcp60_ag)
    retreive_Cinput_bg_rcp60_var = retreive_Cinput(lon_litter_LU_rcp60,lat_litter_LU_rcp60,litter_LU_time_rcp60,litter_LU_rcp60_bg)
    retreive_Cinput_rcp60_var = retreive_Cinput_ag_rcp60_var+retreive_Cinput_bg_rcp60_var
    #retreive_Cinput_rcp60_var_mean = mean(retreive_Cinput_rcp60_var)
    
    AWEN_input = c(as.numeric(input$A_pool),
                   as.numeric(input$W_pool),
                   as.numeric(input$E_pool),
                   as.numeric(input$N_pool),
                   0)
    #Suppose W and E litter compounds are more labile
    decomposable_plant_material <-as.numeric(input$W_pool)+as.numeric(input$E_pool)
    #Suppose A and N litter compounds are more resistant
    resistant_plant_material <-as.numeric(input$A_pool)+as.numeric(input$N_pool)
    #Decomposable to resistant ratio
    DR_in = decomposable_plant_material/resistant_plant_material
    
    #CHANGE
    computation_time_step_fwd = 1
    #simulation_length_CCscenario = length(Potevap_month_fwd_rcp26$Date)/12
    simulation_length_CCscenario=simulation_length()
    ######
    print("Preparing to run RCP26")
    ######
    #This will launch the function defined in "Holisoils_multimodel_v1.R"
    test_mmRCP26_var <-Call_MULTIMODEL_i1(plot_figures=plot_figures,simulation_length=simulation_length_CCscenario, spinup_length=spinup_length,
                                          computation_time_step_fwd=computation_time_step_fwd,
                                          #computation_time_step_fwd=1, computation_time_step_spinup=1, tolti, perche ogni modello ha il suo gia predefinito
                                          start_date_simulations=Prec_month_spinup_rcp26$Date[1],
                                          temperature_spinup=Temp_day_spinup, precipitation_spinup=Precip_day_spinup, potential_evapotranspiration_spinup=Potevap_month_spinup_rcp26,
                                          soilmoisture_spinup=as.numeric(Vswc_day_spinup$Vswc),
                                          #soilmoisture_spinup=as.numeric(Vswc_day_spinup$Vswc),
                                          temperature_fwd=Temp_day_fwd_rcp26_sel, precipitation_fwd=Precip_day_fwd_rcp26_sel, potential_evapotranspiration_fwd=Potevap_month_fwd_rcp26_sel,soilmoisture_fwd=as.numeric(Vswc_day_fwd_rcp26_sel$Vswc),
                                          #soilmoisture_fwd=as.numeric(Vswc_day_fwd$Vswc),
                                          SOC_0=input$SOC,
                                          C_input_ag_spinup=as.numeric(mean(retreive_Cinput_ag_rcp26_var)),C_input_bg_spinup=as.numeric(mean(retreive_Cinput_bg_rcp26_var)),
                                          C_input_ag_fwd=as.numeric(retreive_Cinput_ag_rcp26_var),C_input_bg_fwd=as.numeric(retreive_Cinput_bg_rcp26_var),
                                          #clay_p=input$clay,silt_p=input$silt,soil_thickness=input$soilthick,pH_p=ph_site,
                                          clay_p=input$clay_slider2,silt_p=input$silt,soil_thickness=input$soilthick,pH_p=ph_site,
                                          lignin_to_nitrogen=input$LNratio,structural_in_lignin=input$SLratio,woodylittersize=input$WLS,AWEN_in=AWEN_input,decomp_to_resist_ratio=DR_in,
                                          CN_Ratio=input$CNratio, Bulk_Density=input$bulkdensity, WFPS=water_filled_pore_space, CH4_Conc=input$CH4_data,
                                          decomposition_param_RothC=ksRothC,
                                          decomposition_param_ICBM=param_ICBM,
                                          decomposition_param_Century=ksCent,
                                          decomposition_param_Yasso07=paramYasso07,
                                          decomposition_param_Yasso20=paramYasso20)
    
    print("End of multimodel simulation RCP2.6")
    print(" ")
    print("...Plotting RCP2.6 completed...")
    
    t_fwd_col = seq.int(1,simulation_length_CCscenario,by=computation_time_step_fwd)
    
    data_mmRCP26_var <- get_data_from_multimodel_run(test_mmRCP26_var,t_fwd_col)
    minCRCP26_var=data_mmRCP26_var[[1]]
    maxCRCP26_var=data_mmRCP26_var[[2]]
    mmmean_totCRCP26_var=data_mmRCP26_var[[3]]
    totC_Roth_C_RCP26_var=data_mmRCP26_var[[4]]
    totC_ICBM_C_RCP26_var=data_mmRCP26_var[[5]]
    totC_Century_C_RCP26_var=data_mmRCP26_var[[6]]
    totC_Yasso07_C_RCP26_var=data_mmRCP26_var[[7]]
    totC_YASSO20_C_RCP26_var=data_mmRCP26_var[[8]]
    minFRCP26_var=data_mmRCP26_var[[9]]
    maxFRCP26_var=data_mmRCP26_var[[10]]
    mmmean_totFRCP26_var=data_mmRCP26_var[[11]]
    totF_Roth_C_RCP26_var=data_mmRCP26_var[[12]]
    totF_ICBM_C_RCP26_var=data_mmRCP26_var[[13]]
    totF_Century_C_RCP26_var=data_mmRCP26_var[[14]]
    totF_Yasso07_C_RCP26_var=data_mmRCP26_var[[15]]
    totF_Yasso20_C_RCP26_var=data_mmRCP26_var[[16]]
    
    totF_SG_C_RCP26_var=data_mmRCP26_var[[17]]
    totCH4_SG_C_RCP26_var=data_mmRCP26_var[[18]]
    totN2O_SG_C_RCP26_var=data_mmRCP26_var[[19]]
    
    ###########################    ###########################
    print("Preparing to run RCP60")
    ###########################    ###########################
    #This will launch the function defined in "Holisoils_multimodel_v1.R"
    test_mmRCP60_var <-Call_MULTIMODEL_i1(plot_figures=plot_figures,simulation_length=simulation_length_CCscenario, spinup_length=spinup_length,
                                          computation_time_step_fwd=computation_time_step_fwd,
                                          #computation_time_step_fwd=1, computation_time_step_spinup=1, tolti, perche ogni modello ha il suo gia predefinito
                                          start_date_simulations=Prec_month_spinup_rcp26$Date[1],
                                          temperature_spinup=Temp_day_spinup, precipitation_spinup=Precip_day_spinup, potential_evapotranspiration_spinup=Potevap_month_spinup_rcp26,
                                          soilmoisture_spinup=as.numeric(Vswc_day_spinup$Vswc),
                                          temperature_fwd=Temp_day_fwd_rcp60_sel, precipitation_fwd=Precip_day_fwd_rcp60_sel, potential_evapotranspiration_fwd=Potevap_month_fwd_rcp60_sel,soilmoisture_fwd=as.numeric(Vswc_day_fwd_rcp60_sel$Vswc),
                                          SOC_0=input$SOC,
                                          C_input_ag_spinup=as.numeric(mean(retreive_Cinput_ag_rcp60_var)),C_input_bg_spinup=as.numeric(mean(retreive_Cinput_bg_rcp60_var)),
                                          C_input_ag_fwd=as.numeric(retreive_Cinput_ag_rcp60_var),C_input_bg_fwd=as.numeric(retreive_Cinput_bg_rcp60_var),
                                          clay_p=input$clay_slider2,silt_p=input$silt,soil_thickness=input$soilthick,pH_p=ph_site,
                                          lignin_to_nitrogen=input$LNratio,structural_in_lignin=input$SLratio,woodylittersize=input$WLS,AWEN_in=AWEN_input,decomp_to_resist_ratio=DR_in,
                                          CN_Ratio=input$CNratio, Bulk_Density=input$bulkdensity, WFPS=water_filled_pore_space, CH4_Conc=input$CH4_data,
                                          decomposition_param_RothC=ksRothC,
                                          decomposition_param_ICBM=param_ICBM,
                                          decomposition_param_Century=ksCent,
                                          decomposition_param_Yasso07=paramYasso07,
                                          decomposition_param_Yasso20=paramYasso20)
    
    print("End of multimodel simulation RCP6.0")
    print(" ")
    print("...Plotting RCP6.0 completed...")
    
    data_mmRCP60_var <- get_data_from_multimodel_run(test_mmRCP60_var,t_fwd_col)
    minCRCP60_var=data_mmRCP60_var[[1]]
    maxCRCP60_var=data_mmRCP60_var[[2]]
    mmmean_totCRCP60_var=data_mmRCP60_var[[3]]
    totC_Roth_C_RCP60_var=data_mmRCP60_var[[4]]
    totC_ICBM_C_RCP60_var=data_mmRCP60_var[[5]]
    totC_Century_C_RCP60_var=data_mmRCP60_var[[6]]
    totC_Yasso07_C_RCP60_var=data_mmRCP60_var[[7]]
    totC_Yasso20_C_RCP60_var=data_mmRCP60_var[[8]]
    minFRCP60_var=data_mmRCP60_var[[9]]
    maxFRCP60_var=data_mmRCP60_var[[10]]
    mmmean_totFRCP60_var=data_mmRCP60_var[[11]]
    totF_Roth_C_RCP60_var=data_mmRCP60_var[[12]]
    totF_ICBM_C_RCP60_var=data_mmRCP60_var[[13]]
    totF_Century_C_RCP60_var=data_mmRCP60_var[[14]]
    totF_Yasso07_C_RCP60_var=data_mmRCP60_var[[15]]
    totF_Yasso20_C_RCP60_var=data_mmRCP60_var[[16]]
    
    totF_SG_C_RCP60_var=data_mmRCP60_var[[17]]
    totCH4_SG_C_RCP60_var=data_mmRCP26_var[[18]]
    totN2O_SG_C_RCP60_var=data_mmRCP60_var[[19]]
    ###########################
    #Plot STOCKS COMPARISON CC
    ###########################
    express_plotC = expression("SOC stocks (MgC"~{ha}^{-1}~")")
    
    minC <- min(cbind(minCRCP26_var,minCRCP60_var))
    maxC <- max(cbind(maxCRCP26_var,maxCRCP60_var))
    
    t_fwd = seq(1,simulation_length_CCscenario,by=computation_time_step_fwd)
    
    plot(t_fwd, mmmean_totCRCP26_var, type="l", lty=1, lwd=3, xlab="Time (years)",ylab=" ",col="blue",ylim=c(minC-5,maxC+5))
    lines(t_fwd,totC_Roth_C_RCP26_var,type="l", lty=1, lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totC_ICBM_C_RCP26_var,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totC_Century_C_RCP26_var,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totC_Yasso07_C_RCP26_var,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totC_YASSO20_C_RCP26_var,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    title(ylab=express_plotC,main="Multi-model SOC stocks",mgp=c(2,1,0))
    
    lines(t_fwd,mmmean_totCRCP60_var,type="l", lty=1,lwd=3, col="red")
    lines(t_fwd,totC_Roth_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totC_ICBM_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totC_Century_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totC_Yasso07_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totC_Yasso20_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    
    legend(par('usr')[2], par('usr')[4], c("RCP 2.6", "RCP 6.0"),
           lty=1,lwd=c(3,3), col=c("blue","red"), cex=0.8,xpd=NA,bty="n")
    
    #PLOT FLUXES COMPARISON CC
    express_plotF = expression(CO[2]~"flux (MgC"~{ha}^{-1}*{year}^{-1}~")")
    
    minF <- min(cbind(minFRCP26_var,minFRCP60_var))
    maxF <- max(cbind(maxFRCP26_var,maxFRCP60_var))
    
    plot(t_fwd, mmmean_totFRCP26_var, type="l", lty=1, lwd=3, xlab="Time (years)",ylab=" ",col="blue",ylim=c(minF-2,maxF+2))
    lines(t_fwd,totF_Roth_C_RCP26_var,type="l", lty=1, lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_ICBM_C_RCP26_var,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_Century_C_RCP26_var,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_Yasso07_C_RCP26_var,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_Yasso20_C_RCP26_var,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    lines(t_fwd,totF_SG_C_RCP26_var,type="l", lty=1,lwd=1, col=alpha("blue",0.3))
    title(ylab=express_plotF,main= expression(bold("Multi-model "~CO[2]~"fluxes")),mgp=c(2,1,0))
    
    lines(t_fwd,mmmean_totFRCP60_var,type="l", lty=1,lwd=3, col="red")
    lines(t_fwd,totF_Roth_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_ICBM_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_Century_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_Yasso07_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_SG_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    lines(t_fwd,totF_Yasso20_C_RCP60_var,type="l", lty=1,lwd=1, col=alpha("red",0.3))
    
    legend(par('usr')[2], par('usr')[4], c("RCP 2.6", "RCP 6.0"),
           lty=1,lwd=c(3,3), col=c("blue","red"), cex=0.8,xpd=NA,bty="n")
    
    
    ########################
    ########################
    #Create data for download
    data_download_LUchange <-create_dataframe_for_download(t_fwd,
                                                           data_mmRCP26_var,
                                                           data_mmRCP60_var,
                                                           "RCP2.6",
                                                           "RCP6.0")
    # Filter data based on user input
    filtered_data_LUchange <- reactive({
      
      subset(data_download_LUchange,data_download_LUchange$Variable== input$Select_variable2
             & data_download_LUchange$Scenario== input$Select_RCP2
             & data_download_LUchange$Model== input$Select_model2)
    })
    
    observe({
      category_choices <- subset(data_download_LUchange,data_download_LUchange$Variable== input$Select_variable2)
      updateSelectInput(session, "Select_model2", choices = unique(category_choices$Model))
    })
    
    #Show data in table
    output$outTable_LUchange <- renderDT({
      datatable(filtered_data_LUchange(), options = list(pageLength = 10),
                rownames = FALSE)
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData_LUchange <- downloadHandler(
      filename = function() {
        paste("data_", input$Select_variable2, "_", input$Select_RCP2, "_", input$Select_model2, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data_LUchange(), file, row.names = FALSE)
      }
    )
    
    
    
  })
  
  
  #--------------------------------------------------------------------------------
  #RUNS THE SIMULATIONS LAND MANAGEMENT SCENARIO
  #---------------------------------------------------------------------------------
  
  output$plot_land_management<- renderPlot({
    #req(input$clay_slider3)
    req(input$clay_slider2)
    req(input$MR_slider)
    req(input$HR_slider)
    
    print("Entering plot_land_management")
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    print("Retrieving climate data from user inputs")
    #data_clim_user =upload_user_clim_data_site()
    
    #Fwd
    # Potevap_month = data_clim_user[[1]]
    # Temp_day = data_clim_user[[2]]
    # Precip_day = data_clim_user[[3]]
    # Vswc_day = data_clim_user[[4]]
    #Fwd
    Potevap_month = data_potevap()
    Temp_day = data_temperature()
    Precip_day = data_precipitation()
    Vswc_day = data_vswc()
    
    # print("Potevap_month")
    # print(head(Potevap_month))
    # 
    # print("Temp_day")
    # print(head(Temp_day))
    # 
    # print("Precip_day")
    # print(head(Precip_day))
    # 
    # print("Vswc_day")
    # print(head(Vswc_day))
    
    #Spinup
    #data_clim_user_spinup = upload_user_spinup_clim_data_site()
    print("Creating spinup climate variables")
    Potevap_month_spinup = data_potevap_spinup()
    Temp_day_spinup = data_temp_spinup()
    Precip_day_spinup = data_prec_spinup()
    Vswc_day_spinup = data_vswc_spinup()
    
    # print("Potevap_month_spinup")
    # print(head(Potevap_month_spinup))
    # 
    # print("Temp_day_spinup")
    # print(head(Temp_day_spinup))
    # 
    # print("Precip_day_spinup")
    # print(head(Precip_day_spinup))
    # 
    # print("Vswc_day_spinup")
    # print(head(Vswc_day_spinup))
    
    
    
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<#
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    
    #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>#
    print("Retrieving C input for control land-management")
    
    retreive_Cinput_ag_rcp26_fix=data_Cinput_ag()
    retreive_Cinput_bg_rcp26_fix=data_Cinput_bg()
    #^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    #Cin TOT contol scenario (time series)
    retreive_Cinput_rcp26_fix = retreive_Cinput_ag_rcp26_fix+retreive_Cinput_bg_rcp26_fix
    print("retreive_Cinput_rcp26_fix")
    print(retreive_Cinput_rcp26_fix)
    
    
    print("Retrieving C biomass in vegetation to calculate C input after disturbance event")
    ######################################################
    #C input from died vegetation at year of disturbance
    ######################################################
    #cveg
    #retreive_Cbiomass_rcp26 = retreive_Cbiomass(lon_veg_clitter_rcp26,lat_veg_clitter_rcp26,veg_clitter_time_rcp26,veg_clitter_rcp26,input$year_disturbance)
    retreive_Cbiomass_rcp26_AG<-input$AG_biomass
    retreive_Cbiomass_rcp26_BG<-input$BG_biomass
    
    Cinput_disturbance_vec<- calculate_Cin_mortality_event(retreive_Cinput_ag_rcp26_fix,retreive_Cinput_bg_rcp26_fix,
                                                           retreive_Cbiomass_rcp26_AG, retreive_Cbiomass_rcp26_BG,input$MR_slider,input$HR_slider)
    Cinput_disturbance_ag<-Cinput_disturbance_vec[[1]]
    Cinput_disturbance_bg<-Cinput_disturbance_vec[[2]]
    
    Cinput_disturbance<-Cinput_disturbance_ag+Cinput_disturbance_bg
    
    print("Cinput_disturbance")
    print(Cinput_disturbance)
    
    AWEN_input = c(as.numeric(input$A_pool),
                   as.numeric(input$W_pool),
                   as.numeric(input$E_pool),
                   as.numeric(input$N_pool),
                   0)
    
    #Suppose W and E litter compounds are more labile
    decomposable_plant_material <-as.numeric(input$W_pool)+as.numeric(input$E_pool)
    #Suppose A and N litter compounds are more resistant
    resistant_plant_material <-as.numeric(input$A_pool)+as.numeric(input$N_pool)
    #Decomposable to resistant ratio
    DR_in = decomposable_plant_material/resistant_plant_material
    
    #CHANGE
    computation_time_step_fwd = 1
    simulation_length_LMscenario = simulation_length()
    
    ######
    print("Preparing to run control land management scenario")
    ######
    #This will launch the function defined in "Holisoils_multimodel_v1.R"
    test_mm_control_LM <-Call_MULTIMODEL_i1(plot_figures=plot_figures,simulation_length=simulation_length_LMscenario, spinup_length=spinup_length,
                                            computation_time_step_fwd=computation_time_step_fwd,
                                            start_date_simulations=dates_in(),
                                            temperature_spinup=Temp_day_spinup, precipitation_spinup=Precip_day_spinup, potential_evapotranspiration_spinup=Potevap_month_spinup,
                                            soilmoisture_spinup=as.numeric(Vswc_day_spinup$Vswc),
                                            temperature_fwd=Temp_day, precipitation_fwd=Precip_day, potential_evapotranspiration_fwd=Potevap_month,soilmoisture_fwd=as.numeric(Vswc_day$Vswc),
                                            SOC_0=input$SOC,
                                            C_input_ag_spinup=as.numeric(mean(retreive_Cinput_ag_rcp26_fix)),C_input_bg_spinup=as.numeric(mean(retreive_Cinput_bg_rcp26_fix)),
                                            C_input_ag_fwd=as.numeric(retreive_Cinput_ag_rcp26_fix),C_input_bg_fwd=as.numeric(retreive_Cinput_bg_rcp26_fix),
                                            clay_p=input$clay_slider2,silt_p=input$silt,soil_thickness=input$soilthick,pH_p=ph_site,
                                            lignin_to_nitrogen=input$LNratio,structural_in_lignin=input$SLratio,woodylittersize=input$WLS,AWEN_in=AWEN_input,decomp_to_resist_ratio=DR_in,
                                            CN_Ratio=input$CNratio, Bulk_Density=input$bulkdensity, WFPS=water_filled_pore_space, CH4_Conc=input$CH4_data,
                                            decomposition_param_RothC=ksRothC,
                                            decomposition_param_ICBM=param_ICBM,
                                            decomposition_param_Century=ksCent,
                                            decomposition_param_Yasso07=paramYasso07,
                                            decomposition_param_Yasso20=paramYasso20)
    
    print("End of multimodel simulation Control LM")
    print(" ")
    print("...Plotting control LM completed...")
    
    t_fwd_col = seq.int(1,simulation_length_LMscenario,by=computation_time_step_fwd)
    
    
    data_mmctrLM <- get_data_from_multimodel_run(test_mm_control_LM,t_fwd_col)
    minCctrLM=data_mmctrLM[[1]]
    maxCctrLM=data_mmctrLM[[2]]
    mmmean_totCctrLM=data_mmctrLM[[3]]
    totC_Roth_C_ctrLM=data_mmctrLM[[4]]
    totC_ICBM_C_ctrLM=data_mmctrLM[[5]]
    totC_Century_C_ctrLM=data_mmctrLM[[6]]
    totC_Yasso07_C_ctrLM=data_mmctrLM[[7]]
    totC_YASSO20_C_ctrLM=data_mmctrLM[[8]]
    minFctrLM=data_mmctrLM[[9]]
    maxFctrLM=data_mmctrLM[[10]]
    mmmean_totFctrLM=data_mmctrLM[[11]]
    totF_Roth_C_ctrLM=data_mmctrLM[[12]]
    totF_ICBM_C_ctrLM=data_mmctrLM[[13]]
    totF_Century_C_ctrLM=data_mmctrLM[[14]]
    totF_Yasso07_C_ctrLM=data_mmctrLM[[15]]
    totF_YASSO20_C_ctrLM=data_mmctrLM[[16]]
    
    totF_SG_C_ctrLM=data_mmctrLM[[17]]
    totCH4_SG_C_ctrLM=data_mmctrLM[[18]]
    totN2O_SG_C_ctrLM=data_mmctrLM[[19]]
    
    ###########################    ###########################
    print("Preparing to run disturbance event scenario")
    ###########################    ###########################
    #This will launch the function defined in "Holisoils_multimodel_v1.R"
    test_mm_disturbance_LM <- Call_MULTIMODEL_i1(plot_figures=plot_figures,simulation_length=simulation_length_LMscenario, spinup_length=spinup_length,
                                                 computation_time_step_fwd=computation_time_step_fwd,
                                                 start_date_simulations=dates_in(),
                                                 temperature_spinup=Temp_day_spinup, precipitation_spinup=Precip_day_spinup, potential_evapotranspiration_spinup=Potevap_month_spinup,
                                                 soilmoisture_spinup=as.numeric(Vswc_day_spinup$Vswc),
                                                 temperature_fwd=Temp_day, precipitation_fwd=Precip_day, potential_evapotranspiration_fwd=Potevap_month,soilmoisture_fwd=as.numeric(Vswc_day$Vswc),
                                                 SOC_0=input$SOC,
                                                 C_input_ag_spinup=as.numeric(mean(retreive_Cinput_ag_rcp26_fix)),C_input_bg_spinup=as.numeric(mean(retreive_Cinput_bg_rcp26_fix)),
                                                 C_input_ag_fwd=as.numeric(Cinput_disturbance_ag),C_input_bg_fwd=as.numeric(Cinput_disturbance_bg),
                                                 clay_p=input$clay_slider2,silt_p=input$silt,soil_thickness=input$soilthick,pH_p=ph_site,
                                                 lignin_to_nitrogen=input$LNratio,structural_in_lignin=input$SLratio,woodylittersize=input$WLS,AWEN_in=AWEN_input,decomp_to_resist_ratio=DR_in,
                                                 CN_Ratio=input$CNratio, Bulk_Density=input$bulkdensity, WFPS=water_filled_pore_space, CH4_Conc=input$CH4_data,
                                                 decomposition_param_RothC=ksRothC,
                                                 decomposition_param_ICBM=param_ICBM,
                                                 decomposition_param_Century=ksCent,
                                                 decomposition_param_Yasso07=paramYasso07,
                                                 decomposition_param_Yasso20=paramYasso20)
    
    print("End of multimodel simulation disturbance event scenario")
    print(" ")
    print("...Plotting disturbance event scenario completed...")
    
    data_mmdistLM <- get_data_from_multimodel_run(test_mm_disturbance_LM,t_fwd_col)
    minCdistLM=data_mmdistLM[[1]]
    maxCdistLM=data_mmdistLM[[2]]
    mmmean_totCdistLM=data_mmdistLM[[3]]
    totC_Roth_C_distLM=data_mmdistLM[[4]]
    totC_ICBM_C_distLM=data_mmdistLM[[5]]
    totC_Century_C_distLM=data_mmdistLM[[6]]
    totC_Yasso07_C_distLM=data_mmdistLM[[7]]
    totC_YASSO20_C_distLM=data_mmdistLM[[8]]
    minFdistLM=data_mmdistLM[[9]]
    maxFdistLM=data_mmdistLM[[10]]
    mmmean_totFdistLM=data_mmdistLM[[11]]
    totF_Roth_C_distLM=data_mmdistLM[[12]]
    totF_ICBM_C_distLM=data_mmdistLM[[13]]
    totF_Century_C_distLM=data_mmdistLM[[14]]
    totF_Yasso07_C_distLM=data_mmdistLM[[15]]
    totF_YASSO20_C_distLM=data_mmdistLM[[16]]
    
    totF_SG_C_distLM=data_mmdistLM[[17]]
    totCH4_SG_C_distLM=data_mmdistLM[[18]]
    totN2O_SG_C_distLM=data_mmdistLM[[19]]
    
    ###########################
    #Plot STOCKS COMPARISON CC
    ###########################
    express_plotC = expression("SOC stocks (MgC"~{ha}^{-1}~")")
    
    minC <- min(cbind(minCctrLM,minCdistLM))
    maxC <- max(cbind(maxCctrLM,maxCdistLM))
    
    t_fwd = seq(1,simulation_length_LMscenario,by=computation_time_step_fwd)
    
    plot(t_fwd, mmmean_totCctrLM, type="l", lty=1, lwd=3, xlab="Time (years)",ylab=" ",col="black",ylim=c(minC-5,maxC+5))
    lines(t_fwd,totC_Roth_C_ctrLM,type="l", lty=1, lwd=1, col=alpha("black",0.3))
    lines(t_fwd,totC_ICBM_C_ctrLM,type="l", lty=1,lwd=1, col=alpha("black",0.3))
    lines(t_fwd,totC_Century_C_ctrLM,type="l", lty=1,lwd=1, col=alpha("black",0.3))
    lines(t_fwd,totC_Yasso07_C_ctrLM,type="l", lty=1,lwd=1, col=alpha("black",0.3))
    lines(t_fwd,totC_YASSO20_C_ctrLM,type="l", lty=1,lwd=1, col=alpha("black",0.3))
    title(ylab=express_plotC,main="Multi-model SOC stocks",mgp=c(2,1,0))
    
    lines(t_fwd,mmmean_totCdistLM,type="l", lty=1,lwd=3, col="darkgreen")
    lines(t_fwd,totC_Roth_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    lines(t_fwd,totC_ICBM_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    lines(t_fwd,totC_Century_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    lines(t_fwd,totC_Yasso07_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    lines(t_fwd,totC_YASSO20_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    
    legend(par('usr')[2], par('usr')[4], c("Control", "Disturbance"),
           lty=1,lwd=c(3,3), col=c("black","darkgreen"), cex=0.8,xpd=NA,bty="n")
    
    #PLOT FLUXES COMPARISON CC
    express_plotF = expression(CO[2]~"flux (MgC"~{ha}^{-1}*{year}^{-1}~")")
    
    minF <- min(cbind(minFctrLM,minFdistLM))
    maxF <- max(cbind(maxFctrLM,maxFdistLM))
    
    plot(t_fwd, mmmean_totFctrLM, type="l", lty=1, lwd=3, xlab="Time (years)",ylab=" ",col="black",ylim=c(minF-2,maxF+2))
    lines(t_fwd,totF_Roth_C_ctrLM,type="l", lty=1, lwd=1, col=alpha("black",0.3))
    lines(t_fwd,totF_ICBM_C_ctrLM,type="l", lty=1,lwd=1, col=alpha("black",0.3))
    lines(t_fwd,totF_Century_C_ctrLM,type="l", lty=1,lwd=1, col=alpha("black",0.3))
    lines(t_fwd,totF_Yasso07_C_ctrLM,type="l", lty=1,lwd=1, col=alpha("black",0.3))
    lines(t_fwd,totF_YASSO20_C_ctrLM,type="l", lty=1,lwd=1, col=alpha("black",0.3))
    lines(t_fwd,totF_SG_C_ctrLM,type="l", lty=1,lwd=1, col=alpha("black",0.3))
    title(ylab=express_plotF,main= expression(bold("Multi-model "~CO[2]~"fluxes")),mgp=c(2,1,0))
    
    lines(t_fwd,mmmean_totFdistLM,type="l", lty=1,lwd=3, col="darkgreen")
    lines(t_fwd,totF_Roth_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    lines(t_fwd,totF_ICBM_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    lines(t_fwd,totF_Century_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    lines(t_fwd,totF_Yasso07_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    lines(t_fwd,totF_YASSO20_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    lines(t_fwd,totF_SG_C_distLM,type="l", lty=1,lwd=1, col=alpha("darkgreen",0.3))
    
    legend(par('usr')[2], par('usr')[4], c("Control", "Disturbance"),
           lty=1,lwd=c(3,3), col=c("black","darkgreen"), cex=0.8,xpd=NA,bty="n")
    
    
    
    
    ########################
    ########################
    #Create data for download
    data_download_LM <-create_dataframe_for_download(t_fwd,
                                                     data_mmctrLM,
                                                     data_mmdistLM,
                                                     "Control",
                                                     "Disturbance")
    # Filter data based on user input
    filtered_data_LM <- reactive({
      
      subset(data_download_LM,data_download_LM$Variable== input$Select_variable3
             & data_download_LM$Scenario== input$Select_RCP3
             & data_download_LM$Model== input$Select_model3)
    })
    
    observe({
      category_choices <- subset(data_download_LM,data_download_LM$Variable== input$Select_variable3)
      updateSelectInput(session, "Select_model3", choices = unique(category_choices$Model))
    })
    
    #Show data in table
    output$outTable_LM <- renderDT({
      datatable(filtered_data_LM(), options = list(pageLength = 10),
                rownames = FALSE)
    })
    
    # Downloadable csv of selected dataset ----
    output$downloadData_LM <- downloadHandler(
      filename = function() {
        paste("data_", input$Select_variable3, "_", input$Select_RCP3, "_", input$Select_model3, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(filtered_data_LM(), file, row.names = FALSE)
      }
    )
    
    
  }) #renderPlot
  
  
  
  #--------------------------------------------------------------------------------
  #Creates url links for all references
  #---------------------------------------------------------------------------------
  
  url1 <- a(h4(style = "display: inline;","(Viskari et al., 2022)"), 
            href="https://gmd.copernicus.org/articles/15/1735/2022/",target = "_blank")
  output$tabYasso20 <- renderUI({
    tagList(h4(style = "display: inline;","● Yasso20"), url1)
  })
  
  url2 <- a(h4(style = "display: inline;","(Andrén and Kätterer, 1997)"), 
            href="https://www.jstor.org/stable/2641210#metadata_info_tab_contents",target = "_blank")
  
  output$tabICBM <- renderUI({
    tagList(h4("● ICBM", style = "display: inline;"), url2)
  })
  
  url3 <- a(h4(style = "display: inline;","(Parton et al., 1988)"), 
            href="https://link.springer.com/article/10.1007/BF02180320",target = "_blank")
  output$tabCentury <- renderUI({
    tagList(h4(style = "display: inline;","● Century"), url3)
  })
  
  url4 <- a(h4(style = "display: inline;","(Coleman and Jenkinson, 1996)"), 
            href="https://link.springer.com/chapter/10.1007/978-3-642-61094-3_17",target = "_blank")
  output$tabRothC <- renderUI({
    tagList(h4(style = "display: inline;","● Roth-C"), url4)
  })
  
  url5 <- a(h4(style = "display: inline;","(Tuomi et al., 2009)"), 
            href="https://linkinghub.elsevier.com/retrieve/pii/S030438000900386X",target = "_blank")
  output$tabYasso <- renderUI({
    tagList(h4(style = "display: inline;","● Yasso07"), url5)
  })
  
  
  url6 <- a(h4(style = "display: inline;","(Hashimoto et al., 2011)"), 
            href="https://www.sciencedirect.com/science/article/abs/pii/S0304380011000457?via%3Dihub",target = "_blank")
  output$tabSG <- renderUI({
    tagList(h4(style = "display: inline;","● SG        "), url6)
  })
  
  urlDimassi <- a(h4(style = "display: inline;","(Dimassi et al., 2018)."), 
                  href="https://doi.org/10.1016/j.geoderma.2017.09.038",target = "_blank")
  output$tabDimassi <- renderUI({
    tagList(HTML("<h4 style='text-align: justify;display: inline;'>In order to match the observed total SOC stocks, we <span class='bolder-text'>rescale the total simulated SOC</span> using the value of measured SOC provided by the user,
                              and we keep the same estimated proportion of C in each pool as at steady-state </h4>"),urlDimassi)
  })
  
  urlSoilR <- a(h4(style = "display: inline;","SoilR"), 
                href="https://cran.r-project.org/web/packages/SoilR/SoilR.pdf",target = "_blank")
  output$tabSoilR <- renderUI({
    tagList(h4(style = "display: inline;","More information on the resolution technique and functions used can be found in the  "),
            urlSoilR, h4(style = "display: inline;"," documentation."))
  })
  
  
  #--
  url1.1 <- a(h5(style = "display: inline;","https://doi.org/10.5194/gmd-15-1735-2022",
                 href="https://gmd.copernicus.org/articles/15/1735/2022/"),target = "_blank")
  
  output$tabYasso20.1 <- renderUI({
    tagList(h5(style = "display: inline;","Viskari, T., Janne P., Istem F., Anna R., Julius V., and Jari L.
               Calibrating the Soil Organic Carbon Model Yasso20 with Multiple Datasets. 
               Geoscientific Model Development 15, no. 4 (March 2, 2022): 1735–52. "),url1.1)
  })
  
  url2.1 <- a(h5(style = "display: inline;"," 
              https://doi.org/10.1890/1051-0761(1997)007[1226:ITICBM]2.0.CO;2"), 
              href="https://www.jstor.org/stable/2641210#metadata_info_tab_contents",target = "_blank")
  
  output$tabICBM.1 <- renderUI({
    tagList(h5(style = "display: inline;","Andrén, O. and Kätterer, T.: 
              ICBM: THE INTRODUCTORY CARBON BALANCE MODEL FOR EXPLORATION OF SOIL CARBON BALANCES, 
            Ecological Applications, 7, 1226–1236, 1997."),url2.1)
  })
  
  url3.1 <- a(h5(style = "display: inline;","https://doi.org/10.1007/BF02180320"), 
              href="https://link.springer.com/article/10.1007/BF02180320",target = "_blank")
  
  output$tabCentury.1 <- renderUI({
    tagList(h5(style = "display: inline;","Parton, W. J., Stewart, J. W. B., and Cole, C. V.: Dynamics of C, N, P and S in grassland soils: a model, 
            Biogeochemistry, 5, 109–131, 1988. "),url3.1)
  })
  
  url4.1 <- a(h5(style = "display: inline;"," https://doi.org/10.1007/978-3-642-61094-3_17"), 
              href="https://link.springer.com/chapter/10.1007/978-3-642-61094-3_17",target = "_blank")
  
  output$tabRothC.1 <- renderUI({
    tagList(h5(style = "display: inline;", "Coleman, K. and Jenkinson, D. S.: RothC-26.3 - 
              A Model for the turnover of carbon in soil, in: 
            Evaluation of Soil Organic Matter Models, edited by: 
            Powlson, D. S., Smith, P., and Smith, J. U., Springer Berlin Heidelberg, 
            Berlin, Heidelberg, 237–246, 1996. "),url4.1)
  })
  
  url5.1 <- a(h5(style = "display: inline;","https://doi.org/10.1016/j.ecolmodel.2009.05.016"),
              href="https://linkinghub.elsevier.com/retrieve/pii/S030438000900386X",target = "_blank")
  
  output$tabYasso.1 <- renderUI({
    tagList(h5(style = "display: inline;","Tuomi, M., Thum, T., Järvinen, H., Fronzek, S., Berg, B., 
              Harmon, M., Trofymow, J. A., Sevanto, S., and Liski, J.: 
            Leaf litter decomposition—Estimates of global variability based on Yasso07 
            model, Ecological Modelling, 220,
            3362–3371, 2009. "),url5.1)
  })
  
  url6.1 <- a(h5(style = "display: inline;","https://doi.org/10.1016/j.ecolmodel.2011.01.013"), 
              href="https://www.sciencedirect.com/science/article/abs/pii/S0304380011000457?via%3Dihub",target = "_blank")
  output$tabSG.1 <- renderUI({
    tagList(h5(style = "display: inline;","Hashimoto, S., Morishita, T., Sakata, T., Ishizuka, S., Kaneko, S., and Takahashi, M.: 
              Simple models for soil CO2, CH4, and N2O fluxes calibrated using 
            a Bayesian approach and multi-site data, Ecological Modelling, 11, 2011. "), url6.1)
  })
  
  urlSierra <- a(h5(style = "display: inline;","https://doi.org/10.5194/gmd-5-1045-2012"), 
                 href="https://www.sciencedirect.com/science/article/abs/pii/S0304380011000457?via%3Dihub",target = "_blank")
  output$tabSierra <- renderUI({
    tagList(h5(style = "display: inline;","Sierra, C. A., Müller, M., and 
                Trumbore, S. E.: Models of soil organic matter decomposition: the 
            SoilR package, version 1.0, Geosci. Model Dev., 5, 
            1045–1060, 2012. "), urlSierra)
  })
  
  urlDimassi.1 <- a(h5(style = "display: inline;","https://doi.org/10.1016/j.geoderma.2017.09.038"), 
                    href="https://doi.org/10.1016/j.geoderma.2017.09.038",target = "_blank")
  output$tabDimassi.1 <- renderUI({
    tagList(h5(style = "display: inline;","Dimassi, Bassem, Bertrand Guenet, Nicolas P.A. Saby, Facundo Munoz, Marion Bardy, 
Florent Millet, and Manuel P. Martin. “The Impacts of CENTURY Model 
Initialization Scenarios on Soil Organic Carbon Dynamics Simulation 
in French Long-Term Experiments.” 
            Geoderma 311, 2018."), urlDimassi.1)
  })
  
  urlISIMIP <- a(h5(style = "display: inline;","doi.org/10.5194/gmd-10-4321-2017"),
                 href="https://doi.org/10.5194/gmd-10-4321-2017",target = "_blank")
  
  urlISIMIP_table <- "<a href='https://data.isimip.org/' target='_blank'>data.isimip.org</a>"
  
  output$tabISIMIP <- renderUI({
    tagList(h5(style = "display: inline;",
               "Frieler, Katja, Stefan Lange, Franziska Piontek, Christopher P. O. Reyer, Jacob Schewe, 
    Lila Warszawski, Fang Zhao, et al. “Assessing the Impacts of 1.5 °C Global Warming
    – Simulation Protocol of the Inter-Sectoral Impact Model Intercomparison Project (ISIMIP2b).”
    Geoscientific Model Development 10(12), 4321–45, 2017."), urlISIMIP)
  })
  
  urlTRY <- a(h5(style = "display: inline;","doi:10.1111/gcb.14904"),
              href="https://onlinelibrary-wiley-com.insu.bib.cnrs.fr/doi/full/10.1111/gcb.14904",target = "_blank")
  
  urlTRY_table <- "<a href='https://www.try-db.org/TryWeb/dp.php'target='_blank'>try-db.org</a>"
  
  output$tabTRY <- renderUI({
    tagList(h5(style = "display: inline;",
               "Kattge, J., Bönisch, G., Díaz, S., Lavorel, S., Prentice, I. C., Leadley, P., 
                Tautenhahn, S., Werner, G., et al. 
                TRY plant trait database - enhanced coverage and open access. 
                Global Change Biology, 26(1), 119-188, 2017."), urlTRY)
  })
  
  
  urlLUCAS.1_table <- "<a href='https://esdac.jrc.ec.europa.eu/projects/lucas' target='_blank'>esdac.jrc.ec.europa.eu</a>"
  
  urlLUCAS.1 <- a(h5(style = "display: inline;","doi.org/10.1016/j.geoderma.2015.07.006"),
                  href="https://doi.org/10.1016/j.geoderma.2015.07.006",target = "_blank")
  
  output$tabLUCAS.1 <- renderUI({
    tagList(h5(style = "display: inline;",
               "Ballabio C., Panagos P., Montanarella L. Mapping topsoil physical properties at 
                European scale using the LUCAS database. Geoderma, 261 , pp. 110-123, 2016."), urlLUCAS.1)
  })
  
  urlLUCAS.2 <- a(h5(style = "display: inline;","doi.org/10.1016/j.geoderma.2019.113912"), 
                  href="https://doi.org/10.1016/j.geoderma.2019.113912",target = "_blank")
  
  output$tabLUCAS.2 <- renderUI({
    tagList(h5(style = "display: inline;",
               "Ballabio, C., Lugato, E., Fernández-Ugalde, O., Orgiazzi, A., Jones, A., 
                Borrelli, P., Montanarella, L. and Panagos, P. Mapping LUCAS topsoil chemical properties 
                at European scale using Gaussian process regression. Geoderma, 355: 113912, 2019."), urlLUCAS.2)
  })
  
  urlLUCAS.3 <- a(h5(style = "display: inline;","doi:10.1111/ejss.12193"), 
                  href="https://bsssjournals-onlinelibrary-wiley-com.insu.bib.cnrs.fr/doi/full/10.1111/ejss.12193"
                  ,target = "_blank")
  
  output$tabLUCAS.3 <- renderUI({
    tagList(h5(style = "display: inline;",
               "D. de Brogniez, C. Ballabio, A. Stevens, R. J. A. Jones, L. Montanarella
                and B. van Wesemael. A map of the topsoil organic carbon content of 
                Europe generated by a generalized additive model. European Journal of Soil Science.
                66(1): 121-134, 2015."), urlLUCAS.3)
  })
  
  urlLUCAS.4 <- a(h5(style = "display: inline;","dx.doi.org/10.1016/j.scitotenv.2016.03.085"), 
                  href="http://dx.doi.org/10.1016/j.scitotenv.2016.03.085",target = "_blank")
  
  output$tabLUCAS.4 <- renderUI({
    tagList(h5(style = "display: inline;",
               "Yigini, Y., Panagos, P. Assessment of soil organic carbon stocks 
                under future climate and land cover changes in Europe. 
                Science of The Total Environment. 557–558, 838–850, 2016."), urlLUCAS.4)
  })
  
  
  
  urlplant_cont <- a(h5(style = "display: inline;","doi.org/10.5194/bg-15-693-2018."), 
                     href="https://doi.org/10.5194/bg-15-693-2018.",target = "_blank")
  
  output$tabplant_cont <- renderUI({
    tagList(h5(style = "display: inline;",
               "Ma, Suhui, Feng He, Di Tian, Dongting Zou, Zhengbing Yan, Yulong Yang, 
                Tiancheng Zhou, Kaiyue Huang, Haihua Shen, and Jingyun Fang. 
                “Variations and Determinants of Carbon Content in Plants: 
                A Global Synthesis.” Biogeosciences. 15(3), 693–702, 2018."), urlplant_cont)
  })
  
  
  
  urlGML_table <- "<a href='https://doi.org/10.15138/P8XG-AA10' target='_blank'>gml.noaa.gov</a>"
  
  urlGML <- a(h5(style = "display: inline;","doi.org/10.15138/P8XG-AA10"),
              href="https://doi.org/10.15138/P8XG-AA10",target = "_blank")
  
  output$tabGML <- renderUI({
    tagList(h5(style = "display: inline;",
               "Lan, X., K.W. Thoning, and E.J. Dlugokencky. 
                Trends in globally-averaged CH4, N2O, and SF6 determined from 
                NOAA Global Monitoring Laboratory measurements. Version 2023-11, 2022."), urlGML)
  })
  
  urlIPCC_AR5<-a(h4(style = "display: inline;","Fifth Assessment Report (AR5)"), 
                 href="https://www.ipcc.ch/site/assets/uploads/2018/04/ipcc_ar5_leaflet.pdf",target = "_blank")
  
  output$tabIPCC_AR5 <- renderUI({
    tagList(HTML("<h4 style='text-align: justify;display: inline;'>
     The climate change scenarios are built from the representative concentration pathways (RCPs) 
     described in the </h4>"),
            urlIPCC_AR5,
            HTML("<h4 style='text-align: justify;display: inline;'>
             of the Intergovernmental Panel on Climate Change (IPCC). 
             In particular, we focus on the <span class='bolder-text'>RCP 2.6</span>
             and <span class='bolder-text'>RCP 6.0</span>, which predict an average global land temperature 
             increase of 1˚C and 2.2˚C during the period 2081-2100, 
             compared to mean temperatures in 1986-2005, respectively. 
             To achieve this, we force the models with climate and vegetation biomass data 
             produced by model simulations of the ISIMIP project under the two RCPs.</h4>"))
  })
  
  output$tabIPCC_AR5_ref <- renderUI({
    tagList(h5(style = "display: inline;",
               "REF IPCC"), urlIPCC_AR5)
  })
  
  urlIPCC_AR6<-a(h4(style = "display: inline;","Sixth Assessment Report (AR6)."), 
                 href=" https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter04.pdf",target = "_blank")
  
  output$tabIPCC_AR6 <- renderUI({
    tagList(HTML("<h4 style='text-align: justify;display: inline;'>
     The land-use scenarios are derived from the shared socio-economic pathways (SSPs)
                            defined in the IPCC
                  </h4>"),
            urlIPCC_AR6,
            HTML("<h4 style='text-align: justify;display: inline;'>
             In particular, we focus on the <span class='bolder-text'>SSP2</span>, which contemplates social, 
                            economic and technological trends that do not shift markedly from historical patterns.
                            To achieve this, we force the models with vegetation biomass data produced by model 
                            simulations of the ISIMIP project under a fixed year-2005 land-use,
                            nitrogen deposition and fertilizer input,
                            and a varying land use, water abstraction, nitrogen deposition and fertilizer input 
                            according to SSP2 and both RCPs.</h4>"))
  })
  
  output$tabIPCC_AR6_ref <- renderUI({
    tagList(h5(style = "display: inline;",
               "REF IPCC"), urlIPCC_AR6)
  })
  
  url_CClicence <- a(h5(style = "display: inline;","CC BY-SA 4.0"), 
                     href="https://creativecommons.org/licenses/by-sa/4.0/",target = "_blank")
  
  output$CClicence <- renderUI({
    tagList(
      HTML("<h5 style='text-align: justify;display: inline;'>Credits: </h5>"), url_CClicence,
      HTML("<h5><span class='bolder-text'>Figure </span>
                             Predictions of CO<sub>2</sub> concentration patterns 
                             in the 21<sup>st</sup> century, 
                             according to the IPCC representative concentration pathways.
                             </h5>"))
  })
  
  output$CClicence2 <- renderUI({
    tagList(
      HTML("<h5 style='text-align: justify;display: inline;'>Credits: </h5>"), url_CClicence,
      HTML("<h5><span class='bolder-text'>Figure </span>
                             Representation of the shared IPCC socio-economic pathways in terms
                             of socio-economic challenges for mitigation versus adaptation.
                             </h5>"))
  })
  
  
  
  link_to_documentation<-tags$a(h4(style = "display: inline;","Documentation."), 
                                href="Documentation_multimodel_webtool.pdf",target = "_blank")
  
  output$Download_documentation <- renderUI({
    tagList(HTML("<h4 style='text-align: justify;display: inline;'>
                     The user is asked to input the geographic coordinates of the site of interest, 
                     and provide additional data on pedoclimatic and forest management conditions.
                     In case some of the data is not available, the webtool directly extracts the required data
                     from <span class='bolder-text'>open source databases</span>. Sources and references are listed in the table below and 
                     more information can be found in the </h4>"),link_to_documentation)
  })
  
  
  output$Download_documentation2 <- renderUI({
    tagList(HTML("<h4 style='text-align: justify;display: inline;'> 
    The land management scenario is based on user input data. It considers a disturbance event such as <span class='bolder-text'>cutting, thinning, fire, or tree disease</span>,
                            after which some of the vegetation dies and is partly removed from the soil.
                            The assumptions made to estimate the C input to the soil during the disturbance event
                            and for the rest of the years of the simulation are schematized below.
                            More information can be found in the </h4>"),link_to_documentation)
  })
  
  
  link_to_documentation3<-tags$a(h4("Download documentation"), 
                                 href="Documentation_multimodel_webtool.pdf",target = "_blank")
  output$Download_documentation3 <- renderUI({
    link_to_documentation3
  })
  
  #=======
  #Holisoils website
  #url7 <- a(h4("HoliSoils", style = "display: inline; font-family: courier;"), href="https://holisoils.eu/")
  url7 <- a(h4("HoliSoils", style = "display: inline;"), href="https://holisoils.eu/",target = "_blank")
  # output$tabHOLI <- renderUI({
  #   tagList(HTML("<h4 style='text-align: justify;display: inline;'>This webtool was designed within the "),
  #           url7,HTML("<h4 style='text-align: justify;display: inline;'> project framework."))
  # })
  
  output$tabHOLI2 <- renderUI({
    tagList(HTML("<h4 style='text-align: justify;display: inline;'>This webtool was designed within the "),
    url7,HTML("<h4 style='text-align: justify;display: inline;'> - Holistic management practices, 
         modelling and monitoring European forest soils - project framework. Holisoils 
         is a <span class='bolder-text'>Horizon 2020</span> project that
         aims to harmonise available soil monitoring information
         in European forests to support decision making towards 
         climate and sustainability goals.</h4>"))
  })
  
  url_githubcode <- a(h4("Code", style = "display: inline;"), 
                      href="https://github.com/elisabruni/Multimodel_interface",target = "_blank")
  output$githubcode <- renderUI({
    url_githubcode
  })
  
  
}

#--------------------------#--------------------------#--------------------------#--------------------------
#------call the app ----------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------

shinyApp(ui,server)

#------END ----------#--------------------------#--------------------------#--------------------------
#--------------------------#--------------------------#--------------------------#--------------------------
