#### Script to batch process all model simulation results
#### Note: This is the model simulation output analysis script, 
####       not the mass balance script (although it contains the mass balance checks). 
####
#### Author: Mingkai Jiang
####
##########################################################################
#### Step 1: basic set-up
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")

##########################################################################
#### Step 2: Run individual model mass balance checks

### GDAY-CNP
## convert model-specific output to MIP standard
translate_GDAY_simulation_into_EucFACE_MIP_format(met.path ="/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/met_data",
                                                  sim.path = "/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/outputs",
                                                  out.path = "simulation_output")

EucFACE_mass_balance_and_validation_script_GDAYP()


### ORCHIDEE-CNP
EucFACE_mass_balance_and_validation_script_OCHDP()

### ORCHIDEE-MIC
EucFACE_mass_balance_and_validation_script_OCHDX()



##########################################################################
#### Step 3: Plot CO2 response ratios over observed period

### ORCHIDEE-CNP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                             mod.abb = "OCHDP",
                                             out.dir = paste0(getwd(), "/analysis_output/OCHDP"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")

### ORCHIDEE-MIC
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                             mod.abb = "OCHDX",
                                             out.dir = paste0(getwd(), "/analysis_output/OCHDX"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")

### GDAY
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYP"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


##########################################################################
#### Step 3: Plot CO2 response ratios over predicted period

### GDAY




#### End.
####