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
#### Step 2: Conver model-specific output into requested MIP output format

### GDAY-CNP
translate_GDAY_simulation_into_EucFACE_MIP_format(met.path ="/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/met_data",
                                                  sim.path = "/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/outputs",
                                                  out.path = "simulation_output")


### CABLE-POP
## current issue: unsure PFERT variable (current no variable made in the output)
translate_CABLP_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/CABLP/forest"))


### ORCHIDEE - no need for both models


### LPJ-GUESS-CNP - it seems that no need


##########################################################################
#### Step 3: Run individual model mass balance checks

### GDAY-CNP
EucFACE_mass_balance_and_validation_script_GDAYP()


### ORCHIDEE-CNP
EucFACE_mass_balance_and_validation_script_OCHDP()

### ORCHIDEE-MIC
EucFACE_mass_balance_and_validation_script_OCHDX()

### CABLE-POP 
## forest
EucFACE_mass_balance_and_validation_script_CABLP()

## entire tile


### LPJ-GUESS-CNP
## tree
EucFACE_mass_balance_and_validation_script_LPJGP()

## not sure what ter is, but included for now
EucFACE_mass_balance_and_validation_script_LPJGP_ter()


### LPJ-GUESS-CN

### QUINCY, i.e. QUINC

### QUINCY-JSM, i.e. QUJSM


##########################################################################
#### Step 4: Plot CO2 response ratios over observed period

### GDAY
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYP"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")

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


### CABLE-POP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/analysis_output/CABLP"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### LPJ-GUESS-CNP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


##########################################################################
#### Step 5: compare observed period simulation results with observation data, focus on CO2 response
### Prepare EucFACE observation datasets.
### Take codes from EucFACE C and nutrient budget assessment;
### don't use any unpublished data to compromise upcoming data-based publications.
eucDF <- prepare_EucFACE_observation_dataset()

### GDAY
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                                               mod.abb = "GDAYP",
                                                               out.dir = paste0(getwd(), "/analysis_output/GDAYP"),
                                                               eucDF = eucDF)

### ORCHIDEE-CNP
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                                               mod.abb = "OCHDP",
                                                               out.dir = paste0(getwd(), "/analysis_output/OCHDP"),
                                                               eucDF = eucDF)

### ORCHIDEE-mic
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                                               mod.abb = "OCHDX",
                                                               out.dir = paste0(getwd(), "/analysis_output/OCHDX"),
                                                               eucDF = eucDF)

### CABLE-POP
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                                               mod.abb = "CABLP",
                                                               out.dir = paste0(getwd(), "/analysis_output/CABLP"),
                                                               eucDF = eucDF)


### LPJ-GUESS-POP
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP"),
                                                               mod.abb = "LPJGP",
                                                               out.dir = paste0(getwd(), "/analysis_output/LPJGP"),
                                                               eucDF = eucDF)


##########################################################################
#### Step 6: Plot CO2 response ratios over predicted period

### GDAY
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")



### ORCHIDEE-CNP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                             mod.abb = "OCHDP",
                                             out.dir = paste0(getwd(), "/analysis_output/OCHDP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                             mod.abb = "OCHDP",
                                             out.dir = paste0(getwd(), "/analysis_output/OCHDP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                             mod.abb = "OCHDP",
                                             out.dir = paste0(getwd(), "/analysis_output/OCHDP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")




### ORCHIDEE-mic
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                             mod.abb = "OCHDX",
                                             out.dir = paste0(getwd(), "/analysis_output/OCHDX"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                             mod.abb = "OCHDX",
                                             out.dir = paste0(getwd(), "/analysis_output/OCHDX"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                             mod.abb = "OCHDX",
                                             out.dir = paste0(getwd(), "/analysis_output/OCHDX"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### CABLE-POP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/analysis_output/CABLP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/analysis_output/CABLP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/analysis_output/CABLP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### LPJ-GUESS-CNP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")

##########################################################################
#### Plot drought effect over historic period
###test

##########################################################################
#### Cross-model comparison, and against data


##########################################################################
####
#### End.
####
##########################################################################