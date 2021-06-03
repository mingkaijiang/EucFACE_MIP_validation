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


### GDAY-CN
translate_GDAY_CN_simulation_into_EucFACE_MIP_format(met.path ="/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/met_data",
                                                     sim.path = "simulation_output/GDAYN/default_output",
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

### GDAY-CN
EucFACE_mass_balance_and_validation_script_GDAYN()


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
EucFACE_mass_balance_and_validation_script_QUINC()

### QUINCY-JSM, i.e. QUJSM
EucFACE_mass_balance_and_validation_script_QUJSM()


##########################################################################
#### Step 4: Plot CO2 response ratios over observed period

### GDAY-CNP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYP"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### GDAY-CN
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                             mod.abb = "GDAYN",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYN"),
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

### QUINCY
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### QUINCY-JSM
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                             mod.abb = "QUJSM",
                                             out.dir = paste0(getwd(), "/analysis_output/QUJSM"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


##########################################################################
#### Step 5: compare observed period simulation results with observation data, focus on CO2 response
### Prepare EucFACE observation datasets.
### Take codes from EucFACE C and nutrient budget assessment;
### don't use any unpublished data to compromise upcoming data-based publications.
eucDF <- prepare_EucFACE_observation_dataset()

### GDAY-CNP
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                                               mod.abb = "GDAYP",
                                                               out.dir = paste0(getwd(), "/analysis_output/GDAYP"),
                                                               eucDF = eucDF)

### GDAY-CN
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                                               mod.abb = "GDAYN",
                                                               out.dir = paste0(getwd(), "/analysis_output/GDAYN"),
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


### QUINCY
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                                               mod.abb = "QUINC",
                                                               out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                                               eucDF = eucDF)


### QUINCY-JSM
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                                               mod.abb = "QUJSM",
                                                               out.dir = paste0(getwd(), "/analysis_output/QUJSM"),
                                                               eucDF = eucDF)


##########################################################################
#### Step 6: Plot CO2 response ratios over predicted period

### GDAY-CNP
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


### GDAY-CN
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                             mod.abb = "GDAYN",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYN"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                             mod.abb = "GDAYN",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYN"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                             mod.abb = "GDAYN",
                                             out.dir = paste0(getwd(), "/analysis_output/GDAYN"),
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


### QUINCY
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### QUINCY-JSM
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                             mod.abb = "QUJSM",
                                             out.dir = paste0(getwd(), "/analysis_output/QUJSM"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                             mod.abb = "QUJSM",
                                             out.dir = paste0(getwd(), "/analysis_output/QUJSM"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                             mod.abb = "QUJSM",
                                             out.dir = paste0(getwd(), "/analysis_output/QUJSM"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")

##########################################################################
#### Plot drought effect over historic period
###test

##########################################################################
#### Cross-model comparison, and against data

##########################################################################
### to do list:
### 1. Add LPJ-GUESS CN
### 2. check mass balance with individual modelers: QUINCY, LPJ-GUESS
### 3. Generate summary table on key model features
### 4. Add CABLE-POP
### 5. Include dynamic vegetation analysis: CABLE-POP and LPJ-GUESS
### 6. Add ELM
### 7. MIP figures on CO2 x P
### 8. MIP figures on CO2 x drought


##########################################################################
####
#### End.
####
##########################################################################