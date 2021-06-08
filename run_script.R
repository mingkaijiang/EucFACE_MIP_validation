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

### ORCHIDEE - no need for both models


### CABLE-POP
## current issue: unsure PFERT variable (current no variable made in the output)
translate_CABLP_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/CABLP/forest"))


### LPJ-GUESS-CNP - it seems that no need
translate_LPJGP_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/LPJGP/trunk/"))
translate_LPJGP_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/LPJGP/old_soil/"))

translate_LPJGN_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/LPJGN/trunk/"))
translate_LPJGN_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/LPJGN/old_soil/"))


### QUINCY - some unit conversions are needed


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

## tile_averaged




### LPJ-GUESS-CNP
## euc_ter
EucFACE_mass_balance_and_validation_script_LPJGP(mod.version="trunk", pft.group="euc_ter")
EucFACE_mass_balance_and_validation_script_LPJGP(mod.version="old_soil", pft.group="euc_ter")

## all_pft
EucFACE_mass_balance_and_validation_script_LPJGP(mod.version="trunk", pft.group="all_pft")
EucFACE_mass_balance_and_validation_script_LPJGP(mod.version="old_soil", pft.group="all_pft")


### LPJ-GUESS-CN
## euc_ter
EucFACE_mass_balance_and_validation_script_LPJGN(mod.version="trunk", pft.group="euc_ter")
EucFACE_mass_balance_and_validation_script_LPJGN(mod.version="old_soil", pft.group="euc_ter")

## all_pft
EucFACE_mass_balance_and_validation_script_LPJGN(mod.version="trunk", pft.group="all_pft")
EucFACE_mass_balance_and_validation_script_LPJGN(mod.version="old_soil", pft.group="all_pft")



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


### LPJ-GUESS-CNP - eucalyptus trees only
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/trunk/euc_ter"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/trunk/euc_ter"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/euc_ter"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/old_soil/euc_ter"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### LPJ-GUESS-CNP all pft
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/trunk/all_pft"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/trunk/all_pft"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/all_pft"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/old_soil/all_pft"),
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


### LPJ-GUESS-POP - eucalyptus trees
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/euc_ter"),
                                                               mod.abb = "LPJGP",
                                                               out.dir = paste0(getwd(), "/analysis_output/LPJGP/euc_ter"),
                                                               eucDF = eucDF)


### LPJ-GUESS-POP - all pft
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/all_pft"),
                                                               mod.abb = "LPJGP",
                                                               out.dir = paste0(getwd(), "/analysis_output/LPJGP/all_pft"),
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


### LPJ-GUESS-CNP - eucalyptus trees only
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/euc_ter"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/euc_ter"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/euc_ter"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/euc_ter"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/euc_ter"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/euc_ter"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### LPJ-GUESS-CNP - all pft
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/all_pft"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/all_pft"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/all_pft"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/all_pft"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/all_pft"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/analysis_output/LPJGP/all_pft"),
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
### 4. Add CABLE-POP both forest and tile-averaged
### 5. Include dynamic vegetation analysis: CABLE-POP and LPJ-GUESS
### 6. Add ELM
### 7. MIP figures on CO2 x P
### 8. MIP figures on CO2 x drought


##########################################################################
####
#### End.
####
##########################################################################