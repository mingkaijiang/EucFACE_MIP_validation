#### Script to batch process individual model simulation results
#### Note: This is the model simulation output analysis script
#### Checking model mass balance, CO2 response, P effect and water effect
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
#translate_GDAY_simulation_into_EucFACE_MIP_format(met.path ="/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/met_data",
#                                                  sim.path = "/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/outputs",
#                                                  out.path = "simulation_output")
#
#
#### GDAY-CN
#translate_GDAY_CN_simulation_into_EucFACE_MIP_format(met.path ="/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/met_data",
#                                                     sim.path = "simulation_output/GDAYN/default_output",
#                                                     out.path = "simulation_output")


### CABLE-POP
#translate_CABLP_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/CABLP/"), 
#                                                   pft.variable = "forest")
#translate_CABLP_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/CABLP/"), 
#                                                   pft.variable = "tile_averaged")

### LPJ-GUESS
#unzip_LPJ_GUESS_output(sourceDir=paste0('~/Downloads/2021-09-17_r9996'),
#                       destDir=paste0(getwd(), "/simulation_output"))

### LPJ-GUESS-CNP rename file name
#translate_LPJGP_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/LPJGP/new_soil/"))

#translate_LPJGN_simulation_into_EucFACE_MIP_format(source.dir = paste0(getwd(), "/simulation_output/LPJGN/new_soil/"))


### QUINCY forcing data to convert units
#translate_QUINC_simulation_into_EucFACE_MIP_format(source.dir = "simulation_output/QUINC/")
#translate_QUJSM_simulation_into_EucFACE_MIP_format(source.dir = "simulation_output/QUJSM/")


### ELM output error correction
# the model output CFLITA = -9999, which should be revised using CFLIT
# then CFLIT is the total of CFLITA + CFLITB. Same for N and P 
# one additional variable XR, which is the turnover rate of C from storage C pool
#translate_ELMV1_simulation_into_EucFACE_MIP_format(source.dir = "simulation_output/ELMV1/")


### ORCHIDEE: 
### add PFERT variable
### revise PMIN: which currently includes PBIOCHMIN (gross) and net biological mineralization
###              redo the calculation so that PMIN is net biological mineralization (i.e. old PMIN - PBIOCHMIN)
### for OCRHIDEE-MICROBE: we have additional variables on microbes. 

#translate_ORCHD_simulation_into_EucFACE_MIP_format(mod.abb="OCHDP")
#translate_ORCHD_simulation_into_EucFACE_MIP_format(mod.abb="OCHDX")






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
EucFACE_mass_balance_and_validation_script_CABLP(forest = T)

## tile_averaged
EucFACE_mass_balance_and_validation_script_CABLP(forest = F)


### QUINCY, i.e. QUINC
EucFACE_mass_balance_and_validation_script_QUINC()

### QUINCY-JSM, i.e. QUJSM
EucFACE_mass_balance_and_validation_script_QUJSM()


### LPJ-GUESS-CNP
## euc_ter
EucFACE_mass_balance_and_validation_script_LPJGP(mod.version="new_soil", 
                                                 pft.group="euc_ter")

## all_pft
EucFACE_mass_balance_and_validation_script_LPJGP(mod.version="new_soil", 
                                                 pft.group="all_pft")


### LPJ-GUESS-CN
## euc_ter
EucFACE_mass_balance_and_validation_script_LPJGN(mod.version="new_soil", 
                                                 pft.group="euc_ter")

## all_pft
EucFACE_mass_balance_and_validation_script_LPJGN(mod.version="new_soil",
                                                 pft.group="all_pft")


### ELMV1
# summary of ELM: retranslocation flux needed
# respiration flux needed to check
# litter pool check
EucFACE_mass_balance_and_validation_script_ELMXX()



##########################################################################
#### Step 4: Plot CO2 response ratios over observed period

### GDAY-CNP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/GDAYP"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### GDAY-CN
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                             mod.abb = "GDAYN",
                                             out.dir = paste0(getwd(), "/output/analysis_output/GDAYN"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")

### ORCHIDEE-CNP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                             mod.abb = "OCHDP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/OCHDP"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")

### ORCHIDEE-MIC
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                             mod.abb = "OCHDX",
                                             out.dir = paste0(getwd(), "/output/analysis_output/OCHDX"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### CABLE-POP - forest
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/CABLP/forest"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")

### CABLE-POP - tile_averaged
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/tile_averaged"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/CABLP/tile_averaged"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### LPJ-GUESS-CNP - eucalyptus trees only
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/euc_ter"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/euc_ter"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")



### LPJ-GUESS-CNP all pft
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/all_pft"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/all_pft"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")



### LPJ-GUESS-CN - eucalyptus trees only
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/new_soil/euc_ter"),
                                             mod.abb = "LPJGN",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/new_soil/euc_ter"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")



### LPJ-GUESS-CN all pft
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/new_soil/all_pft"),
                                             mod.abb = "LPJGN",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/new_soil/all_pft"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### QUINCY
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/output/analysis_output/QUINC"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### QUINCY-JSM
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                             mod.abb = "QUJSM",
                                             out.dir = paste0(getwd(), "/output/analysis_output/QUJSM"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


### ELMV1
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/ELMV1"),
                                             mod.abb = "ELMV1",
                                             out.dir = paste0(getwd(), "/output/analysis_output/ELMV1"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


##########################################################################
#### Step 5: compare observed period simulation results with observation data, focus on CO2 response
### Prepare EucFACE observation datasets.
### Take codes from EucFACE C and nutrient budget assessment;
### don't use any unpublished data to compromise upcoming data-based publications.
eucDF <- prepare_EucFACE_observation_dataset(ignore.understorey=F)

### GDAY-CNP
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                                               mod.abb = "GDAYP",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/GDAYP"),
                                                               eucDF = eucDF)

### GDAY-CN
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                                               mod.abb = "GDAYN",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/GDAYN"),
                                                               eucDF = eucDF)


### ORCHIDEE-CNP
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                                               mod.abb = "OCHDP",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/OCHDP"),
                                                               eucDF = eucDF)

### ORCHIDEE-mic
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                                               mod.abb = "OCHDX",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/OCHDX"),
                                                               eucDF = eucDF)

### CABLE-POP - forest
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                                               mod.abb = "CABLP",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/CABLP/forest"),
                                                               eucDF = eucDF)


### CABLE-POP - tile_averaged
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/tile_averaged"),
                                                               mod.abb = "CABLP",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/CABLP/tile_averaged"),
                                                               eucDF = eucDF)


### LPJ-GUESS-CNP - eucalyptus trees
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/euc_ter"),
                                                               mod.abb = "LPJGP",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/euc_ter"),
                                                               eucDF = eucDF)

#plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/euc_ter"),
#                                                               mod.abb = "LPJGP",
#                                                               out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/old_soil/euc_ter"),
#                                                               eucDF = eucDF)


### LPJ-GUESS-CNP - all pft
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/all_pft"),
                                                               mod.abb = "LPJGP",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/all_pft"),
                                                               eucDF = eucDF)

#plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/all_pft"),
#                                                               mod.abb = "LPJGP",
#                                                               out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/old_soil/all_pft"),
#                                                               eucDF = eucDF)



### LPJ-GUESS-CN - eucalyptus trees
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/new_soil/euc_ter"),
                                                               mod.abb = "LPJGN",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/new_soil/euc_ter"),
                                                               eucDF = eucDF)

#plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/old_soil/euc_ter"),
#                                                               mod.abb = "LPJGN",
#                                                               out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/old_soil/euc_ter"),
#                                                               eucDF = eucDF)


### LPJ-GUESS-CN - all pft
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/new_soil/all_pft"),
                                                               mod.abb = "LPJGN",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/new_soil/all_pft"),
                                                               eucDF = eucDF)

#plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/old_soil/all_pft"),
#                                                               mod.abb = "LPJGN",
#                                                               out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/old_soil/all_pft"),
#                                                               eucDF = eucDF)


### QUINCY
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                                               mod.abb = "QUINC",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/QUINC"),
                                                               eucDF = eucDF)


### QUINCY-JSM
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                                               mod.abb = "QUJSM",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/QUJSM"),
                                                               eucDF = eucDF)


### ELMV1
plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/ELMV1"),
                                                               mod.abb = "ELMV1",
                                                               out.dir = paste0(getwd(), "/output/analysis_output/ELMV1"),
                                                               eucDF = eucDF)


##########################################################################
#### Step 6: Plot CO2 response ratios over predicted period

### GDAY-CNP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/GDAYP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/GDAYP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYP"),
                                             mod.abb = "GDAYP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/GDAYP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### GDAY-CN
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                             mod.abb = "GDAYN",
                                             out.dir = paste0(getwd(), "/output/analysis_output/GDAYN"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                             mod.abb = "GDAYN",
                                             out.dir = paste0(getwd(), "/output/analysis_output/GDAYN"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/GDAYN"),
                                             mod.abb = "GDAYN",
                                             out.dir = paste0(getwd(), "/output/analysis_output/GDAYN"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### ORCHIDEE-CNP
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                             mod.abb = "OCHDP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/OCHDP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                             mod.abb = "OCHDP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/OCHDP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDP"),
                                             mod.abb = "OCHDP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/OCHDP"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")




### ORCHIDEE-mic
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                             mod.abb = "OCHDX",
                                             out.dir = paste0(getwd(), "/output/analysis_output/OCHDX"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                             mod.abb = "OCHDX",
                                             out.dir = paste0(getwd(), "/output/analysis_output/OCHDX"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/OCHDX"),
                                             mod.abb = "OCHDX",
                                             out.dir = paste0(getwd(), "/output/analysis_output/OCHDX"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### CABLE-POP - forest
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/CABLP/forest"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/CABLP/forest"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/forest"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/CABLP/forest"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### CABLE-POP - tile_averaged
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/tile_averaged"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/CABLP/tile_averaged"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/tile_averaged"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/CABLP/tile_averaged"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/CABLP/tile_averaged"),
                                             mod.abb = "CABLP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/CABLP/tile_averaged"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### LPJ-GUESS-CNP - eucalyptus trees only
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/euc_ter"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/euc_ter"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/euc_ter"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/euc_ter"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/euc_ter"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/euc_ter"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


#plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/euc_ter"),
#                                             mod.abb = "LPJGP",
#                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/old_soil/euc_ter"),
#                                             sim.period = "PRD",
#                                             nutrient.trt = "NOP")
#
#plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/euc_ter"),
#                                             mod.abb = "LPJGP",
#                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/old_soil/euc_ter"),
#                                             sim.period = "PRD",
#                                             nutrient.trt = "MDP")
#
#plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/euc_ter"),
#                                             mod.abb = "LPJGP",
#                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/old_soil/euc_ter"),
#                                             sim.period = "PRD",
#                                             nutrient.trt = "HIP")


### LPJ-GUESS-CNP - all pft
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/all_pft"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/all_pft"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/all_pft"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/all_pft"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/new_soil/all_pft"),
                                             mod.abb = "LPJGP",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/new_soil/all_pft"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


#plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/all_pft"),
#                                             mod.abb = "LPJGP",
#                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/old_soil/all_pft"),
#                                             sim.period = "PRD",
#                                             nutrient.trt = "NOP")
#
#plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/all_pft"),
#                                             mod.abb = "LPJGP",
#                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/old_soil/all_pft"),
#                                             sim.period = "PRD",
#                                             nutrient.trt = "MDP")
#
#plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGP/old_soil/all_pft"),
#                                             mod.abb = "LPJGP",
#                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGP/old_soil/all_pft"),
#                                             sim.period = "PRD",
#                                             nutrient.trt = "HIP")



### LPJ-GUESS-CN - eucalyptus trees only
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/new_soil/euc_ter"),
                                             mod.abb = "LPJGN",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/new_soil/euc_ter"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")



#plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/old_soil/euc_ter"),
#                                             mod.abb = "LPJGN",
#                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/old_soil/euc_ter"),
#                                             sim.period = "PRD",
#                                             nutrient.trt = "NOP")




### LPJ-GUESS-CN - all pft
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/new_soil/all_pft"),
                                             mod.abb = "LPJGN",
                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/new_soil/all_pft"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")



#plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/LPJGN/old_soil/all_pft"),
#                                             mod.abb = "LPJGN",
#                                             out.dir = paste0(getwd(), "/output/analysis_output/LPJGN/old_soil/all_pft"),
#                                             sim.period = "PRD",
#                                             nutrient.trt = "NOP")




### QUINCY
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/output/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/output/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/output/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### QUINCY-JSM
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                             mod.abb = "QUJSM",
                                             out.dir = paste0(getwd(), "/output/analysis_output/QUJSM"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                             mod.abb = "QUJSM",
                                             out.dir = paste0(getwd(), "/output/analysis_output/QUJSM"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUJSM"),
                                             mod.abb = "QUJSM",
                                             out.dir = paste0(getwd(), "/output/analysis_output/QUJSM"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


### ELMV1
plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/ELMV1"),
                                             mod.abb = "ELMV1",
                                             out.dir = paste0(getwd(), "/output/analysis_output/ELMV1"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/ELMV1"),
                                             mod.abb = "ELMV1",
                                             out.dir = paste0(getwd(), "/output/analysis_output/ELMV1"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/ELMV1"),
                                             mod.abb = "ELMV1",
                                             out.dir = paste0(getwd(), "/output/analysis_output/ELMV1"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")



##########################################################################
##########################################################################
##########################################################################
####
#### End.
####
##########################################################################