#### Script to batch process QUINCY simulation analysis for EucFACE-MIP

rm(list=ls(all=TRUE))
setwd("~/SpiderOak Hive/not synced/EucFACE-MIP/EucFACE_MIP_validation/")

#### Source functions and packages
source("prepare.R")

##########################################################################
#### Step 1: Plot CO2 response ratios over observed period

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC/exp_EUC_QUINCY-auto-static_low-Init-P-v1_r2535"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP")


##########################################################################
#### Step 2: Plot CO2 absolute response over observed period for FIX and VAR climate scenario

plot_CO2_absolute_response_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC/exp_EUC_QUINCY-auto-static_low-Init-P-v1_r2535"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "OBS",
                                             nutrient.trt = "NOP",
                                             clim.trt = "VAR")

plot_CO2_absolute_response_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC/exp_EUC_QUINCY-auto-static_low-Init-P-v1_r2535"),
                                                mod.abb = "QUINC",
                                                out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                                sim.period = "OBS",
                                                nutrient.trt = "NOP",
                                                clim.trt = "FIX")


##########################################################################
#### Step 3: compare observed period simulation results with observation data, focus on CO2 response
### Prepare EucFACE observation datasets.

### Take codes from EucFACE C and nutrient budget assessment;
### don't use any unpublished data to compromise upcoming data-based publications.

# # this dataset is not in the folder yet
# eucDF <- prepare_EucFACE_observation_dataset()
# 
# ### QUINCY
# plot_CO2_response_comparison_against_data_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC"),
#                                                                mod.abb = "QUINC",
#                                                                out.dir = paste0(getwd(), "/analysis_output/QUINC"),
#                                                                eucDF = eucDF)

##########################################################################
#### Step 4: Plot CO2 response ratios over predicted period

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC/exp_EUC_QUINCY-auto-static_low-Init-P-v1_r2535"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC/exp_EUC_QUINCY-auto-static_low-Init-P-v1_r2535"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP")

plot_CO2_response_ratio_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC/exp_EUC_QUINCY-auto-static_low-Init-P-v1_r2535"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP")


##########################################################################
#### Step 5: Plot absolute CO2 response over predicted period for VAR climate scenario

plot_CO2_absolute_response_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC/exp_EUC_QUINCY-auto-static_low-Init-P-v1_r2535"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "NOP",
                                             clim.trt = "VAR")

plot_CO2_absolute_response_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC/exp_EUC_QUINCY-auto-static_low-Init-P-v1_r2535"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "MDP",
                                             clim.trt = "VAR")

plot_CO2_absolute_response_for_individual_model(source.dir=paste0(getwd(), "/simulation_output/QUINC/exp_EUC_QUINCY-auto-static_low-Init-P-v1_r2535"),
                                             mod.abb = "QUINC",
                                             out.dir = paste0(getwd(), "/analysis_output/QUINC"),
                                             sim.period = "PRD",
                                             nutrient.trt = "HIP",
                                             clim.trt = "VAR")


##########################################################################
#### Plot drought effect over historic period




##########################################################################
####
#### End.
####
##########################################################################