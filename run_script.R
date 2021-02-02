#### Script to batch process all model simulation results
#### Mingkai Jiang
####
##########################################################################
#### Step 1: basic set-up
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")

##########################################################################
#### Step 2: Run individual model mass balance checks

### ORCHIDEE-MIC
EucFACE_mass_balance_and_validation_script_ORCHIDEE_MIC()

### ORCHIDEE-CNP
EucFACE_mass_balance_and_validation_script_ORCHIDEE_CNP()

### GDAY-CNP
translate_GDAY_simulation_into_EucFACE_MIP_format(met.path ="/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/met_data",
                                                  sim.path = "/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/outputs",
                                                  out.path = "simulation_output")

EucFACE_mass_balance_and_validation_script_GDAY()


##########################################################################
#### Step 3: Plot CO2 response ratios over observed period

### ORCHIDEE-CNP
plot_CO2_response_ratio_over_obs_period(source.dir=paste0(getwd(), "/simulation_output/ORCHIDEE"),
                                        mod.abb = "OCHDP",
                                        out.dir = paste0(getwd(), "/analysis_output"))

### ORCHIDEE-MIC
plot_CO2_response_ratio_over_obs_period(source.dir=paste0(getwd(), "/simulation_output/ORCHIDEE"),
                                        mod.abb = "OCHDX",
                                        out.dir = paste0(getwd(), "/analysis_output"))

### GDAY
plot_CO2_response_ratio_over_obs_period(source.dir=paste0(getwd(), "/simulation_output/ORCHIDEE"),
                                        mod.abb = "GDAYP",
                                        out.dir = paste0(getwd(), "/analysis_output"))



#### End.
####