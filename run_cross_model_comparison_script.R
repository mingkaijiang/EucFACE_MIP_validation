#### Script to cross compare model results
#### Author: Mingkai Jiang
####
##########################################################################
#### Step 1: basic set-up
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")

##########################################################################
#### Cross-model comparison over the observed period (2013 - 19), with variable climate
#### Read in individual model results;
#### Perform conversion on selected variables;
#### Calculate multi-model means and variance;
#### Merge together with observed data;
#### Make plot with different factors: 
####                                    nutrient cycle
####                                    vegetation dynamics
####                                    microbial processes

### Model list
## CNP model
p.mod.list <- c("CABLP", "ELMV1", 
                "GDAYP", "LPJGP",
                "OCHDP", "OCHDX", 
                "QUINC", "QUJSM")

## CN model
n.mod.list <- c("GDAYN", "LPJGN")

## DGVM model
## we decided to not look at the DGVM output for the current paper
d.mod.list <- c("CABLP", "LPJGP")

### compile all model results together, and save annual and daily datasets
### for observed period under variable climate only.
### run once only
compile_obs_var_dataset_across_models(p.mod.list, n.mod.list, d.mod.list)

compile_obs_fix_dataset_across_models(p.mod.list, n.mod.list, d.mod.list)


##########################################################################
### check forcing data consistency
### go into function to plot
check_forcing_data_consistency(scenario="var")

check_forcing_data_consistency(scenario="fix")


##########################################################################
### make MIP time-series plot for both variable and fixed climate, over observed period
### when number of variables change in the input, need to revise the code
make_MIP_time_series_plot(scenario="var")

make_MIP_time_series_plot(scenario="fix")



##########################################################################
### show P effect with the two models that have CN and CNP versions
### based on fixed climate forcing
scenario="var"
compare_CNP_and_CN_model_output(scenario="var")

scenario="fix"
compare_CNP_and_CN_model_output(scenario="fix")


### show microbial effects with the two microbial enabled models
### based on fixed climate forcing
### add Csoil, Clit, Rh comparison plot

compare_microbial_model_output(scenario="fix")
compare_microbial_model_output(scenario="var")

### show dynamic vegetation effect with the two DGVM models
#compare_dynamic_vegetation_effect_model_output()



### prepare observed dataset at annual timestep wherever possible
eucDF <- prepare_EucFACE_observation_dataset()

### compare to observed dataset
### aCO2 + eCO2
### go into function to plot
### only plot the variable scenario because this is used to compare against data
make_time_averaged_data_model_comparison_over_obs_period(eucDF,
                                                         scenario="var")


### plot photosynthesis response
### need to go into function to plot
scenario="fix"
plot_normalized_GPP_response(scenario="fix")
scenario="var"
plot_normalized_GPP_response(scenario="var")


### Vegetation biomass response
scenario="fix"
plot_normalized_delta_Cveg_response(scenario="fix")
scenario="var"
plot_normalized_delta_Cveg_response(scenario="var")


### plot allocation coefficients:
### fast plant structural pools: leaf and fineroot
### slow plant structural pools: wood and coarseroot
### plant NSC pools: CSTOR, CEX, labile and reseve
### respiration fluxes
### need to go into function to plot
plot_normalized_plant_allocation_response(scenario="fix")




### fate of carbon - go into function to plot
scenario="var"
trace_fate_of_carbon_MIP_plot(scenario="var")

scenario="fix"
trace_fate_of_carbon_MIP_plot(scenario="fix")

#plot_taylor_diagram()

#plot_photosynthesis_relationships(scenario="fix")
#plot_photosynthesis_relationships(scenario="var")


### ambient treatment, CO2 responses
### incomplete
#plot_MIP_CO2_response_comparison(eucDF)



### this is a theoretical analysis on how leaf nutrient concentrations
### affect Vcmax and Jmax parameters. 
### Currently we know:
### Walker et al. 2014: GDAYN, GDAYP, CABLP (also coordination hypothesis)
### Ellsworth unpublished: OCHDY, OCHDX, LPJGP (with Haxeltine & Prentice, 1996)
### Haxeltine and Prentice 1996: LPJGN
### P only downregulate biomass growth: ELMXX, QUINC, QUJSM
### To fill this script, we will need Ellsworth unpublished relationships
#theoretical_analysis_of_leaf_nutrient_effect_on_leaf_physiology()



##########################################################################
### to do list:
### 1. Check individual model to include unaccounted fluxes and pools that are necesary for mass balance closure (e.g. allocation)
### 2. Storyline development
### 2. Generate summary table on key model features:
###    Waiting for input from ELM only,
###    Need to go through each model to confirm their entries.
### 3. Model structure diagrams 
###    all completed except ORCHIDEE-MIC
### 4. Traceability analysis?
### 5. Leaf nutrient effect on Vcmax and Jmax - theoretical analysis
###    Need to proceed even without the Ellsworth relationship
### 6. Data-model intercomparison figures:
###    Ambient + CO2 response (% and abs);
###    Time series plots;
###    Fate of C;
###    N + P variables;
###    Projected future response - can additional C sequestration under reduced P stress
### 7. CN cs. CNP model comparisons
###    GDAYN vs. GDAYP, LPJGN vs. LPJGP
###    Medlyn et al. 2016 model simulations
### 8. Microbial processes analysis
###    Explicit microbial processes: QUJSM, OCHDX
###    Semi-explicit microbial processes: ELMXX
###    Exudation and priming: GDAYP
### 9. DGVM analysis:
###    CABLE-POP + LPJ-GUESS, waiting for ED2 and possibly FATES (noting that BiomeEES also available)
### 10. CO2 x water: conceptual diagram + soil hydrology

##########################################################################
####
#### End.
####
##########################################################################