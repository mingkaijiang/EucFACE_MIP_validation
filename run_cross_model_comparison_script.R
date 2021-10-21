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
#### Step 2: prepare the simulation results

#### 2.1. Model list
## CNP model
p.mod.list <- c("CABLP", "ELMV1", 
                "GDAYP", "LPJGP",
                "OCHDP", "OCHDX", 
                "QUINC", "QUJSM")

## CN model
n.mod.list <- c("GDAYN", "LPJGN")

## DGVM model
## we decided to not look at the DGVM output for the current paper
#d.mod.list <- c("CABLP", "LPJGP")


#### 2.2. Compile all model results together, 
####      and save annual and daily datasets
###       Note that, OCHDX and QUJSM has many more additional variables 
###       that could be useful, 
###       but are currently not included here. 
compile_obs_dataset_across_models(p.mod.list, n.mod.list)


### 2.3. Compile all future predicted datasets together.
###      Generate daily and annual output.
compile_pred_dataset_across_models(p.mod.list=p.mod.list,
                                   n.mod.list=n.mod.list)


### 2.4. Compile both historic and future period together.
###      Use daily data to compute annual data, including delta pools,
###      so that we can calculate normalized responses in the next step.
compile_all_dataset_across_models()


### 2.5. Normalize all the variables to year 2012 or 2019, 
###      so that we can compare cross models.
###      Note that just do it for the each CO2 treatment.
###      Also generated CO2 effect comparison.
normalize_dataset_across_models(yr.to.normalize=2012)



#### 2.6. Prepare observed dataset at annual timestep wherever possible
####      Including C budget but also P and N budget and the
####      associated stoichiometry, water budget, etc.
####      Use model variable names
####      The confidence interval in the data indicates treatment variation,
####      and less so about inter-annual variation.
####      The model confidence interval is more about inter-annual variation.
eucDF <- prepare_EucFACE_observation_dataset()



##########################################################################
#### Step 3. Cross-model comparison 

### 3.1. Check forcing data consistency,
###      Only focusing on historic period
###      Go into function to plot.
scenario="VAR"
check_forcing_data_consistency(scenario="VAR")

scenario="FIX"
check_forcing_data_consistency(scenario="FIX")



### 3.2. Make MIP time-series plot for both variable and fixed climate, 
###      over observed period only!!!
###      Note: When number of variables change in the input, 
###      need to revise the code.
make_MIP_time_series_plot(scenario="VAR")

make_MIP_time_series_plot(scenario="FIX")


### 3.3. Plot photosynthetic response.
###      Need to go into function to plot
scenario="FIX"
plot_normalized_GPP_response(scenario="FIX")
scenario="VAR"
plot_normalized_GPP_response(scenario="VAR")


### 3.4. Plot allocation coefficients:
###      fast plant structural pools: leaf and fineroot
###      slow plant structural pools: wood and coarseroot
###      plant NSC pools: CSTOR, CEX, labile and reseve
###      respiration fluxes
###      need to go into function to plot
###      unfinished
#plot_normalized_plant_allocation_response(scenario="FIX")


### 3.5. Fate of carbon 
###      To make it comparable to the obs, we will subset 2013 - 2016 
trace_fate_of_carbon_MIP_plot(scenario="VAR")

trace_fate_of_carbon_MIP_plot(scenario="FIX")


### 3.6. Plot normalized trajectory as well as the CO2 effect over time
###      This function checks the P fertilization effect for the future period
###      but does not look at the drought effect:
###      1. real magnitude of P fertilization from different model starting point
###      2. normalized magnitude of P fertilization normalized to the same starting point
plot_normalized_pred_trajectories(climate.scenario="VAR",
                                  yr.to.normalize=2012)

plot_normalized_pred_trajectories(climate.scenario="FIX",
                                  yr.to.normalize=2012)



### 3.7. Check CO2 x drought interaction
###      1. Firstly check historic period with NOP
###         to see whether we have soil water savings under eCO2
###         and how that translates into drought response and post-drought
###         recovery.
###      2. Check how continued increase in CO2 affects the drought year response,
###         and the long-term cumulative C storage in plants and soils.
###      3. Check if additional P alleviates plant drought stress under eCO2.
###      4. Check water use efficiency
plot_CO2_water_interaction()


### 3.8 check gradual increase in CO2 vs. sharp increase in CO2
### compare the eCO2 response with aCO2, i.e.:
### aCO2, eCO2 with sharp increase, eCO2 with gradual increase
### Subset a period of future only and historic only under NOP
### ambDF: 2013 - 2018, 2064 - 2069
### eleDF: 2013 - 2018
plot_gradual_increase_CO2_trajectories(climate.scenario="FIX")

plot_gradual_increase_CO2_trajectories(climate.scenario="VAR")



### 3.9 Investigate gross mineralization rate and its relationship to
###     plant nutrient uptake.
###     Expectation: microbial models should have a large component of
###     the gross mineralization flux to be immobilized by microbes,
###     and hence the ratio of plant nutrient uptake to gross mineralization
###     is tiny.
investigate_gross_mineralization(scenario="VAR")
investigate_gross_mineralization(scenario="FIX")



##########################################################################
#### Step 4: Model specific investigations

### 4.1. show P effect with the two models that have CN and CNP versions
###      based on fixed climate forcing
###      Go into function to plot
scenario="VAR"
compare_CNP_and_CN_model_output(scenario="VAR")

scenario="FIX"
compare_CNP_and_CN_model_output(scenario="FIX")


### 4.2. show microbial effects with the two microbial enabled models
###      based on fixed climate forcing
###      add Csoil, Clit, Rh comparison plot

### 4.2.1. prepare microbial model input
prepare_microbial_model_input()


### 4.2.2. compare general variables
scenario="FIX"
compare_microbial_model_general_output(scenario="FIX")

scenario="VAR"
compare_microbial_model_general_output(scenario="VAR")


### 4.2.3. Investigate the MIMICS-like ORCHX performance
investigate_OCHDX_microbial_responses(scenario="FIX")

investigate_OCHDX_microbial_responses(scenario="VAR")



##########################################################################
#### Step 5. Data-model intercomparison

### 5.1. Compare to observed dataset
###      aCO2 + eCO2
###      Only plot the variable scenario,
###      because this is used to compare against data
###      Go into function to plot.

scenario="VAR"
make_time_averaged_data_model_comparison_over_obs_period(eucDF,
                                                         scenario="VAR")


### ambient treatment, CO2 responses
### incomplete
#plot_MIP_CO2_response_comparison(eucDF)


### 5.2. time-varying response
###      LAI, Rsoil
make_time_varying_data_model_comparison_over_obs_period(scenario="VAR")



### 5.3. Taylor diagram for LAI, Rsoil and GPP
plot_taylor_diagram(scenario="VAR")




##########################################################################
#### Step 6. Theoretical analyses

### 6.1. Compare against Medlyn 2016
###      The Medlyn 2016 is simulated based on fixed climate (hypothetical, wet)
###      and so we will need to compare it against the FIX climate scenario in this MIP.
prepare_Medlyn_2016_input()

compare_two_MIP_results()



### 6.2. Photosynthesis and relationship with leaf nutrients
#plot_photosynthesis_relationships(scenario="FIX")
#plot_photosynthesis_relationships(scenario="VAR")



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
#### Step 7. Other work




##########################################################################
### to do list:
### 1. Check individual model to include unaccounted fluxes and pools 
###    that are necesary for mass balance closure (e.g. allocation)
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