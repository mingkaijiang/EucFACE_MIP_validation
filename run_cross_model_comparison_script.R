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

## ignore ELM for future period because no simulation has been provided
p.mod.list.rev <- c("CABLP", #"ELMV1",
                    "GDAYP", "LPJGP",
                    "OCHDP", "OCHDX", 
                    "QUINC", "QUJSM")



#### 2.2. Compile all model results together, 
####      and save annual and daily datasets
###       Note that, OCHDX and QUJSM has many more additional variables 
###       that could be useful, 
###       but are currently not included here. 
compile_obs_dataset_across_models(p.mod.list, n.mod.list)


### 2.3. Compile all future predicted datasets together.
###      Generate daily and annual output.
compile_pred_dataset_across_models(p.mod.list=p.mod.list.rev,
                                   n.mod.list=n.mod.list)


### 2.4. Compile both historic and future period together.
###      Use daily data to compute annual data, including delta pools,
###      so that we can calculate normalized responses in the next step.
compile_all_dataset_across_models()


### 2.5. Normalize all the variables to year 2012 or 2019, 
### so that we can compare cross models.
### Note that just do it for the each CO2 treatment.
### Also generated CO2 effect comparison.
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
###      over observed period only.
###      Note: When number of variables change in the input, 
###      need to revise the code.
###      Go into function to plot
scenario="VAR"
make_MIP_time_series_plot(scenario="VAR")

scenario="FIX"
make_MIP_time_series_plot(scenario="FIX")



##########################################################################
#### show P effect with the two models that have CN and CNP versions
#### based on fixed climate forcing
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
### Investigate P fertilization treatment
### 1. real magnitude of P fertilization from different model starting point
### 2. normalized magnitude of P fertilization normalized to the same starting point


### to do: 13-10-2021
### add obs period data (2012-19) and re-compile
### normalize all to 2012 value, or 2019 value




### now we can look at the normalized trajectory as well as the CO2 effect
plot_normalized_pred_trajectories()


### the CO2 experimental design for future period is also interesting to explore
### We have ambient conditions where CO2 rises linearly each year,
### whereas elevated treatment has CO2 stop in year 2030 and then stays the same for the rest 40 years
### Both scenarios land to the same CO2 concentration in year 2069.



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