#### Script for the data-model intercomparison part of the analysis
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



##########################################################################
#### 2.2. Compile all model results together, 
####      and save annual and daily datasets
###       Note that, OCHDX and QUJSM has many more additional variables 
###       that could be useful, 
###       but are currently not included here. 
#compile_obs_dataset_across_models(p.mod.list, n.mod.list)

###   prepare microbial model input
#prepare_microbial_model_input()


### 2.3. Compile all future predicted datasets together.
###      Generate daily and annual output.
#compile_pred_dataset_across_models(p.mod.list=p.mod.list,
#                                   n.mod.list=n.mod.list)


### 2.4. Compile both historic and future period together.
###      Use daily data to compute annual data, including delta pools,
###      so that we can calculate normalized responses in the next step.
#compile_all_dataset_across_models()


### 2.5. Normalize all the variables to year 2012 or 2019, 
###      so that we can compare cross models.
###      Note that just do it for the each CO2 treatment.
###      Also generated CO2 effect comparison.
#normalize_dataset_across_models(yr.to.normalize=2012)

#normalize_dataset_across_models(yr.to.normalize=2019)


##########################################################################
#### Step 3. Cross-model comparison 

### 3.1. Check forcing data consistency,
###      Only focusing on historic period
###      Go into function to plot.
#scenario="VAR"
#check_forcing_data_consistency(scenario="VAR")

#scenario="FIX"
#check_forcing_data_consistency(scenario="FIX")


##########################################################################
####      Prepare observed dataset at annual timestep wherever possible
####      Including C budget but also P and N budget and the
####      associated stoichiometry, water budget, etc.
####      Use model variable names
####      The confidence interval in the data indicates treatment variation,
####      and less so about inter-annual variation.
####      The model confidence interval is more about inter-annual variation.
ignore.understorey=F
eucDF <- prepare_EucFACE_observation_dataset(ignore.understorey=F)


### check data-model agreement
scenario="VAR"
rev.sd=1.0

check_data_model_agreement(scenario="VAR", eucDF, rev.sd=rev.sd)



##########################################################################
###      Show P effect with the two models that have CN and CNP versions
###      based on fixed climate forcing
###      Go into function to plot
scenario="VAR"
compare_CNP_and_CN_model_output(scenario="VAR")

scenario="FIX"
compare_CNP_and_CN_model_output(scenario="FIX")



##########################################################################
###      Compare against Medlyn 2016
###      The Medlyn 2016 is simulated based on fixed climate (hypothetical, wet)
###      and so we will need to compare it against the FIX climate scenario in this MIP.
prepare_Medlyn_2016_input()

compare_two_MIP_results()



##########################################################################
### plot growth responses and their comparison to data
### including delta C veg, NEP, GPP, Cveg and allocation coefficients
scenario="VAR"
plot_growth_and_nep_response(scenario="VAR", eucDF)



##########################################################################
### This is a theoretical analysis on how leaf nutrient concentrations
### affect Vcmax and Jmax parameters. 
### Currently we know:
### Walker et al. 2014: GDAYN, GDAYP, CABLP (also coordination hypothesis)
### Ellsworth unpublished: OCHDY, OCHDX, LPJGP (with Haxeltine & Prentice, 1996)
### Haxeltine and Prentice 1996: LPJGN
### P only downregulate biomass growth: ELMXX, QUINC, QUJSM
scenario="VAR"
theoretical_analysis_of_leaf_nutrient_effect_on_leaf_physiology(scenario="VAR")

###      Photosynthesis and relationship with leaf nutrients
scenario="VAR"
#plot_photosynthesis_relationships(scenario="FIX")
plot_photosynthesis_relationships(scenario="VAR", eucDF=eucDF)



###      Plot photosynthetic response.
###      Need to go into function to plot
#scenario="FIX"
#plot_normalized_GPP_response(scenario="FIX")
scenario="VAR"
plot_normalized_GPP_response(scenario="VAR", eucDF=eucDF)




##########################################################################
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
###      need to go into function to plot
scenario="VAR"
trace_fate_of_carbon_MIP_plot(scenario="VAR")

#scenario="FIX"
#trace_fate_of_carbon_MIP_plot(scenario="FIX")


##########################################################################
####     Data-model intercomparison

###      Compare to observed dataset
###      aCO2 + eCO2
###      Only plot the variable scenario,
###      because this is used to compare against data
###      Go into function to plot.
scenario="VAR"
plot_plant_p_cycle_responses(eucDF,
                             scenario="VAR")

#plot_plant_p_cycle_responses2(eucDF,
#                              scenario="VAR")

plot_soil_p_cycle_responses(eucDF,
                            scenario="VAR")



##########################################################################

###       show microbial effects with the two microbial enabled models
###      based on fixed climate forcing
###      add Csoil, Clit, Rh comparison plot


###   compare general variables
#scenario="FIX"
#compare_microbial_model_general_output(scenario="FIX")

#scenario="VAR"
#compare_microbial_model_general_output(scenario="VAR")


###  Investigate the MIMICS-like ORCHX performance
#investigate_OCHDX_microbial_responses(scenario="FIX")
#investigate_OCHDX_microbial_responses(scenario="VAR")


###   Combine the two
#investigate_microbial_responses(scenario="FIX", compare.to.obs=F)
scenario="VAR"
investigate_microbial_responses(scenario="VAR", eucDF)


###   Investigate QUJSM
#investigate_QUJSM_microbial_responses(scenario="FIX")
investigate_QUJSM_microbial_responses(scenario="VAR")


##########################################################################
###      Investigate gross mineralization rate and its relationship to
###     plant nutrient uptake.
###     Expectation: microbial models should have a large component of
###     the gross mineralization flux to be immobilized by microbes,
###     and hence the ratio of plant nutrient uptake to gross mineralization
###     is tiny.
#investigate_gross_mineralization(scenario="VAR")
#investigate_gross_mineralization(scenario="FIX")




##########################################################################
###      Make MIP time-series plot for both variable and fixed climate, 
###      over observed period only!!!
###      Note: When number of variables change in the input, 
###      need to revise the code.
make_MIP_time_series_plot(scenario="VAR")

make_MIP_time_series_plot(scenario="FIX")


### ambient treatment, CO2 responses
### incomplete
#plot_MIP_CO2_response_comparison(eucDF)


### time-varying response
###      LAI, Rsoil
make_time_varying_data_model_comparison_over_obs_period(scenario="VAR")



### Taylor diagram for LAI, Rsoil and GPP
plot_taylor_diagram(scenario="VAR")










##########################################################################
####
#### End.
####
##########################################################################