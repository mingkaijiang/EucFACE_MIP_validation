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
p.mod.list <- c("CABLP", "ELMXX", 
                "GDAYP", "LPJGP",
                "OCHDP", "OCHDX", 
                "QUINC", "QUJSM")

## CN model
n.mod.list <- c("GDAYN", "LPJGN")

## DGVM model
d.mod.list <- c("CABLP", "LPJGP")

### compile all model results together, and save annual and daily datasets
### for observed period under variable climate only.
### run once only
compile_obs_var_dataset_across_models(p.mod.list, n.mod.list, d.mod.list)

compile_obs_fix_dataset_across_models(p.mod.list, n.mod.list, d.mod.list)


### show P effect with the two models that have CN and CNP versions
compare_CNP_and_CN_model_output()


### show microbial effects with the two microbial enabled models
#compare_microbial_processes_model_output()

### show dynamic vegetation effect with the two DGVM models
#compare_dynamic_vegetation_effect_model_output()


### make MIP time-series plot for both variable and fixed climate, over observed period
make_MIP_time_series_plot(scenario="var")

make_MIP_time_series_plot(scenario="fix")


### prepare observed dataset at annual timestep wherever possible
eucDF <- prepare_EucFACE_observation_dataset()


### compare to observed dataset
### ambient treatment, real values
make_time_averaged_data_model_comparison_over_obs_period(eucDF)


### plot photosynthesis vs leaf N & P relationships
plot_normalized_GPP_response(scenario="fix")
plot_normalized_GPP_response(scenario="var")

#plot_taylor_diagram()

#plot_photosynthesis_relationships(scenario="fix")
#plot_photosynthesis_relationships(scenario="var")


### fate of carbon
trace_fate_of_carbon_MIP_plot()


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
### 1. Simulation completion:
###    Waiting for
###                QUINC + QUJSM for new set of simulations
###                ELM for first set of simulations
### 2. Generate summary table on key model features:
###    Waiting for input from ELM only,
###    Need to go through each model to confirm their entries.
### 3. Model structure diagrams 
###    Received: GDAYP, GDAYN, CABLP, LPJGN, LPJGP, QUINC, QUJSM, OCHDX (cannot to use for pub)
###    Received pointers: ELMXX (published paper but rough), OCHDP (nothing available)
### 4. Traceability analysis?
### 5. Leaf nutrient effect on Vcmax and Jmax - theoretical analysis
### 6. Data-model intercomparison figures:
###    Ambient + CO2 response (% and abs);
###    Time series plots;
###    Fate of C;
###    N + P variables;
###    Projected future responses.
### 7. CN cs. CNP model comparisons
###    GDAYN vs. GDAYP, LPJGN vs. LPJGP
###    Medlyn et al. 2016 model simulations
### 8. Microbial processes analysis
###    Explicit microbial processes: QUJSM, OCHDX
###    Semi-explicit microbial processes: ELMXX
###    Exudation and priming: GDAYP
### 9. DGVM analysis:
###    CABLE-POP + LPJ-GUESS
### 10. CO2 x water: conceptual diagram + soil hydrology

##########################################################################
####
#### End.
####
##########################################################################