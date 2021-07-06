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
p.mod.list <- c("CABLP", #"ELMXX", 
                "GDAYP", "LPJGP",
                "OCHDP", "OCHDX", 
                "QUINC", "QUJSM")

## CN model
n.mod.list <- c("GDAYN", "LPJGN")

## DGVM model
d.mod.list <- c("CABLP", "LPJGP")

### compile all model results together, and save annual and daily datasets
### for observed period under variable climate only.
compile_obs_var_dataset_across_models(p.mod.list, n.mod.list, d.mod.list)

compile_obs_fix_dataset_across_models(p.mod.list, n.mod.list, d.mod.list)


### show P effect with the two models that have CN and CNP versions
compare_CNP_and_CN_model_output()


### make MIP time-series plot for both variable and fixed climate, over observed period
make_MIP_time_series_plot(scenario="var")

make_MIP_time_series_plot(scenario="fix")


### prepare observed dataset at annual timestep wherever possible
eucDF <- prepare_EucFACE_observation_dataset()


### compare to observed dataset
### ambient treatment, real values
make_time_averaged_data_model_comparison_over_obs_period(eucDF,
                                                         p.mod.list, 
                                                         n.mod.list, 
                                                         d.mod.list)

### ambient treatment, CO2 responses
plot_MIP_CO2_response_comparison(eucDF,
                                 p.mod.list, 
                                 n.mod.list, 
                                 d.mod.list)



### plot photosynthesis vs leaf N & P relationships
plot_photosynthesis_relationships(scenario="fix")

plot_photosynthesis_relationships(scenario="var")


### this is a theoretical analysis on how leaf nutrient concentrations
### affect Vcmax and Jmax parameters. 
### Currently we know:
### Walker et al. 2014: GDAYN, GDAYP, CABLP (also coordination hypothesis)
### Ellsworth unpublished: OCHDY, OCHDX, LPJGP (with Haxeltine & Prentice, 1996)
### Haxeltine and Prentice 1996: LPJGN
### P only downregulate biomass growth: ELMXX, QUINC, QIJSM
### To fill this script, we will need Ellsworth unpublished relationships
theoretical_analysis_of_leaf_nutrient_effect_on_leaf_physiology()



##########################################################################
### to do list:
### 1. Go through mass balance checks with David: LPJ-GUESS CN, LPJ-GUESS CNP
### 2. New set of simulation to close mass balance checks: QUINCY, QUINCY-JSM
### 3. Generate summary table on key model features
### 4. Model structure diagrams and traceability analysis
### 6. Include dynamic vegetation analysis: CABLE-POP and LPJ-GUESS
### 7. Add ELM
### 8. MIP figures on CO2 x P
###    8.1. Data-model intercomparison, ambient conditions
###    8.2. CO2 response ratios
###    8.3. Beyond C budget
###    8.4. Photosynthesis and leaf N, P relationships, and the CO2 response
###    8.5. Microbial related analyses
### 9. MIP figures on CO2 x drought


##########################################################################
####
#### End.
####
##########################################################################