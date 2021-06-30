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


### prepare observed dataset at annual timestep wherever possible
eucDF <- prepare_EucFACE_observation_dataset()


### compare to observed dataset
### color palette:
obs.color <- c("#000000") # black

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

make_time_averaged_data_model_comparison_over_obs_period(eucDF,
                                                         p.mod.list, 
                                                         n.mod.list, 
                                                         d.mod.list)










##########################################################################
### to do list:
### 1. Go through mass balance checks with David: LPJ-GUESS CN, LPJ-GUESS CNP
### 2. New set of simulation to close mass balance checks: QUINCY, QUINCY-JSM
### 3. Generate summary table on key model features
### 4. Model structure diagrams and traceability analysis
### 6. Include dynamic vegetation analysis: CABLE-POP and LPJ-GUESS
### 7. Add ELM
### 8. MIP figures on CO2 x P
### 9. MIP figures on CO2 x drought


##########################################################################
####
#### End.
####
##########################################################################