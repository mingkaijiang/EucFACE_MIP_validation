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
### Step 2: Run individual model mass balance checks
EucFACE_mass_balance_and_validation_script_ORCHIDEE_MIC()
EucFACE_mass_balance_and_validation_script_ORCHIDEE_CNP()
EucFACE_mass_balance_and_validation_script_GDAY()