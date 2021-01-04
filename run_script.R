#### This is the master script for processing model simulation results
#### and compare it against some validation datasets for the 
#### EucFACE multi-model intercomparison project.
#### Author: Mingkai Jiang


############################## Basic set-ups ############################## 

#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")


############################## GDAY simulation output ############################## 
### Plot historic validation
plot_historic_validation()
