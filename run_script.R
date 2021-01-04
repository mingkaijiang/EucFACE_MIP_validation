#### This is the master script for processing model simulation results
#### and compare it against some validation datasets for the 
#### EucFACE multi-model intercomparison project.
#### Author: Mingkai Jiang


############################## Basic set-ups ############################## 

#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("prepare.R")

#### select the model abbreviation
#### options are:
####             GDAYN: GDAY, CN version
####             GDAYP: GDAY, CNP version
####             QUNIC: QUINCY
####             OCHDP: ORCHIDEE, CNP version
####             OCHDN: ORCHIDEE, CN version
####             LPJGN: LPJ-Guess, CN version
####             LPJGP: LPJ-Guess, CNP version
####             CABLP: CABLE-POP, CNP version
####             ELMXX: ELM, CNP version

mod.abb <- "GDAYP"

############################## GDAY simulation output ############################## 
### Plot historic validation
plot_historic_validation(mod.abb)
