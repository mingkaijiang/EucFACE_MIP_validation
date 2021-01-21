#### EucFACE mass balance and validation script
#### Mingkai Jiang (m.jiang@westernsydney.edu.au)
####
#### 
#### To use this script, please create two folders within this repository,
#### and place the relevant data files into each folder. 
#### - Create a folder "simulation_output", 
####   and place the model simulation results into this folder 
####   (note, only the file containing ambient treatment over the period of 2012 - 2019).
#### - Create a folder "validation_dataset", 
###    and place the validation datasets into this folder (data can be obtained via the CloudStor link sent previously).
#### 
#### 
#### This output of this script is organized in the following order:
#### - Step 2: Checking mass balance. 
####   This section will check mass balance of model output, 
####   including major carbon, water, nitrogen and phosphorus cycle variables.  
#### - Step 3: Checking time-invariance variables against validation data. 
####   This section will check some key pools and fluxes against the validation dataset. 
####   All validation data are provided in the EucFACE parameter list file. 
####   Note that the data are either averaged over time, or one fixed value measured at one point. 
#### - Step 4: Checking time-varying variables against some validation data. 
####   This section will check some temporal patterns of the simulation results against some validation datasets. 
#### 
#### Finally, please note that, all checks are performed for the ambient CO2 treatment only, 
#### over the period where observations are available (i.e. 2012 - 2019). 
#### For the validation against observation dataset, 
#### please also use simulation results under the no P addition and variable climate forcing scenario. 

##########################################################################
#### Step 1: basic set-up
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

#### setting out path to store the files
out.dir <- paste0(getwd(), "/validation_output")

##########################################################################
#### Step 2. Mass balance checks

### read in simulation results and get it in shape for comparison.
### the naming of the file follows the output protocol. 
### Note that this is the daily file. 
### You can modify this path to read in different files. 
modDF <- read.csv(paste0("simulation_output/EUC_", mod.abb, "_OBS_VAR_AMB_NOP_D.csv"))

### checking number of column in the original dataframe
ncol <- ncol(modDF)
#print(paste0("no. of columns is ", ncol))

# check length of frames, need to be TRUE, otherwise you have too many or little days
# 8 years with 365 days plus 2 years with a leap day! (for those models that do not model leap days, please repeat Feb 28 to fill Feb 29)
if (nrow(modDF)!=(8*365)+2) {
    print(paste("number of rows of model output does not match, all further results unreliable!!"))
}


### add date to the dataset to help with the plotting
for (i in 2012:2019) {
    
    date.list <- as.Date((modDF$DOY[modDF$YEAR==i]-1), 
                         origin = paste0(i, "-01-01"))
    
    modDF$Date[modDF$YEAR == i] <- as.character(date.list)
}

modDF$Date <- as.Date(modDF$Date)

### add other variables not defined in the protocol but are potentially important for this MIP
# P weathering rate
modDF$PWEA <- 0.0


### The mass balance check is performed at annual timestep. 
### Note that, I assume that many models may not output some of these variables, 
### and as such, the mass balance may not close without the inclusion model-specific variables. 
### For those that are relevant, please modify the script with additional/alternative variables 
### to try to close the mass balance. 
### Otherwise, please indicate the reasons as to why your model does not have mass balance closure.

### summarize all fluxes first to obain annual rate
fluxDF <- summaryBy(ET+TRANS+ES+EC+RO+DRAIN+NEP+GPP+NPP+RHET+RAU+RECO+CGL+CGFR+CGCR+CGW+NGL+NGFR+NGCR+NGW+PGL+PGFR+PGCR+PGW+NUP+NGMIN+NMIN+NLEACH+PUP+PGMIN+PMIN+PLEACH+PBIOCHMIN+NLRETR+PLRETR+RCR+RFR+CREPR+CEX+CVOC+RL+RW+RGR+CLITIN+CCRLIN+CFRLIN+CWLIN+NLITIN+NCRLIN+NFRLIN+NWLIN+PLITIN+PCRLIN+PFRLIN+PWLIN+NWRETR+PWRETR+NCRRETR+PCRRETR+NFRRETR+PFRRETR+NDEP+NFIX+NVOL+PDEP+PWEA~YEAR, data=modDF, FUN=sum, keep.names=T, na.rm=T)


### subset first day within a year of all pools
poolDF <- modDF[,c("YEAR", "DOY", "CL","LAI","CW","CFR","CCR","NL","NW","NFR","NCR","PL","PW","PFR","PCR","CSTOR","NSTOR","PSTOR",
                   "CSOIL","NSOIL","PSOIL","NPMIN","PPMIN","PLAB","PSEC","POCC","PPAR","CFLIT","CFLITA","CFLITB",
                   "NFLITA","NFLITB","PFLITA","PFLITB","CCLITB","NCLITB","PCLITB","NFLIT","PFLIT", "NPORG", "PPORG")]

poolDF <- subset(poolDF, DOY==1)

poolDF$DOY <- NULL

### calculate change in pools for mass balance
deltaDF <- poolDF[poolDF$YEAR < 2019,]

l <- dim(deltaDF)[2]

for (i in c(2012:2018)) {
    deltaDF[deltaDF$YEAR==i,2:l] <- poolDF[poolDF$YEAR==(i+1),2:l]-poolDF[poolDF$YEAR==i,2:l]
}

### add delta column name to deltaDF
names(deltaDF)[2:l] <- paste0("delta", names(deltaDF[2:l]))

### merge all dataframe together
annDF <- merge(fluxDF, poolDF, by="YEAR")
annDF <- merge(annDF, deltaDF, by="YEAR", all.x=T)

### calculate annual maximum for some variables
maxDF <- summaryBy(LAI+CL+CFR+CSTOR+NL+NFR+NSTOR+PL+NFR+PSTOR~YEAR, data=modDF, keep.names=T, na.rm=T)




##################### Carbon balance check ################################
### NPP + RAU = GPP
### all models should get this right,
### but note that some models may simply assume a fixed CUE to get RAU (e.g. GDAY)
p1<-xyplot(I(NPP+RAU)~GPP,fluxDF,
           #main='NPP+RAU~GPP',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### NEP + RECO = GPP
p2<-xyplot(I(NEP+RECO)~GPP,fluxDF,
           #main='NEP+RECO~GPP',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### RHET + RAU = RECO
p3<-xyplot(I(RHET+RAU)~RECO,fluxDF,
           #main='RHET+RAUTO~RECO',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 

### sum of all autotrophic respiration fluxes
### Some models may not explicitly simulate individual respiratory fluxes (e.g. GDAY)
### Hence, this mass balance check may not be closed for all models. 
p4<-xyplot(I(RL+RW+RCR+RFR+RGR)~RAU,fluxDF,
           #main='RL+RW+RCR+RFR+RGR~RAU',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### This is not a mass balance check, but rather, to see how LAI scales with leaf carbon pool.
### Given that we provided SLA, we can use
p5<-xyplot(LAI~CL,poolDF,
           #main='LAI~CL',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


p6<-xyplot(I(CGL-CLITIN)~deltaCL,annDF,
           #main='CGL-CLITIN~deltaCL',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


p7<-xyplot(I(CGW-CWLIN)~deltaCW,annDF,
           #main='CGW-CWLIN~deltaCW',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


p8<-xyplot(I(CGFR-CFRLIN)~deltaCFR,annDF,
           #main='CGFR-CFRLIN~deltaCFR',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


p9<-xyplot(I(CGCR-CCRLIN)~deltaCCR,annDF,
           #main='CGCR-CCRLIN~deltaCCR',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### GPP should in theory equals to individual production fluxes and respiratory flux. 
### Here production fluxes include: CGL, CGFR, CGCR, CGW and CREPR, 
### and the respiratory flux is RAU. 
### If there is an exudation flux, make it part of GPP too.
p10<-xyplot(I(RAU+CGL+CGFR+CGCR+CGW+CREPR+CEX)~GPP,annDF,
           #main='I(RAU+CGL+CGFR+CGCR+CGW+CREPR)~GPP',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### Similarly, NPP should equal to all growth fluxes, 
### that is, the sum of CGL, CGFR, CGCR, CGW, CREPR, and CEX.
p11<-xyplot(I(CGW+CGL+CGFR+CGCR+CREPR+CEX)~NPP,annDF,
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### This is a different way to check mass balance for NPP, 
### in that it includes Delta$CSTOR in addition to those growth fluxes included in the previous figure. 
### Some models don't explictly simulate CSTOR, so this mass balance may not apply. 
p12<-xyplot(I(CGW+CGL+CGFR+CGCR+CREPR+CEX+deltaCSTOR)~NPP,annDF,
           #main='I(CGW+CGL+CGFR+CGCR+CREPR+deltaCSTOR)~NPP',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### Here, CFLIT = CFLITA + CFLITB. 
p13<-xyplot(I(CFLITA+CFLITB)~CFLIT,annDF,
           #main='CFLITA+CFLITB~CFLIT',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### This mass balance equation checks the net of total influx litter and heterotrophic respiration. 
### In theory, the net difference of these two flues should equal to the change in soil + litter pool. 
### Note that CEX was included as an additional influx into the soil. The full equation is: 
### CLITIN+CWLIN+CFRLIN+CCRLIN+CREPR+CEX-RHET = Delta$CSOIL+Delta$CCLITB+Delta$CFLIT
p14<-xyplot(I(CEX+CLITIN+CWLIN+CFRLIN+CCRLIN+CREPR-RHET)~I(deltaCSOIL+deltaCCLITB+deltaCFLIT),annDF,
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 
    

### print plots to file, change numbering if needed
pdf(paste0(out.dir, '/QC_Carbon_Balance_',mod.abb,'.pdf',sep=''),width=10,height=8)
for (i in 1:14) {
    print(get(paste("p",i,sep="")))
}
dev.off()


##################### Water balance check ################################

### Firstly, the total evapotranspiration flux (ET) should equal to 
### total transpiration (T) + evaporation fluxes (ES + EC). 
p1<-xyplot(I(ES+EC+TRANS)~ET,fluxDF,
           #main='ES+EC+T~ET',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 



### print plots to file, change numbering if needed
pdf(paste0(out.dir, '/QC_Water_Balance_',mod.abb,'.pdf',sep=''),width=10,height=8)
for (i in 1:1) {
    print(get(paste("p",i,sep="")))
}
dev.off()



##################### Nitrogen balance check ################################
### Firstly, we check Delta$NL, Delta$NW, Delta$NFR and Delta$NCR. 
### Here, Delta$NL = NGL + NLITIN - NLRETR, where NLRETR is the retranslocation flux. 
p1<-xyplot(I(NGL-NLITIN-NLRETR)~deltaNL,annDF,
           #main='I(NGL-NLITIN-NLRETR)~deltaNL',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 

p2<-xyplot(I(NGW-NWLIN-NWRETR)~deltaNW,annDF,
           #main='I(NGW-NWLIN-NWRETR)~deltaNW',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


p3<-xyplot(I(NGFR-NFRLIN-NFRRETR)~deltaNFR,annDF,
           #main='I(NGFR-NFRLIN-NFRRETR)~deltaNFR',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


p4<-xyplot(I(NGCR-NCRLIN-NCRRETR)~deltaNCR,annDF,
           #main='I(NGCR-NCRLIN-NCRRETR)~deltaNCR',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### This is to check finelitter influx. Total NFLIT = NFLITA + NFLITB. 
p5<-xyplot(I(NFLITA+NFLITB)~NFLIT,annDF,
           #main='I(NFLITA+NFLITB)~NFLIT',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### Now, we check mass balance of total N required to support the new growth, 
### which should euqal to the sum of nitrogen uptake and total retranslocation fluxes. 
### The full equation is written as:
### NUP+NLRETR+NWRETR+NFRRETR+NCRRETR = NGL+NGFR+NGCR+NGW
p6<-xyplot(I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW),annDF,
           #main='I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW)',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 

### Some models could have a NSTOR pool, 
### so not all nitrogen available for plant is used for growth. 
### Here we are looking at DeltaNSTOR 
### to see if it helps to close the mass balance if the figure above doesn't close its mass balance. 
p7<-xyplot(I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGFR-NGCR-NGW)~deltaNSTOR,annDF,
           #main='I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGFR-NGCR-NGW)~deltaNSTOR',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### N fixation could also be added to plant directly. 
### So here we are checking its effect. Full equation is: 
### NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR = NGL+NGFR+NGCR+NGW
p8<-xyplot(I(NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW),annDF,
           #main='I(NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR)~I(NGL+NGFR+NGCR+NGW)',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### Similar to the above figure, we are checking if Delta$NSTOR helps to close the budget. 
p9<-xyplot(I(NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGFR-NGCR-NGW)~deltaNSTOR,annDF,
           #main='I(NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGFR-NGCR-NGW)~deltaNSTOR',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### Now we are checking the whole ecosystem N input and output. 
### Full equation is:
### NDEP+NFIX-NLEACH-NVOL = DeltaNL+DeltaNW+DeltaNCR+DeltaNFR+DeltaNSOIL+DeltaNFLIT+DeltaNCLITB

p10<-xyplot(I(NDEP+NFIX-NLEACH-NVOL)~(I(deltaNL+deltaNW+deltaNCR+deltaNFR+deltaNSOIL+deltaNFLIT+deltaNCLITB)),annDF,
           #main='I(NDEP+NFIX-NLEACH-NVOL)~(I(deltaNL+deltaNW+deltaNCR+deltaNFR+deltaNSOIL+deltaNFLIT+deltaNCLITB))',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### A different way to check whole ecosystem N budget, as:
### NDEP+NFIX+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL = DeltaNSOIL+DeltaNFLIT+DeltaNCLITB
p11<-xyplot(I(NDEP+NFIX+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL)~I(deltaNSOIL+deltaNFLIT+deltaNCLITB),annDF,
           #main='I(NDEP+NFIX+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL)~I(deltaNSOIL+deltaNFLIT+deltaNCLITB)',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### Another way to check ecosystem N budget, by excluding NFIX:
### NDEP+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL = DeltaNSOIL+DeltaNFLIT+DeltaNCLITB
p12<-xyplot(I(NDEP+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL)~I(deltaNSOIL+deltaNFLIT+deltaNCLITB),annDF,
           #main='I(NDEP+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL)~I(deltaNSOIL+deltaNFLIT+deltaNCLITB)',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### We now check the mass balance for NSOIL, which should equal to total organic and inorganic pools. 
p13<-xyplot(I(NPMIN+NPORG)~NSOIL,annDF,
           #main='I(NPMIN+NPORG)~NSOIL',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 



### print plots to file, change numbering if needed
pdf(paste0(out.dir, '/QC_Nitrogen_Balance_',mod.abb,'.pdf',sep=''),width=10,height=8)
for (i in 1:13) {
    print(get(paste("p",i,sep="")))
}
dev.off()



##################### Phosphorus balance check ################################
### The net influx - outflux should equal to the change in all ecosystem P pools: 
### PDEP+PWEA-PLEACH = DeltaPL+DeltaPW+DeltaPCR+DeltaPFR+DeltaPSOIL+DeltaPFLIT+DeltaPCLITB
p1<-xyplot(I(PDEP+PWEA-PLEACH)~(I(deltaPL+deltaPW+deltaPCR+deltaPFR+deltaPSOIL+deltaPFLIT+deltaPCLITB)),annDF,
           #main='I(PDEP+PWEA-PLEACH)~(I(deltaPL+deltaPW+deltaPCR+deltaPFR+deltaPSOIL+deltaPFLIT+deltaPCLITB))',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### Next, we check changes in major vegetation P pools. 
### It should equal to production flux - retranslocation flux - litterfall. 
p2<-xyplot(I(PGL-PLITIN-PLRETR)~deltaPL,annDF,
           #main='I(PGL-PLITIN-PLRETR)~deltaPL',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


p3<-xyplot(I(PGW-PWLIN-PWRETR)~deltaPW,annDF,
           #main='I(PGW-PWLIN-PWRETR)~deltaPW',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


p4<-xyplot(I(PGFR-PFRLIN-PFRRETR)~deltaPFR,annDF,
           #main='I(PGFR-PFRLIN-PFRRETR)~deltaPFR',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


p6<-xyplot(I(PGCR-PCRLIN-PCRRETR)~deltaPCR,annDF,
           #main='I(PGCR-PCRLIN-PCRRETR)~deltaPCR',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### This is to check P litter flux. 
p7<-xyplot(I(PFLITA+PFLITB)~PFLIT,annDF,
           #main='I(PFLITA+PFLITB)~PFLIT',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 

### Now we check the total P required to make new vegetation: 
### PUP+PLRETR+PWRETR+PFRRETR+PCRRETR=PGL+PGFR+PGCR+PGW
p8<-xyplot(I(PUP+PLRETR+PWRETR+PFRRETR+PCRRETR)~I(PGL+PGFR+PGCR+PGW),annDF,
           #main='I(PUP+PLRETR+PWRETR+PFRRETR+PCRRETR)~I(PGL+PGFR+PGCR+PGW)',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### This is to consider the effect of DeltaPSTOR. 
p9<-xyplot(I(PUP+PLRETR+PWRETR+PFRRETR+PCRRETR-PGL-PGFR-PGCR-PGW)~deltaPSTOR,annDF,
           #main='I(PUP+PLRETR+PWRETR+PFRRETR+PCRRETR-PGL-PGFR-PGCR-PGW)~deltaPSTOR',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### This is to check whole ecosystem P flux, 
### which means that total in - out = net change:
### PDEP+PWEA+PLITIN+PWLIN+PCRLIN+PFRLIN-PUP-PLEACH = DeltaPSOIL+DeltaFLIT+DeltaPCLITB
p10<-xyplot(I(PDEP+PWEA+PLITIN+PWLIN+PCRLIN+PFRLIN-PUP-PLEACH)~I(deltaPSOIL+deltaPFLIT+deltaPCLITB),annDF,
           #main='I(PDEP+PWEA+PLITIN+PWLIN+PCRLIN+PFRLIN-PUP-PLEACH)~I(deltaPSOIL+deltaPFLIT+deltaPCLITB)',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### Now to check a basic mass balance on soil P. 
p11<-xyplot(I(PPMIN+PPORG)~PSOIL,annDF,
           #main='I(PPMIN+PPORG)~PSOIL',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### Inorganic P pool in soils: PLAB+PSEC+POCC+PPAR=PPMIN
p12<-xyplot(I(PLAB+PSEC+POCC+PPAR)~PPMIN,annDF,
           #main='I(PLAB+PSEC+POCC+PPAR)~PPMIN',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### A different way to check PSOIL. 
p13<-xyplot(I(PLAB+PSEC+POCC+PPAR+PPORG)~PSOIL,annDF,
           #main='I(PLAB+PSEC+POCC+PPAR+PPORG)~PSOIL',
           auto.key=T,
           scales=list(relation='free'),
           panel=function(...){
               panel.xyplot(...)
               panel.abline(a=0,b=1)}) 


### print plots to file, change numbering if needed
pdf(paste0(out.dir, '/QC_Phosphorus_Balance_',mod.abb,'.pdf',sep=''),width=10,height=8)
for (i in 1:13) {
    print(get(paste("p",i,sep="")))
}
dev.off()





##########################################################################
#### Step 3. Time-averaged validation

################# Major carbon pools  ####################
### Firstly we will check the major carbon pools, 
### as these data are provided in Table 1 in the parameter file. 
### Note that:
### * CFR combines fineroot (< 2 mm in diameter) and intermediate root (2-3 mm) in the observation;
### * CSOIL is for top 10 cm of soil in the observation;
### * CL includes overstorey leaf only in the observation;
### * CW includes branch and stem in the model simulation.

### create a DF to store observation data for vegetation carbon stocks
vegDF <- data.frame(c("CL", "CW", "CFR", "CCR", "CSOIL"), NA, NA)
colnames(vegDF) <- c("Variable", 
                     "observed",
                     "simulated")

vegDF$observed[vegDF$Variable=="CL"] <- "151 ± 14"
vegDF$observed[vegDF$Variable=="CW"] <- "4558 ± 321"
vegDF$observed[vegDF$Variable=="CFR"] <- "227 ± 5"
vegDF$observed[vegDF$Variable=="CCR"] <- "606 ± 60"
vegDF$observed[vegDF$Variable=="CSOIL"] <- "2183 ± 280"


### calcualte annual means in the simulated data
poolDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012)

poolDF <- summaryBy(CL+CW+CFR+CCR+CSOIL~YEAR, data=poolDF, FUN=mean, na.rm=T, keep.names=T)

### assign values
vegDF$simulated[vegDF$Variable=="CL"] <- round(poolDF$CL[poolDF$YEAR=="2016"], 0)
vegDF$simulated[vegDF$Variable=="CW"] <- round(poolDF$CW[poolDF$YEAR=="2016"], 0)
vegDF$simulated[vegDF$Variable=="CFR"] <- round(poolDF$CFR[poolDF$YEAR=="2016"], 0)
vegDF$simulated[vegDF$Variable=="CCR"] <- round(poolDF$CCR[poolDF$YEAR=="2016"], 0)
vegDF$simulated[vegDF$Variable=="CSOIL"] <- round(poolDF$CSOIL[poolDF$YEAR=="2016"], 0)

### Plotting
p1 <- ggplot(vegDF,)




##########################################################################
#### Step 4. Time-varying validation


##########################################################################

### End.