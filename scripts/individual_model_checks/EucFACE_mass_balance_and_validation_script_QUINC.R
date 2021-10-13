EucFACE_mass_balance_and_validation_script_QUINC <- function() {
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
    #rm(list=ls(all=TRUE))
    #
    ##### Source functions and packages
    #source("prepare.R")
    
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
    mod.abb <- "QUINC"
    
    #### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/QC/", mod.abb)

    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ##########################################################################
    #### Step 2. Mass balance checks
    
    ### read in simulation results and get it in shape for comparison.
    ### the naming of the file follows the output protocol. 
    ### Note that this is the daily file. 
    ### You can modify this path to read in different files. 
    modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                             "/EUC_", mod.abb, "_OBS_VAR_AMB_NOP_D.csv"))
    
    ### checking number of column in the original dataframe
    ncol <- ncol(modDF)
    #print(paste0("no. of columns is ", ncol))
    
    # check length of frames, need to be TRUE, otherwise you have too many or little days
    # 8 years with 365 days plus 2 years with a leap day! (for those models that do not model leap days, please repeat Feb 28 to fill Feb 29)
    if (nrow(modDF)==(8*365)) {
        print(paste("number of rows of model output indicate no leap years, proceed further analysis..."))
        
    } else if (nrow(modDF)==(8*365)+2) {
        print(paste("number of rows of model output indicate two leap years, proceed further analysis..."))
    } else {
        print(paste("number of rows of model output does not match correct number of days, all further results unreliable!!"))
        
    }
    
    
    ### add date to the dataset to help with the plotting
    for (i in 2012:2019) {
        
        date.list <- as.Date((modDF$DOY[modDF$YEAR==i]-1), 
                             origin = paste0(i, "-01-01"))
        
        modDF$Date[modDF$YEAR == i] <- as.character(date.list)
    }
    
    modDF$Date <- as.Date(modDF$Date)
    
    ### add other variables not defined in the protocol but are potentially important for this MIP

    
    ### The mass balance check is performed at annual timestep. 
    ### Note that, I assume that many models may not output some of these variables, 
    ### and as such, the mass balance may not close without the inclusion model-specific variables. 
    ### For those that are relevant, please modify the script with additional/alternative variables 
    ### to try to close the mass balance. 
    ### Otherwise, please indicate the reasons as to why your model does not have mass balance closure.
    
    ### additional variables are:
    ### RMAIN: maintenance respiration
    ### RNTRANS: respiration for N transformation
    ### CGSTR: growth for storage compartments (labile and reserve pool)
    ### NGSTR: growth N to plant labile and reserve pool
    ### NREPR: Growth N to plant reproducable pool
    ### NRECYC: N recycling
    ### NSEED: seed pool N
    ### NFRUIT: fruit pool N
    ### PGSTR: growth P to plant labile and reserve pool
    ### PREPR: Growth P to plant reproducable pool
    ### PRECYC: P recycling
    ### PSEED: seed pool P
    ### PFRUIT: fruit pool P
    ### CTSOIL: total SOC (for 9.5 m)
    ### NTPMIN: Total mineral N
    ### NTPORG: Total SON
    ### NTSOIL: Total Soil N
    ### PTSOIL: Total Soil P
    ### PTPMIN: Total inorganic soil P
    ### PTPORG: Total organic soil P
    ### SWT: Total soil water
    
    ### summarize all fluxes first to obain annual rate
    fluxDF <- summaryBy(PREC+ET+TRANS+ES+EC+RO+DRAIN+NEP+GPP+NPP+RHET+RAU+
                            RECO+CGL+CGFR+CGCR+CGW+NGL+NGFR+NGCR+NGW+PGL+
                            PGFR+PGCR+PGW+NUP+NGMIN+NMIN+NLEACH+PUP+PGMIN+
                            PMIN+PLEACH+PBIOCHMIN+NLRETR+PLRETR+RCR+RFR+CREPR+
                            CEX+CVOC+RL+RW+RGR+CLITIN+CCRLIN+CFRLIN+CWLIN+
                            NLITIN+NCRLIN+NFRLIN+NWLIN+PLITIN+PCRLIN+PFRLIN+
                            PWLIN+NWRETR+PWRETR+NCRRETR+PCRRETR+NFRRETR+
                            PFRRETR+NDEP+NFIX+NVOL+PDEP+PWEA+
                            ### additional variables for QUINCY
                            RMAIN+RNTRANS+CVEGLIN+CGSTR+ ## labile C flux into plant labile pool
                            NGSTR+NSTRLIN+NVEGLIN+NREPR+NRECYC+
                            PGSTR+PSTRLIN+PVEGLIN+PREPR+PRECYC~
                            YEAR, data=modDF, FUN=sum, keep.names=T, na.rm=T)
    
    
    ### subset first day within a year of all pools
    poolDF <- modDF[,c("YEAR", "DOY", "SW","CL","LAI","CW","CFR","CCR","NL",
                       "NW","NFR","NCR","PL","PW","PFR","PCR","CSTOR","NSTOR","PSTOR",
                       "CSOIL","NSOIL","PSOIL","NPMIN","PPMIN","PLAB","PSEC",
                       "POCC","PPAR","CFLIT","CFLITA","CFLITB",
                       "NFLITA","NFLITB","PFLITA","PFLITB","CCLITB","NCLITB",
                       "PCLITB","NFLIT","PFLIT", "NPORG", "PPORG", 
                       ### additional variables for QUINCY
                       "NSEED","NFRUIT","PSEED","PFRUIT",
                       "CTSOIL", "NTPMIN","NTPORG", "NTSOIL",
                       "PTSOIL","PTPMIN","PTPORG","SWT")]
    
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
    maxDF <- summaryBy(LAI+CL+CFR+CSTOR+NL+NFR+NSTOR+PL+PFR+PSTOR~YEAR, data=modDF, keep.names=T, na.rm=T)
    
    
    
    
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
    p4<-# xyplot(I(RL+RW+RCR+RFR+RGR+RMAIN+RNTRANS)~RAU,fluxDF,
        xyplot(I(RGR+RMAIN+RNTRANS)~RAU,fluxDF,     
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
    p10<-#xyplot(I(RAU+CGL+CGFR+CGCR+CGW+CREPR+CEX+deltaCSTOR)~GPP,annDF,
                xyplot(I(RAU+CGL+CGFR+CGCR+CGW+CGSTR+CREPR+CEX)~GPP,annDF,
                #main='I(RAU+CGL+CGFR+CGCR+CGW+CREPR)~GPP',
                auto.key=T,
                scales=list(relation='free'),
                panel=function(...){
                    panel.xyplot(...)
                    panel.abline(a=0,b=1)}) 
    
    
    ### Similarly, NPP should equal to all growth fluxes, 
    ### that is, the sum of CGL, CGFR, CGCR, CGW, CREPR, and CEX.
    p11<-#xyplot(I(CGW+CGL+CGFR+CGCR+CREPR+CEX)~NPP,annDF,
                xyplot(I(CGW+CGL+CGFR+CGCR+CREPR+CGSTR+CEX)~NPP,annDF,
                auto.key=T,
                scales=list(relation='free'),
                panel=function(...){
                    panel.xyplot(...)
                    panel.abline(a=0,b=1)}) 
    
    
    ### This is a different way to check mass balance for NPP, 
    ### in that it includes Delta$CSTOR in addition to those growth fluxes included in the previous figure. 
    ### Some models don't explictly simulate CSTOR, so this mass balance may not apply. 
    ### Lin COMMENT: I(deltaCVEG+CVEGLIN)~NPP
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
    
    
    
    
    ### Lin to check
    p14<-# xyplot(I(CEX+CLITIN+CWLIN+CFRLIN+CCRLIN+CREPR-RHET)~I(deltaCTSOIL+deltaCCLITB+deltaCFLIT),annDF,
                xyplot(I(CEX+CVEGLIN-RHET)~I(deltaCTSOIL+deltaCCLITB+deltaCFLIT),annDF,
                auto.key=T,
                scales=list(relation='free'),
                panel=function(...){
                    panel.xyplot(...)
                    panel.abline(a=0,b=1)}) 
    
    
    #plot(p15)
    
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
    
    ### new version 
    p2<-# xyplot(I(PREC-(RO+DRAIN+ET))~deltaSW, annDF,
               xyplot(I(PREC-(RO+DRAIN+ET))~deltaSWT, annDF,
               #main='PREC-RO-DRAIN-ET~deltaSW',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)})
    
    
    ### print plots to file, change numbering if needed
    pdf(paste0(out.dir, '/QC_Water_Balance_',mod.abb,'.pdf',sep=''),width=10,height=8)
    for (i in 1:2) {
        print(get(paste("p",i,sep="")))
    }
    dev.off()
    
    
    
    ##################### Nitrogen balance check ################################
    ### Firstly, we check Delta$NL, Delta$NW, Delta$NFR and Delta$NCR. 
    ### Here, Delta$NL = NGL + NLITIN - NLRETR, where NLRETR is the retranslocation flux. 
    ### Lin COMMENT: since we cannot separate NWRETR, leaf and root retranslocation from each other, 
    ### I combine the three compartments for the N budget
    p1<-# xyplot(I(NGL-NLITIN-NRECYC)~deltaNL,annDF,
        xyplot(I(NGL-NLITIN+NGW-NWLIN+NGFR-NFRLIN-NRECYC)~I(deltaNL+deltaNW+deltaNFR),annDF,
               #main='I(NGL-NLITIN-NLRETR)~deltaNL',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### Lin COMMENT: not printed out for QUINCY
    p2<-xyplot(I(NGW-NWLIN-NWRETR)~deltaNW,annDF,
               #main='I(NGW-NWLIN-NWRETR)~deltaNW',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### Lin COMMENT: not printed out for QUINCY
    p3<-xyplot(I(NGW-NWLIN)~deltaNW,annDF,
               #main='I(NGW-NWLIN-NWRETR)~deltaNW',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### Lin COMMENT: not printed out for QUINCY
    p4<-xyplot(I(NGFR-NFRLIN)~deltaNFR,annDF,
               #main='I(NGFR-NFRLIN-NFRRETR)~deltaNFR',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    
    p5<-xyplot(I(NGCR-NCRLIN)~deltaNCR,annDF,
               #main='I(NGCR-NCRLIN-NCRRETR)~deltaNCR',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    
    ### This is to check finelitter influx. Total NFLIT = NFLITA + NFLITB. 
    p6<-xyplot(I(NFLITA+NFLITB)~NFLIT,annDF,
               #main='I(NFLITA+NFLITB)~NFLIT',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    
    p7<-# xyplot(I(NRECYC)~deltaNSTOR,annDF,
        xyplot(I(NRECYC+NGSTR-NSTRLIN)~deltaNSTOR,annDF,
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
    p8<-# xyplot(I(NUP+NRECYC)~I(NGL+NGFR+NGCR+NGW+NREPR),annDF,
          xyplot(I(NUP+NRECYC)~I(NGL+NGFR+NGCR+NGW+NREPR+deltaNSTOR+NSTRLIN),annDF,
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### Some models could have a NSTOR pool, 
    ### so not all nitrogen available for plant is used for growth. 
    ### Here we are looking at DeltaNSTOR 
    ### to see if it helps to close the mass balance if the figure above doesn't close its mass balance. 
    ### Lin COMMENT: balance resolved in p8
    #p9<-xyplot(I(NUP+NRECYC-NGL-NGFR-NGCR-NGW-NREPR)~deltaNSTOR,annDF,
    #           #main='I(NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGFR-NGCR-NGW)~deltaNSTOR',
    #           auto.key=T,
    #           scales=list(relation='free'),
    #           panel=function(...){
    #               panel.xyplot(...)
    #               panel.abline(a=0,b=1)}) 
    
    
    ### Similar to the above figure, we are checking if Delta$NSTOR helps to close the budget. 
    ### Lin COMMENT: not applicable for QUINCY since NFIX goes to soil not plant
    ### MJ: same as above, mass balance why not closed?
    p9<-xyplot(I(NUP+NRECYC-NGL-NGFR-NGCR-NGW-NREPR)~deltaNSTOR,annDF,
               #main='I(NFIX+NUP+NLRETR+NWRETR+NFRRETR+NCRRETR-NGL-NGFR-NGCR-NGW)~deltaNSTOR',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    
    ### Now we are checking the whole ecosystem N input and output. 
    ### Full equation is:
    ### NDEP+NFIX-NLEACH-NVOL = DeltaNL+DeltaNW+DeltaNCR+DeltaNFR+DeltaNSOIL+DeltaNFLIT+DeltaNCLITB
    p10<-# xyplot(I(NDEP+NFIX-NLEACH-NVOL)~(I(deltaNL+deltaNW+deltaNCR+deltaNFR+deltaNSOIL+deltaNFLIT+deltaNCLITB)),annDF,
         xyplot(I(NDEP+NFIX-NLEACH-NVOL)~
                (I(deltaNL+deltaNW+deltaNCR+deltaNFR+deltaNSTOR+
                       deltaNSEED+deltaNFRUIT+deltaNTSOIL+deltaNFLIT+deltaNCLITB)),annDF,
                #main='I(NDEP+NFIX-NLEACH-NVOL)~(I(deltaNL+deltaNW+deltaNCR+deltaNFR+deltaNSOIL+deltaNFLIT+deltaNCLITB))',
                auto.key=T,
                scales=list(relation='free'),
                panel=function(...){
                    panel.xyplot(...)
                    panel.abline(a=0,b=1)}) 
    
    p11<-xyplot(I(NDEP+NFIX-NLEACH-NVOL)~
                    (I(deltaNL+deltaNW+deltaNCR+deltaNFR+deltaNSTOR+deltaNTSOIL+deltaNFLIT+deltaNCLITB+deltaNSEED+deltaNFRUIT)),annDF,
                #main='I(NDEP+NFIX-NLEACH-NVOL)~(I(deltaNL+deltaNW+deltaNCR+deltaNFR+deltaNSOIL+deltaNFLIT+deltaNCLITB))',
                auto.key=T,
                scales=list(relation='free'),
                panel=function(...){
                    panel.xyplot(...)
                    panel.abline(a=0,b=1)}) 
    
    
    ### A different way to check whole ecosystem N budget, as:
    ### Lin COMMENT: this is acutually checking the soil N budget
    ### NDEP+NFIX+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL = DeltaNSOIL+DeltaNFLIT+DeltaNCLITB
    p12<-xyplot(I(NDEP+NFIX+NVEGLIN-NUP-NLEACH-NVOL)~I(deltaNTSOIL+deltaNFLIT+deltaNCLITB),annDF,
                #main='I(NDEP+NFIX+NLITIN+NWLIN+NCRLIN+NFRLIN-NUP-NLEACH-NVOL)~I(deltaNSOIL+deltaNFLIT+deltaNCLITB)',
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
    
    
    p14<-xyplot(I(NTPMIN+NTPORG)~NTSOIL,annDF,
                #main='I(NPMIN+NPORG)~NSOIL',
                auto.key=T,
                scales=list(relation='free'),
                panel=function(...){
                    panel.xyplot(...)
                    panel.abline(a=0,b=1)}) 
    
    ### print plots to file, change numbering if needed
    pdf(paste0(out.dir, '/QC_Nitrogen_Balance_',mod.abb,'.pdf',sep=''),width=10,height=8)
    for (i in 1:14) {
        print(get(paste("p",i,sep="")))
    }
    dev.off()
    
    
    
    ##################### Phosphorus balance check ################################
    ### The net influx - outflux should equal to the change in all ecosystem P pools: 
    ### Partical transport
    ### MJ: missing outflux? What about PWEA? - parent pool in soil pool
    p1<-xyplot(I(PDEP-PLEACH)~
                   (I(deltaPL+deltaPW+deltaPCR+deltaPFR+deltaPSTOR+deltaPSEED+deltaPFRUIT+
                                     deltaPTSOIL+deltaPFLIT+deltaPCLITB)),annDF,
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    p2<-xyplot(I(PDEP+PVEGLIN-PUP-PLEACH)~I(deltaPTSOIL+deltaPFLIT+deltaPCLITB),annDF,       
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### Lin: the reason for the unbalance is the missing litterfall of seedbed in PVEGLIN
    p3<-xyplot(I(PUP-PVEGLIN)~
                   I(deltaPL+deltaPW+deltaPCR+deltaPFR+deltaPSTOR+deltaPSEED+deltaPFRUIT),annDF,
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    
    ### Next, we check changes in major vegetation P pools. 
    ### It should equal to production flux - retranslocation flux - litterfall. 
    p4<- xyplot(I(PGL-PLITIN+PGW-PWLIN+PGFR-PFRLIN-PRECYC)~I(deltaPL+deltaPW+deltaPFR),annDF,
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### Lin: not printed out for QUINCY
    p5<-xyplot(I(PGW-PWLIN-PWRETR)~deltaPW,annDF,
               #main='I(PGW-PWLIN-PWRETR)~deltaPW',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### Lin: not printed out for QUINCY
    p6<-xyplot(I(PGFR-PFRLIN-PFRRETR)~deltaPFR,annDF,
               #main='I(PGFR-PFRLIN-PFRRETR)~deltaPFR',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### Lin: not printed out for QUINCY
    p7<-xyplot(I(PGCR-PCRLIN-PCRRETR)~deltaPCR,annDF,
               #main='I(PGCR-PCRLIN-PCRRETR)~deltaPCR',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    
    ### This is to check P litter flux. 
    p8<-xyplot(I(PFLITA+PFLITB)~PFLIT,annDF,
               #main='I(PFLITA+PFLITB)~PFLIT',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### Now we check the total P required to make new vegetation: 
    ### PUP+PLRETR+PWRETR+PFRRETR+PCRRETR=PGL+PGFR+PGCR+PGW
    p9<-# xyplot(I(PUP+PRECYC)~I(PGL+PGFR+PGCR+PGW+PREPR),annDF,
        xyplot(I(PUP+PRECYC)~I(PGL+PGFR+PGCR+PGW+PREPR+deltaPSTOR+PSTRLIN),annDF,
               #main='I(PUP+PLRETR+PWRETR+PFRRETR+PCRRETR)~I(PGL+PGFR+PGCR+PGW)',
               auto.key=T,
               scales=list(relation='free'),
               panel=function(...){
                   panel.xyplot(...)
                   panel.abline(a=0,b=1)}) 
    
    ### This is to check whole ecosystem P flux, 
    ### Lin: this is soil P budget. NOTE: weathering is already taken account in deltaPTSOIL
    ### which means that total in - out = net change:
    ### PDEP+PWEA+PLITIN+PWLIN+PCRLIN+PFRLIN-PUP-PLEACH = DeltaPSOIL+DeltaFLIT+DeltaPCLITB
    p10<-# xyplot(I(PDEP+PWEA+PLITIN+PWLIN+PCRLIN+PFRLIN-PUP-PLEACH)~I(deltaPSOIL+deltaPFLIT+deltaPCLITB),annDF,
         xyplot(I(PDEP+PVEGLIN-PUP-PLEACH)~
                    I(deltaPTSOIL+deltaPFLIT+deltaPCLITB),annDF,       
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
    vegDF <- data.frame(rep(c("CL", "CW", "CFR", "CCR", "CSOIL"), 2), 
                        rep(c("obs", "sim"), each=5), NA, NA)
    colnames(vegDF) <- c("Variable", 
                         "Group",
                         "meanvalue",
                         "sevalue")
    
    vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CL"] <- 151
    vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CW"] <- 4558
    vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CFR"] <- 227
    vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CCR"] <- 606
    vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CSOIL"] <- 2183
    
    vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CL"] <- 14
    vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CW"] <- 321
    vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CFR"] <- 5
    vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CCR"] <- 60
    vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CSOIL"] <- 280
    
    
    ### calcualte annual means in the simulated data
    poolDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012 & DOY == 1)
    
    ### assign values
    vegDF$meanvalue[vegDF$Group=="sim"&vegDF$Variable=="CL"] <- mean(poolDF$CL, na.rm=T)
    vegDF$meanvalue[vegDF$Group=="sim"&vegDF$Variable=="CW"] <- mean(poolDF$CW, na.rm=T)
    vegDF$meanvalue[vegDF$Group=="sim"&vegDF$Variable=="CFR"] <- mean(poolDF$CFR, na.rm=T)
    vegDF$meanvalue[vegDF$Group=="sim"&vegDF$Variable=="CCR"] <- mean(poolDF$CCR, na.rm=T)
    ### Lin: we are printing out soilC for top 30cm, while the measurement is top 10cm. 
    ### I use a nasty fix here by assuming 50% of soilC in top 10cm
    vegDF$meanvalue[vegDF$Group=="sim"&vegDF$Variable=="CSOIL"] <- mean(poolDF$CSOIL/2, na.rm=T)
    
    vegDF$sevalue[vegDF$Group=="sim"&vegDF$Variable=="CL"] <- se(poolDF$CL, na.rm=T)
    vegDF$sevalue[vegDF$Group=="sim"&vegDF$Variable=="CW"] <- se(poolDF$CW, na.rm=T)
    vegDF$sevalue[vegDF$Group=="sim"&vegDF$Variable=="CFR"] <- se(poolDF$CFR, na.rm=T)
    vegDF$sevalue[vegDF$Group=="sim"&vegDF$Variable=="CCR"] <- se(poolDF$CCR, na.rm=T)
    vegDF$sevalue[vegDF$Group=="sim"&vegDF$Variable=="CSOIL"] <- se(poolDF$CSOIL/2, na.rm=T)
    
    
    ### Plotting
    p1 <- ggplot(data=vegDF, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        #geom_errorbar(aes(x=Group, ymin=meanvalue-sevalue, 
        #                  ymax=meanvalue+sevalue), 
        #              position="dodge", width=0.2) +
        ggtitle("Major ecosystem carbon pools")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Carbon pools (g C " * m^2*")")))
    
    
    
    ################# allocation coefficient  ####################
    ### Allocation coefficients are calculated different comparing the data and the model. 
    ### In the EucFACE data, allocation to leaf includes allocation to overstorey and understorey leaves, 
    ### and allocation to root includes allocation to overstorey and understorey roots. 
    ### In the data, there is also an additional allocation coefficient to Mycorrhizae, 
    ### which can be grouped with allocation to root as total belowground allocation. 
    ### This total belowground allocation is comparable to allocation coefficient to root in the model. 
    
    ### create a DF to store observation data for allocation coefficients
    allocDF <- data.frame(rep(c("leaf", "wood", "root", "exudation", 
                                "belowground", "reproduction"), 2),
                          rep(c("obs", "sim"), each = 6), NA)
    colnames(allocDF) <- c("Variable", 
                           "Group",
                           "meanvalue")
    
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="leaf"] <- 0.48
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="wood"] <- 0.20
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="root"] <- 0.22
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="exudation"] <- 0.10
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="belowground"] <- 0.32
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="reproduction"] <- 0.0
    
    
    ### calcualte annual means in the simulated data
    subDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012)
    
    fluxDF <- summaryBy(GPP+NEP+NPP+CGL+CGW+CGFR+CGCR+CEX+CREPR+RAU~YEAR, data=subDF, FUN=sum, na.rm=T, keep.names=T)
    
    ### assign values
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="leaf"] <- mean(fluxDF$CGL/fluxDF$NPP)
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="wood"] <- round(mean(fluxDF$CGW/fluxDF$NPP),2)
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="root"] <- round(mean((fluxDF$CGFR+fluxDF$CGCR)/fluxDF$NPP),2)
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="exudation"] <- round(mean((fluxDF$CEX)/fluxDF$NPP),2)
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="belowground"] <- round(mean((fluxDF$CGFR+fluxDF$CGCR+fluxDF$CEX)/fluxDF$NPP),2)
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="reproduction"] <- round(mean((fluxDF$CREPR)/fluxDF$NPP),2)
    
    
    allocDF2 <- allocDF[allocDF$Variable%in%c("leaf", "wood", "root", "exudation", "reproduction"),]
    
    ### Plotting
    p2 <- ggplot(data=allocDF2, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        ggtitle("Allocation coefficient")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("allocation coefficients")
    
    
    ################# Major carbon fluxes  ####################
    ### create a DF to store observation data for allocation coefficients
    outDF <- data.frame(rep(c("NEP", "GPP", "NPP", "CUE", "RAU"), 2), 
                        rep(c("obs", "sim"), each = 5), NA)
    colnames(outDF) <- c("Variable", 
                         "Group",
                         "meanvalue")
    
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="NEP"] <- -8
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="GPP"] <- 1563
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="NPP"] <- 484
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="CUE"] <- 0.31
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="RAU"] <- 1079
    
    
    ### these fluxes were calculated above already
    ### assign values
    outDF$meanvalue[outDF$Group=="sim"&outDF$Variable=="NEP"] <- round(mean(fluxDF$NEP),2)
    outDF$meanvalue[outDF$Group=="sim"&outDF$Variable=="GPP"] <- round(mean(fluxDF$GPP),2)
    outDF$meanvalue[outDF$Group=="sim"&outDF$Variable=="NPP"] <- round(mean(fluxDF$NPP),2)
    outDF$meanvalue[outDF$Group=="sim"&outDF$Variable=="CUE"] <- round(mean(fluxDF$NPP/fluxDF$GPP),2)
    outDF$meanvalue[outDF$Group=="sim"&outDF$Variable=="RAU"] <- round(mean(fluxDF$RAU),2)
    
    ### plotDF1
    plotDF1 <- outDF[outDF$Variable%in%c("GPP", "NPP", "RAU", "NEP"),]
    
    ### plotting GPP, NPP, and RAU
    p3 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("Major carbon fluxes")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Carbon fluxes (g C " * m^2 * " " * yr^-1 * ")")))
    
    
    ################# Major nutrient pools  ####################
    ### Below I provide several variables to help constrain the nutrient cycles in the model, 
    ### namely labile inorganic P pool (PLAB), 
    ### soil net N and P mineralization rate (NMIN and PMIN), 
    ### plant N and P uptake (NUP and PUP), 
    ### and soil N and P leaching (NLEACH and PLEACH). 
    ### We did not include total soil P pool, 
    ### because its size could be misleading given that 
    ### the majority of the P in the soil is stored as occluded form unavailable for plants. 
    ### Note that in the table below, simulated results are for top 30 cm of the soil, 
    ### but observed data are for top 10 cm only. 
    
    ### create a DF to store observation data for vegetation carbon stocks
    pDF <- data.frame(rep(c("PLAB", "PMIN", "PBIOCHMIN", "NMIN", "NUP", "PUP",
                            "NLEACH", "PLEACH"), 2), 
                      rep(c("obs", "sim"), each = 8), NA)
    colnames(pDF) <- c("Variable", 
                       "Group",
                       "meanvalue")
    
    pDF$meanvalue[pDF$Group=="obs"&pDF$Variable=="PLAB"] <- 0.17
    pDF$meanvalue[pDF$Group=="obs"&pDF$Variable=="PMIN"] <- 0.3
    pDF$meanvalue[pDF$Group=="obs"&pDF$Variable=="NMIN"] <- 8.81
    
    
    ### calcualte annual means in the simulated data
    subDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012)
    
    poolDF <- summaryBy(PLAB~YEAR, data=subDF, FUN=mean, na.rm=T, keep.names=T)
    fluxDF <- summaryBy(PMIN+PBIOCHMIN+NMIN+NLEACH+PLEACH+NUP+PUP~YEAR, data=subDF, FUN=sum, na.rm=T, keep.names=T)
    
    ### assign values
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="PLAB"] <- round(poolDF$PLAB[poolDF$YEAR=="2016"], 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="PMIN"] <- round(mean(fluxDF$PMIN + fluxDF$PBIOCHMIN), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="NMIN"] <- round(mean(fluxDF$NMIN), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="PLEACH"] <- round(mean(fluxDF$PLEACH), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="NLEACH"] <- round(mean(fluxDF$NLEACH), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="PUP"] <- round(mean(fluxDF$PUP), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="NUP"] <- round(mean(fluxDF$NUP), 3)
    
    
    plotDF1 <- pDF[pDF$Variable%in%c("PLAB", "PMIN"),]
    plotDF2 <- pDF[pDF$Variable%in%c("NMIN"),]
    
    ### plotting
    p4 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("Major phosphorus pools")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Phosphorus pools (g P " * m^2 * ")")))
    
    
    p5 <- ggplot(data=plotDF2, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("Major nitrogen pool")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Nitrogen pool (g N " * m^2 * ")")))
    
    
    
    ################# stoichiometry  ####################
    ### create a DF to store observation data 
    stDF <- data.frame(rep(c("leaf", "sapwood", "wood", "fineroot", "soil"),2), 
                       rep(c("obs", "sim"), each = 5), 
                       NA, NA, NA, NA, NA, NA)
    colnames(stDF) <- c("Variable", "Group", 
                        "CN.mean", "CP.mean",  "NP.mean",
                        "CN.se", "CP.se",  "NP.se")
    
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 35.5
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 101.6
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="wood"] <- 110.2 
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 56.9
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="soil"] <- 13.8 
    
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 722 
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 3705 
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="wood"] <- 7696 
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 1626
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="soil"] <- 224
    
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 22.9 
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 35.6
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="wood"] <- 33.7 
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 28.7
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="soil"] <- 16.4 
    
    
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 2.7
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 14.7
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="wood"] <- 30.3
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 4.6
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="soil"] <- 1.0
    
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 33
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 702
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="wood"] <- 982
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 81
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="soil"] <- 39
    
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="leaf"] <-  0.1
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 2.1
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="wood"] <- 2.7
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 3.3
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="soil"] <- 3.4
    
    
    
    
    ### create a DF to store simulated stocks
    tmpDF <- data.frame(rep(c("leaf", "wood", "fineroot", "soil"), 4), 
                        rep(c(2013:2016), each = 4), NA, NA, NA)
    colnames(tmpDF) <- c("Variable", 
                         "Year",
                         "Cstock",
                         "Nstock", 
                         "Pstock")
    
    ### calcualte annual means in the simulated data
    poolDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012 & DOY == 1)
    
    ### tmpDF assignment
    for (i in 2013:2016) {
        tmpDF$Cstock[tmpDF$Year==i&tmpDF$Variable=="leaf"] <- poolDF$CL[poolDF$YEAR == i]
        tmpDF$Cstock[tmpDF$Year==i&tmpDF$Variable=="wood"] <- poolDF$CW[poolDF$YEAR == i]
        tmpDF$Cstock[tmpDF$Year==i&tmpDF$Variable=="fineroot"] <- poolDF$CFR[poolDF$YEAR == i]
        tmpDF$Cstock[tmpDF$Year==i&tmpDF$Variable=="soil"] <- poolDF$CSOIL[poolDF$YEAR == i]
        
        tmpDF$Nstock[tmpDF$Year==i&tmpDF$Variable=="leaf"] <- poolDF$NL[poolDF$YEAR == i]
        tmpDF$Nstock[tmpDF$Year==i&tmpDF$Variable=="wood"] <- poolDF$NW[poolDF$YEAR == i]
        tmpDF$Nstock[tmpDF$Year==i&tmpDF$Variable=="fineroot"] <- poolDF$NFR[poolDF$YEAR == i]
        tmpDF$Nstock[tmpDF$Year==i&tmpDF$Variable=="soil"] <- poolDF$NSOIL[poolDF$YEAR == i]
        
        tmpDF$Pstock[tmpDF$Year==i&tmpDF$Variable=="leaf"] <- poolDF$PL[poolDF$YEAR == i]
        tmpDF$Pstock[tmpDF$Year==i&tmpDF$Variable=="wood"] <- poolDF$PW[poolDF$YEAR == i]
        tmpDF$Pstock[tmpDF$Year==i&tmpDF$Variable=="fineroot"] <- poolDF$PFR[poolDF$YEAR == i]
        tmpDF$Pstock[tmpDF$Year==i&tmpDF$Variable=="soil"] <- poolDF$PSOIL[poolDF$YEAR == i]
        
    }
    
    tmpDF$CN <- with(tmpDF, Cstock/Nstock)
    tmpDF$CP <- with(tmpDF, Cstock/Pstock)
    tmpDF$NP <- with(tmpDF, Nstock/Pstock)
    
    tmpDF2 <- summaryBy(CN+CP+NP~Variable, FUN=c(mean, se), data=tmpDF, keep.names=T)
    
    
    ### assign values
    stDF$CN.mean[stDF$Group=="sim"&stDF$Variable=="leaf"] <- tmpDF2$CN.mean[tmpDF2$Variable=="leaf"]
    stDF$CN.mean[stDF$Group=="sim"&stDF$Variable=="wood"] <-  tmpDF2$CN.mean[tmpDF2$Variable=="wood"]
    stDF$CN.mean[stDF$Group=="sim"&stDF$Variable=="fineroot"] <-  tmpDF2$CN.mean[tmpDF2$Variable=="fineroot"]
    stDF$CN.mean[stDF$Group=="sim"&stDF$Variable=="soil"] <-  tmpDF2$CN.mean[tmpDF2$Variable=="soil"]
    
    
    stDF$CP.mean[stDF$Group=="sim"&stDF$Variable=="leaf"] <-  tmpDF2$CP.mean[tmpDF2$Variable=="leaf"]
    stDF$CP.mean[stDF$Group=="sim"&stDF$Variable=="wood"] <-  tmpDF2$CP.mean[tmpDF2$Variable=="wood"]
    stDF$CP.mean[stDF$Group=="sim"&stDF$Variable=="fineroot"] <- tmpDF2$CP.mean[tmpDF2$Variable=="fineroot"]
    stDF$CP.mean[stDF$Group=="sim"&stDF$Variable=="soil"] <-  tmpDF2$CP.mean[tmpDF2$Variable=="soil"]
    
    stDF$NP.mean[stDF$Group=="sim"&stDF$Variable=="leaf"] <- tmpDF2$NP.mean[tmpDF2$Variable=="leaf"]
    stDF$NP.mean[stDF$Group=="sim"&stDF$Variable=="wood"] <- tmpDF2$NP.mean[tmpDF2$Variable=="wood"]
    stDF$NP.mean[stDF$Group=="sim"&stDF$Variable=="fineroot"] <- tmpDF2$NP.mean[tmpDF2$Variable=="fineroot"]
    stDF$NP.mean[stDF$Group=="sim"&stDF$Variable=="soil"] <- tmpDF2$NP.mean[tmpDF2$Variable=="soil"]
    
    
    
    stDF$CN.se[stDF$Group=="sim"&stDF$Variable=="leaf"] <- tmpDF2$CN.se[tmpDF2$Variable=="leaf"]
    stDF$CN.se[stDF$Group=="sim"&stDF$Variable=="wood"] <-  tmpDF2$CN.se[tmpDF2$Variable=="wood"]
    stDF$CN.se[stDF$Group=="sim"&stDF$Variable=="fineroot"] <-  tmpDF2$CN.se[tmpDF2$Variable=="fineroot"]
    stDF$CN.se[stDF$Group=="sim"&stDF$Variable=="soil"] <-  tmpDF2$CN.se[tmpDF2$Variable=="soil"]
    
    
    stDF$CP.se[stDF$Group=="sim"&stDF$Variable=="leaf"] <-  tmpDF2$CP.se[tmpDF2$Variable=="leaf"]
    stDF$CP.se[stDF$Group=="sim"&stDF$Variable=="wood"] <-  tmpDF2$CP.se[tmpDF2$Variable=="wood"]
    stDF$CP.se[stDF$Group=="sim"&stDF$Variable=="fineroot"] <- tmpDF2$CP.se[tmpDF2$Variable=="fineroot"]
    stDF$CP.se[stDF$Group=="sim"&stDF$Variable=="soil"] <- tmpDF2$CP.se[tmpDF2$Variable=="soil"]
    
    
    stDF$NP.se[stDF$Group=="sim"&stDF$Variable=="leaf"] <- tmpDF2$NP.se[tmpDF2$Variable=="leaf"]
    stDF$NP.se[stDF$Group=="sim"&stDF$Variable=="wood"] <- tmpDF2$NP.se[tmpDF2$Variable=="wood"]
    stDF$NP.se[stDF$Group=="sim"&stDF$Variable=="fineroot"] <- tmpDF2$NP.se[tmpDF2$Variable=="fineroot"]
    stDF$NP.se[stDF$Group=="sim"&stDF$Variable=="soil"] <- tmpDF2$NP.se[tmpDF2$Variable=="soil"]
    
    ### plot
    p6 <- ggplot(data=stDF, 
                 aes(Group, CN.mean, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        #geom_errorbar(aes(x=Group, ymin=CN.mean-CN.se, 
        #                  ymax=CN.mean+CN.se), 
        #              position=position_dodge2(), width=0.2) +
        ggtitle("CN stoichiometry")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("CN stoichiometry")
    
    p7 <- ggplot(data=stDF, 
                 aes(Group, CP.mean, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("CP stoichiometry")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("CP stoichiometry")
    
    
    p8 <- ggplot(data=stDF, 
                 aes(Group, NP.mean, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("NP stoichiometry")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("NP stoichiometry")
    
    
    
    ################# Nutrient retranslocation coefficients  ####################
    
    ### subset and calculate
    subDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012)
    
    tmpDF <- summaryBy(NGL+PGL+NRECYC+PRECYC~YEAR, data=subDF, FUN=sum, keep.names=T, na.rm=T)
    
    
    ### create DF for output
    rtDF <- data.frame(rep(c("leafN", "leafP"), 2), 
                       rep(c("obs", "sim"), each = 2), NA)
    colnames(rtDF) <- c("Variable", "Group", "meanvalue")
    
    ### assign values
    rtDF$meanvalue[rtDF$Group=="obs"&rtDF$Variable=="leafN"] <- 0.31
    rtDF$meanvalue[rtDF$Group=="obs"&rtDF$Variable=="leafP"] <- 0.53
    
    rtDF$meanvalue[rtDF$Group=="sim"&rtDF$Variable=="leafN"] <- round(mean(tmpDF$NRECYC/tmpDF$NGL),2)
    rtDF$meanvalue[rtDF$Group=="sim"&rtDF$Variable=="leafP"] <- round(mean(tmpDF$PRECYC/tmpDF$PGL),2)
    
    
    ### plotting
    p9 <- ggplot(data=rtDF, 
                 aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("Leaf retranslocation")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Leaf retranslocation")
    
    
    ### print plots to file, change numbering if needed
    pdf(paste0(out.dir, '/QC_Time_averaged_validation_',mod.abb,'.pdf',sep=''),width=10,height=8)
    for (i in 1:9) {
        print(get(paste("p",i,sep="")))
    }
    dev.off()
    
    
    ##########################################################################
    #### Step 4. Time-varying validation
    
    #### Leaf area index
    ### A time series LAI data over the period of 2012 - 2016 was provided for validation purpose. 
    ### Models should aim to match the magnitude of LAI as well as its temporal patterns. 
    ### Note that in the observed dataset, the LAI data is really indicative of the vegetation structure as well as canopy leaf area. 
    ### validation LAI
    laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    laiDF <- laiDF[laiDF$Trt=="aCO2",]
    laiDF$Date <- as.Date(as.character(laiDF$Date))
    
    ### simulated LAI, subset
    subDF <- subset(modDF, YEAR <= 2016)
    subDF <- subDF[,c("YEAR", "DOY", "Date", "LAI")]
    subDF$Date <- as.Date(as.character(subDF$Date))
    
    ### merge the two dataset
    testDF1 <- merge(subDF, laiDF, by="Date", all=T)
    
    ### plot all data
    p1 <- ggplot(testDF1, aes(x=Date)) +
        geom_errorbar(aes(ymin=lai-laiSD,
                          ymax=lai+laiSD, color="obs"))+
        geom_line(aes(y=LAI, color="sim"), lwd = 1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        ylab("LAI")+
        scale_colour_manual("", 
                            values = c("obs"="black", "sim"="red2"),
                            labels = c("Observed", "Simulated"))
    
    ### Now we can check the goodness-of-fit of all days where observation is available. 
    ### A perfect fit should have slope of 1 and intercept of 0. 
    ### subset only days where observations are available
    testDF2 <- testDF1[complete.cases(testDF1$lai),]
    
    lm.fit <- lm(testDF2$LAI~testDF2$lai)
    
    ### plot all data
    p2 <- ggplot(testDF2, aes(x=lai, y=LAI)) +
        geom_point()+
        theme_linedraw() +
        geom_smooth(method="lm")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        ylab("simulated LAI")+
        xlab("observed LAI")
    
    
    #### Soil respiration
    ### The measured soil respiration rate represents both root 
    ### and soil heterotrophic respiration flux. 
    ### It was up-scaled from the LICOR chambers by averaging 
    ### all measurements within the same treatment. 
    ### It was a model product, 
    ### in that we used DAMM model to establish relationship with soil temperature, 
    ### and then obtained the daily rate throughout the year. 
    ### Nevertheless, we expect modelers to provide a good match simulation to this dataset. 
    
    ### Note that we didn't ask the modelers to output soil respiration flux in the output protocol. 
    ### Please add heterotrophic respiration and root respiration to obtain soil respiration flux. 
    ### Also, please note that, the unit for all carbon fluxes is given in the output protocol, as gC m-2 d-1. 
    ### validation Rsoil
    rsoilDF <- read.csv("validation_dataset/EucFACE_daily_soil_respiration_flux_2013_2015.csv")
    rsoilDF <- rsoilDF[rsoilDF$Trt=="aCO2",]
    rsoilDF$Date <- as.Date(as.character(rsoilDF$Date))
    
    ### convert unit, from mg m-2 d-1 to g m-2 d-1
    rsoilDF$Rsoil_g_m2_d <- rsoilDF$Rsoil_mg_m2_d / 1000.0
    rsoilDF$RsoilSD_g <- rsoilDF$RsoilSD / 1000.0
    
    
    ### simulated Rsoil, subset
    subDF <- subset(modDF, YEAR <= 2015 & YEAR > 2012)
    subDF <- subDF[,c("YEAR", "DOY", "Date", "RHET", "RCR", "RFR")]
    subDF$Date <- as.Date(as.character(subDF$Date))
    subDF$Rsoil_sim <- with(subDF, RHET+RCR+RFR)
    
    
    
    ### merge the two dataset
    testDF1 <- merge(subDF, rsoilDF, by="Date", all=T)
    
    ### plot all data
    p3 <- ggplot(testDF1, aes(x=Date)) +
        geom_errorbar(aes(ymin=Rsoil_g_m2_d-RsoilSD_g,
                          ymax=Rsoil_g_m2_d+RsoilSD_g, color="obs"))+
        geom_line(aes(y=Rsoil_sim, color="sim"), lwd = 1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        ylab("Soil respiration flux")+
        scale_colour_manual("", 
                            values = c("obs"="black", "sim"="red2"),
                            labels = c("Observed", "Simulated"))
    
    
    #### Soil water content
    ### Soil water content is more complex and non-linear, 
    ### depending on many model-specific settings. 
    ### The validation dataset only serves as a guidance to evaluate your model performance. 
    ### validation Rsoil
    swcDF <- read.csv("validation_dataset/EucFACE_SWC_2012_2019.csv")
    
    ### convert unit from VWC to kg H2O m-2
    swcDF$multiplier <- ifelse(swcDF$Depth%in%c(25,50,75,100,125,150), 0.25, 0.5)
    
    swcDF$WC_kg_m2 <- swcDF$VWC * swcDF$multiplier * 1000.0 / 100.0
    
    sumDF <- summaryBy(WC_kg_m2~Location+Date, FUN=sum, data=swcDF, keep.names=T, na.rm=T)
    
    ### process simulation data
    subDF <- modDF[,c("YEAR", "DOY", "Date", "SW", "SWPA")]
    
    plotDF <- merge(subDF, sumDF, by="Date", all=T)
    
    
    ### plot all data
    p4 <- ggplot(plotDF, aes(x=Date)) +
        geom_point(aes(y=WC_kg_m2, color="obs"))+
        geom_point(aes(y=SW, color="sim"), lwd = 1) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        ylab("Soil water (kg m-2)")+
        scale_colour_manual("", 
                            values = c("obs"="black", "sim"="red2"),
                            labels = c("Observed", "Simulated"))
    
    
    ### print plots to file, change numbering if needed
    pdf(paste0(out.dir, '/QC_Time_varying_validation_',mod.abb,'.pdf',sep=''),width=10,height=8)
    for (i in 1:4) {
        print(get(paste("p",i,sep="")))
    }
    dev.off()
    
    ##########################################################################
    
    ### End.
}
