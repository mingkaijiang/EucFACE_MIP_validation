translate_GDAY_simulation_into_EucFACE_MIP_format <- function(met.path,
                                                              sim.path,
                                                              out.path) {
    
    #######################################################################################################
    ### This is the script to translate GDAY output into EucFACE Multi-model intercomparison project
    ### requested output format with request output variables
    
    ### met.path is the path where met forcing data is stored
    ### sim.path is the path where model simulation output is stored
    ### out.path is the path where translated output is stored
    
    #######################################################################################################
    
    ### prepare a dataframe to assign correct file names     
    sim.scenarios <- c("EUC_simulated_DRY_AMB_2012_2019",
                       "EUC_simulated_DRY_ELE_2012_2019",
                       "EUC_simulated_WET_AMB_2012_2019",
                       "EUC_simulated_WET_ELE_2012_2019",
                       "EUC_simulated_DRY_AMB_NOP_2020_2069",
                       "EUC_simulated_DRY_AMB_MDP_2020_2069",
                       "EUC_simulated_DRY_AMB_HIP_2020_2069",
                       "EUC_simulated_DRY_ELE_NOP_2020_2069",
                       "EUC_simulated_DRY_ELE_MDP_2020_2069",
                       "EUC_simulated_DRY_ELE_HIP_2020_2069",
                       "EUC_simulated_WET_AMB_NOP_2020_2069",
                       "EUC_simulated_WET_AMB_MDP_2020_2069",
                       "EUC_simulated_WET_AMB_HIP_2020_2069",
                       "EUC_simulated_WET_ELE_NOP_2020_2069",
                       "EUC_simulated_WET_ELE_MDP_2020_2069",
                       "EUC_simulated_WET_ELE_HIP_2020_2069")
    
    met.scenarios <- c("EUC_met_DRY_AMB_daily_2012_2019",
                       "EUC_met_DRY_ELE_daily_2012_2019",
                       "EUC_met_WET_AMB_daily_2012_2019",
                       "EUC_met_WET_ELE_daily_2012_2019",
                       "EUC_met_DRY_AMB_NOP_daily_2020_2069",
                       "EUC_met_DRY_AMB_MDP_daily_2020_2069",
                       "EUC_met_DRY_AMB_HIP_daily_2020_2069",
                       "EUC_met_DRY_ELE_NOP_daily_2020_2069",
                       "EUC_met_DRY_ELE_MDP_daily_2020_2069",
                       "EUC_met_DRY_ELE_HIP_daily_2020_2069",
                       "EUC_met_WET_AMB_NOP_daily_2020_2069",
                       "EUC_met_WET_AMB_MDP_daily_2020_2069",
                       "EUC_met_WET_AMB_HIP_daily_2020_2069",
                       "EUC_met_WET_ELE_NOP_daily_2020_2069",
                       "EUC_met_WET_ELE_MDP_daily_2020_2069",
                       "EUC_met_WET_ELE_HIP_daily_2020_2069")
    
    out.names <- c("EUC_GDAYP_OBS_VAR_AMB_NOP_D",
                   "EUC_GDAYP_OBS_VAR_ELE_NOP_D",
                   "EUC_GDAYP_OBS_FIX_AMB_NOP_D",
                   "EUC_GDAYP_OBS_FIX_ELE_NOP_D",
                   "EUC_GDAYP_PRD_VAR_AMB_NOP_D",
                   "EUC_GDAYP_PRD_VAR_AMB_MDP_D",
                   "EUC_GDAYP_PRD_VAR_AMB_HIP_D",
                   "EUC_GDAYP_PRD_VAR_ELE_NOP_D",
                   "EUC_GDAYP_PRD_VAR_ELE_MDP_D",
                   "EUC_GDAYP_PRD_VAR_ELE_HIP_D",
                   "EUC_GDAYP_PRD_FIX_AMB_NOP_D",
                   "EUC_GDAYP_PRD_FIX_AMB_MDP_D",
                   "EUC_GDAYP_PRD_FIX_AMB_HIP_D",
                   "EUC_GDAYP_PRD_FIX_ELE_NOP_D",
                   "EUC_GDAYP_PRD_FIX_ELE_MDP_D",
                   "EUC_GDAYP_PRD_FIX_ELE_HIP_D")
    
    sceDF <- data.frame(met.scenarios, sim.scenarios, out.names)
    
    ### loop through all data
    for (i in 1:16) {
        ### read in met file
        metDF <- read.csv(paste0(met.path, "/", sceDF$met.scenarios[i], ".csv"), skip=4)
        names(metDF)[1] <- "year"
        
        
        ### read in corresponding simulation file
        simDF <- read.csv(paste0(sim.path, "/", sceDF$sim.scenarios[i], ".csv"), skip=1)
        
        ### merge the two dataset
        myDF <- merge(metDF, simDF, by=c("year", "doy"))
        
        ### order
        myDF <- myDF[order(myDF$year, myDF$doy),]
        
        
        
        ############################################
        ### start converting unit 
        
        ## define a few conversion parameters
        MJ_TO_MOL = 4.6
        SW_TO_PAR = 0.48
        DAYS_TO_HRS = 24.0
        UMOL_TO_MOL = 1E-6
        SW_RAD_TO_PAR = 2.3
        tonnes_per_ha_to_g_m2 = 100.0
        
        ## convert
        # met forcing
        myDF$par <- (myDF$par_am + myDF$par_pm) * MJ_TO_MOL * 12.0
        myDF$vpd <- (myDF$vpd_am + myDF$vpd_pm) / 2
        
        myDF$ndep <- myDF$ndep * tonnes_per_ha_to_g_m2
        myDF$nfix <- myDF$nfix * tonnes_per_ha_to_g_m2
        myDF$pdep <- myDF$pdep * tonnes_per_ha_to_g_m2
        myDF$pfert <- myDF$pfert * tonnes_per_ha_to_g_m2
        
        myDF$apar <- myDF$apar / SW_RAD_TO_PAR
        
        
        
        # carbon pools and fluxes
        myDF$shoot <- myDF$shoot * tonnes_per_ha_to_g_m2
        myDF$branch <- myDF$branch * tonnes_per_ha_to_g_m2
        myDF$stem <- myDF$stem * tonnes_per_ha_to_g_m2
        myDF$root <- myDF$root * tonnes_per_ha_to_g_m2
        myDF$croot <- myDF$croot * tonnes_per_ha_to_g_m2
        myDF$cstore <- myDF$cstore * tonnes_per_ha_to_g_m2
        
        myDF$soilc <- myDF$soilc * tonnes_per_ha_to_g_m2
        
        myDF$litterc <- myDF$litterc * tonnes_per_ha_to_g_m2
        myDF$littercag <- myDF$littercag * tonnes_per_ha_to_g_m2
        myDF$littercbg <- myDF$littercbg * tonnes_per_ha_to_g_m2
        
        myDF$activesoil <- myDF$activesoil * tonnes_per_ha_to_g_m2
        myDF$slowsoil <- myDF$slowsoil * tonnes_per_ha_to_g_m2
        myDF$passivesoil <- myDF$passivesoil * tonnes_per_ha_to_g_m2
        
        myDF$deadleaves <- myDF$deadleaves * tonnes_per_ha_to_g_m2
        myDF$deadbranch <- myDF$deadbranch * tonnes_per_ha_to_g_m2
        myDF$deadstems <- myDF$deadstems * tonnes_per_ha_to_g_m2
        myDF$deadroots <- myDF$deadroots * tonnes_per_ha_to_g_m2
        myDF$deadcroots <- myDF$deadcroots * tonnes_per_ha_to_g_m2
        
        myDF$nep <- myDF$nep * tonnes_per_ha_to_g_m2
        myDF$gpp <- myDF$gpp * tonnes_per_ha_to_g_m2
        myDF$npp <- myDF$npp * tonnes_per_ha_to_g_m2
        myDF$hetero_resp <- myDF$hetero_resp * tonnes_per_ha_to_g_m2
        myDF$auto_resp <- myDF$auto_resp * tonnes_per_ha_to_g_m2
        
        myDF$cpleaf <- myDF$cpleaf * tonnes_per_ha_to_g_m2
        myDF$cpbranch <- myDF$cpbranch * tonnes_per_ha_to_g_m2
        myDF$cpstem <- myDF$cpstem * tonnes_per_ha_to_g_m2
        myDF$cproot <- myDF$cproot * tonnes_per_ha_to_g_m2
        myDF$cpcroot <- myDF$cpcroot * tonnes_per_ha_to_g_m2
        
        myDF$root_exc <- myDF$root_exc * tonnes_per_ha_to_g_m2
        
        # nitrogen pools and fluxes
        myDF$shootn <- myDF$shootn * tonnes_per_ha_to_g_m2
        myDF$branchn <- myDF$branchn * tonnes_per_ha_to_g_m2
        myDF$stemn <- myDF$stemn * tonnes_per_ha_to_g_m2
        myDF$rootn <- myDF$rootn * tonnes_per_ha_to_g_m2
        myDF$crootn <- myDF$crootn * tonnes_per_ha_to_g_m2
        myDF$nstore <- myDF$nstore * tonnes_per_ha_to_g_m2
        
        myDF$soiln <- myDF$soiln * tonnes_per_ha_to_g_m2
        myDF$inorgn <- myDF$inorgn * tonnes_per_ha_to_g_m2
        
        myDF$litternag <- myDF$litternag * tonnes_per_ha_to_g_m2
        myDF$litternbg <- myDF$litternbg * tonnes_per_ha_to_g_m2
        
        myDF$activesoiln <- myDF$activesoiln * tonnes_per_ha_to_g_m2
        myDF$slowsoiln <- myDF$slowsoiln * tonnes_per_ha_to_g_m2
        myDF$passivesoiln <- myDF$passivesoiln * tonnes_per_ha_to_g_m2
        
        myDF$deadleafn <- myDF$deadleafn * tonnes_per_ha_to_g_m2
        myDF$deadbranchn <- myDF$deadbranchn * tonnes_per_ha_to_g_m2
        myDF$deadstemn <- myDF$deadstemn * tonnes_per_ha_to_g_m2
        myDF$deadrootn <- myDF$deadrootn * tonnes_per_ha_to_g_m2
        myDF$deadcrootn <- myDF$deadcrootn * tonnes_per_ha_to_g_m2
        
        myDF$npleaf <- myDF$npleaf * tonnes_per_ha_to_g_m2
        myDF$npbranch <- myDF$npbranch * tonnes_per_ha_to_g_m2
        myDF$npstemimm <- myDF$npstemimm * tonnes_per_ha_to_g_m2
        myDF$npstemmob <- myDF$npstemmob * tonnes_per_ha_to_g_m2
        myDF$nproot <- myDF$nproot * tonnes_per_ha_to_g_m2
        myDF$npcroot <- myDF$npcroot * tonnes_per_ha_to_g_m2
        
        myDF$nuptake <- myDF$nuptake * tonnes_per_ha_to_g_m2
        myDF$ngross <- myDF$ngross * tonnes_per_ha_to_g_m2
        myDF$nmineralisation <- myDF$nmineralisation * tonnes_per_ha_to_g_m2
        myDF$nloss <- myDF$nloss * tonnes_per_ha_to_g_m2
        
        myDF$leafretransn <- myDF$leafretransn * tonnes_per_ha_to_g_m2
        myDF$rootretransn <- myDF$rootretransn * tonnes_per_ha_to_g_m2
        myDF$crootretransn <- myDF$crootretransn * tonnes_per_ha_to_g_m2
        myDF$stemretransn <- myDF$stemretransn * tonnes_per_ha_to_g_m2
        myDF$bramchretransn <- myDF$branchretransn * tonnes_per_ha_to_g_m2
        
        
        # phosphorus pools and fluxes
        myDF$shootp <- myDF$shootp * tonnes_per_ha_to_g_m2
        myDF$branchp <- myDF$branchp * tonnes_per_ha_to_g_m2
        myDF$stemp <- myDF$stemp * tonnes_per_ha_to_g_m2
        myDF$rootp <- myDF$rootp * tonnes_per_ha_to_g_m2
        myDF$crootp <- myDF$crootp * tonnes_per_ha_to_g_m2
        myDF$pstore <- myDF$pstore * tonnes_per_ha_to_g_m2
        
        myDF$soilp <- myDF$soilp * tonnes_per_ha_to_g_m2
        myDF$inorgp <- myDF$inorgp * tonnes_per_ha_to_g_m2
        myDF$inorgavlp <- myDF$inorgavlp * tonnes_per_ha_to_g_m2
        myDF$inorglabp <- myDF$inorglabp * tonnes_per_ha_to_g_m2
        myDF$inorgsorbp <- myDF$inorgsorbp * tonnes_per_ha_to_g_m2
        myDF$inorgssorbp <- myDF$inorgssorbp * tonnes_per_ha_to_g_m2
        myDF$inorgoccp <- myDF$inorgoccp * tonnes_per_ha_to_g_m2
        myDF$inorgparp <- myDF$inorgparp * tonnes_per_ha_to_g_m2
        myDF$fertilizerp <- myDF$fertilizerp * tonnes_per_ha_to_g_m2
        
        myDF$litterpag <- myDF$litterpag * tonnes_per_ha_to_g_m2
        myDF$litterpbg <- myDF$litterpbg * tonnes_per_ha_to_g_m2
        
        myDF$activesoilp <- myDF$activesoilp * tonnes_per_ha_to_g_m2
        myDF$slowsoilp <- myDF$slowsoilp * tonnes_per_ha_to_g_m2
        myDF$passivesoilp <- myDF$passivesoilp * tonnes_per_ha_to_g_m2
        
        myDF$deadleafp <- myDF$deadleafp * tonnes_per_ha_to_g_m2
        myDF$deadbranchp <- myDF$deadbranchp * tonnes_per_ha_to_g_m2
        myDF$deadstemp <- myDF$deadstemp * tonnes_per_ha_to_g_m2
        myDF$deadrootp <- myDF$deadrootp * tonnes_per_ha_to_g_m2
        myDF$deadcrootp <- myDF$deadcrootp * tonnes_per_ha_to_g_m2
        
        myDF$ppleaf <- myDF$ppleaf * tonnes_per_ha_to_g_m2
        myDF$ppbranch <- myDF$ppbranch * tonnes_per_ha_to_g_m2
        myDF$ppstemimm <- myDF$ppstemimm * tonnes_per_ha_to_g_m2
        myDF$ppstemmob <- myDF$ppstemmob * tonnes_per_ha_to_g_m2
        myDF$pproot <- myDF$pproot * tonnes_per_ha_to_g_m2
        myDF$ppcroot <- myDF$ppcroot * tonnes_per_ha_to_g_m2
        
        myDF$puptake <- myDF$puptake * tonnes_per_ha_to_g_m2
        myDF$pgross <- myDF$pgross * tonnes_per_ha_to_g_m2
        myDF$pmineralisation <- myDF$pmineralisation * tonnes_per_ha_to_g_m2
        myDF$ploss <- myDF$ploss * tonnes_per_ha_to_g_m2
        myDF$p_slow_biochemical <- myDF$p_slow_biochemical * tonnes_per_ha_to_g_m2
        myDF$p_par_to_lab <- myDF$p_par_to_lab * tonnes_per_ha_to_g_m2
        
        myDF$leafretransp <- myDF$leafretransp * tonnes_per_ha_to_g_m2
        myDF$rootretransp <- myDF$rootretransp * tonnes_per_ha_to_g_m2
        myDF$crootretransp <- myDF$crootretransp * tonnes_per_ha_to_g_m2
        myDF$stemretransp <- myDF$stemretransp * tonnes_per_ha_to_g_m2
        myDF$bramchretransp <- myDF$branchretransp * tonnes_per_ha_to_g_m2
        
        # water pools and fluxes
        myDF$et <- myDF$et # mm of water' are same value as kg/m2
        myDF$transpiration <- myDF$transpiration # mm of water' are same value as kg/m2
        myDF$soil_evap <- myDF$soil_evap # mm of water' are same value as kg/m2
        myDF$canopy_evap <- myDF$canopy_evap # mm of water' are same value as kg/m2
        myDF$runoff <- myDF$runoff # mm of water' are same value as kg/m2
        
        
        ### end converting unit
        ############################################
        # empty
        # empty
        # empty
        # empty
        # empty
        ############################################
        ### start changing variable names 
        
        ## met forcing
        names(myDF)[names(myDF) == "year"] <- "YEAR"
        names(myDF)[names(myDF) == "doy"] <- "DOY"
        
        names(myDF)[names(myDF) == "CO2"] <- "CO2"
        names(myDF)[names(myDF) == "rain"] <- "PREC"
        names(myDF)[names(myDF) == "par"] <- "PAR"
        names(myDF)[names(myDF) == "tair"] <- "TAIR"
        names(myDF)[names(myDF) == "tsoil"] <- "TSOIL"
        names(myDF)[names(myDF) == "vpd"] <- "VPD"
        myDF$SW <- myDF$pawater_root
        names(myDF)[names(myDF) == "pawater_root"] <- "SWPA"
        names(myDF)[names(myDF) == "ndep"] <- "NDEP"
        names(myDF)[names(myDF) == "nfix"] <- "NFIX"
        names(myDF)[names(myDF) == "pdep"] <- "PDEP"
        names(myDF)[names(myDF) == "fertilizerp"] <- "PFERT"
        
        
        myDF$LE <- NA # replace 
        myDF$SH <- NA # replace 
        names(myDF)[names(myDF) == "apar"] <- "APARd"
        
        
        
        ## carbon variables
        names(myDF)[names(myDF) == "nep"] <- "NEP"
        names(myDF)[names(myDF) == "gpp"] <- "GPP"
        names(myDF)[names(myDF) == "npp"] <- "NPP"
        names(myDF)[names(myDF) == "auto_resp"] <- "RAU"
        names(myDF)[names(myDF) == "hetero_resp"] <- "RHET"
        
        names(myDF)[names(myDF) == "root_exc"] <- "CEX"
        
        myDF$RECO <- myDF$RAU + myDF$RHET
        myDF$CVOC <- 0.0
        myDF$RL <- 0.0
        myDF$RW <- 0.0
        myDF$RCR <- 0.0
        myDF$RFR <- 0.0
        myDF$RGR <- 0.0
        
        names(myDF)[names(myDF) == "shoot"] <- "CL"
        names(myDF)[names(myDF) == "root"] <- "CFR"
        names(myDF)[names(myDF) == "croot"] <- "CCR"
        
        myDF$CW <- myDF$branch + myDF$stem
        names(myDF)[names(myDF) == "cstore"] <- "CSTOR"
        
        names(myDF)[names(myDF) == "litterc"] <- "CFLIT"
        names(myDF)[names(myDF) == "littercag"] <- "CFLITA"
        names(myDF)[names(myDF) == "littercbg"] <- "CFLITB"
        
        myDF$CCLITB <- 0.0
        
        names(myDF)[names(myDF) == "soilc"] <- "CSOIL"
        
        names(myDF)[names(myDF) == "cpleaf"] <- "CGL"
        names(myDF)[names(myDF) == "cproot"] <- "CGFR"
        names(myDF)[names(myDF) == "cpcroot"] <- "CGCR"
        
        myDF$CGW <- myDF$cpbranch + myDF$cpstem
        myDF$CREPR <- 0.0
        
        names(myDF)[names(myDF) == "deadleaves"] <- "CLITIN"
        names(myDF)[names(myDF) == "deadcroots"] <- "CCRLIN"
        names(myDF)[names(myDF) == "deadroots"] <- "CFRLIN"
        
        myDF$CWLIN <- myDF$deadbranch + myDF$deadstems
        
        names(myDF)[names(myDF) == "lai"] <- "LAI"
        myDF$LMA <- myDF$CL / myDF$LAI
        
        
        ## water variables
        names(myDF)[names(myDF) == "et"] <- "ET"
        names(myDF)[names(myDF) == "transpiration"] <- "TRANS"
        names(myDF)[names(myDF) == "soil_evap"] <- "ES"
        names(myDF)[names(myDF) == "canopy_evap"] <- "EC"
        names(myDF)[names(myDF) == "runoff"] <- "RO"
        myDF$DRAIN <- 0.0
        
        names(myDF)[names(myDF) == "gs_mol_m2_sec"] <- "GCd"
        names(myDF)[names(myDF) == "ga_mol_m2_sec"] <- "GAd"
        
        myDF$GBd <- NA  # replace 
        
        names(myDF)[names(myDF) == "wtfac_root"] <- "Betad"
        
        
        
        
        ## nitrogen variables
        names(myDF)[names(myDF) == "shootn"] <- "NL"
        names(myDF)[names(myDF) == "rootn"] <- "NFR"
        names(myDF)[names(myDF) == "crootn"] <- "NCR"
        
        myDF$NW <- myDF$branchn + myDF$stemn
        names(myDF)[names(myDF) == "nstore"] <- "NSTOR"
        
        names(myDF)[names(myDF) == "litternag"] <- "NFLITA"
        names(myDF)[names(myDF) == "litternbg"] <- "NFLITB"
        myDF$NFLIT <- myDF$NFLITA + myDF$NFLITB
        
        myDF$NCLITB <- 0.0
        
        names(myDF)[names(myDF) == "soiln"] <- "NSOIL"
        
        names(myDF)[names(myDF) == "npleaf"] <- "NGL"
        names(myDF)[names(myDF) == "nproot"] <- "NGFR"
        names(myDF)[names(myDF) == "npcroot"] <- "NGCR"
        
        myDF$NGW <- myDF$npbranch + myDF$npstemimm + myDF$npstemmob  
        
        names(myDF)[names(myDF) == "deadleafn"] <- "NLITIN"
        names(myDF)[names(myDF) == "deadcrootn"] <- "NCRLIN"
        names(myDF)[names(myDF) == "deadrootn"] <- "NFRLIN"
        
        myDF$NWLIN <- myDF$deadbranchn + myDF$deadstemn
        
        myDF$NCON <- NA  # replace 
        
        names(myDF)[names(myDF) == "inorgn"] <- "NPMIN"
        myDF$NPORG <- myDF$activesoiln + myDF$slowsoiln + myDF$passivesoiln
        
        names(myDF)[names(myDF) == "nuptake"] <- "NUP"
        names(myDF)[names(myDF) == "ngross"] <- "NGMIN"
        names(myDF)[names(myDF) == "nmineralisation"] <- "NMIN"
        myDF$NVOL <- 0.0
        names(myDF)[names(myDF) == "nloss"] <- "NLEACH"
        names(myDF)[names(myDF) == "leafretransn"] <- "NLRETR"
        names(myDF)[names(myDF) == "rootretransn"] <- "NFRRETR"
        names(myDF)[names(myDF) == "crootretransn"] <- "NCRRETR"
        
        myDF$NWRETR <- myDF$branchretransn + myDF$stemretransn
        
        
        ## phosphorus variables
        names(myDF)[names(myDF) == "shootp"] <- "PL"
        names(myDF)[names(myDF) == "rootp"] <- "PFR"
        names(myDF)[names(myDF) == "crootp"] <- "PCR"
        
        myDF$PW <- myDF$branchp + myDF$stemp
        names(myDF)[names(myDF) == "pstore"] <- "PSTOR"
        
        names(myDF)[names(myDF) == "litterpag"] <- "PFLITA"
        names(myDF)[names(myDF) == "litterpbg"] <- "PFLITB"
        myDF$PFLIT <- myDF$PFLITA + myDF$PFLITB
        
        myDF$PCLITB <- 0.0
        
        names(myDF)[names(myDF) == "soilp"] <- "PSOIL"
        
        
        names(myDF)[names(myDF) == "ppleaf"] <- "PGL"
        names(myDF)[names(myDF) == "pproot"] <- "PGFR"
        names(myDF)[names(myDF) == "ppcroot"] <- "PGCR"
        
        myDF$PGW <- myDF$ppbranch + myDF$ppstemimm + myDF$ppstemmob  
        
        names(myDF)[names(myDF) == "deadleafp"] <- "PLITIN"
        names(myDF)[names(myDF) == "deadcrootp"] <- "PCRLIN"
        names(myDF)[names(myDF) == "deadrootp"] <- "PFRLIN"
        
        myDF$PWLIN <- myDF$deadbranchp + myDF$deadstemp
        
        names(myDF)[names(myDF) == "inorgp"] <- "PPMIN"
        myDF$PPORG <- myDF$activesoilp + myDF$slowsoilp + myDF$passivesoilp
        
        names(myDF)[names(myDF) == "puptake"] <- "PUP"
        names(myDF)[names(myDF) == "pgross"] <- "PGMIN"
        names(myDF)[names(myDF) == "pmineralisation"] <- "PMIN"
        names(myDF)[names(myDF) == "p_slow_biochemical"] <- "PBIOCHMIN"
        
        names(myDF)[names(myDF) == "ploss"] <- "PLEACH"
        names(myDF)[names(myDF) == "leafretransp"] <- "PLRETR"
        names(myDF)[names(myDF) == "rootretransp"] <- "PFRRETR"
        names(myDF)[names(myDF) == "crootretransp"] <- "PCRRETR"
        
        myDF$PWRETR <- myDF$branchretransp + myDF$stemretransp
        
        names(myDF)[names(myDF) == "inorglabp"] <- "PLAB"
        myDF$PSEC <- myDF$inorgsorbp + myDF$inorgssorbp
        names(myDF)[names(myDF) == "inorgoccp"] <- "POCC"
        names(myDF)[names(myDF) == "inorgparp"] <- "PPAR"
        
        # P weathering rate
        names(myDF)[names(myDF) == "p_par_to_lab"] <- "PWEA"
        
        
        ### end changing variable names
        ############################################
        # empty
        # empty
        # empty
        # empty
        # empty
        ############################################
        ### start preparing output
        
        outDF <- myDF[,c("YEAR","DOY","CO2","PREC","PAR","TAIR","TSOIL","VPD","SW",
                         "SWPA","NDEP","NEP","GPP","NPP","CEX","CVOC","RECO","RAU",
                         "RL","RW","RCR","RFR","RGR","RHET","ET","TRANS","ES","EC",
                         "RO","DRAIN","LE","SH","CL","CW","CCR","CFR","CSTOR","CFLIT",
                         "CFLITA","CFLITB","CCLITB","CSOIL","CGL","CGW","CGCR","CGFR",
                         "CREPR","CLITIN","CCRLIN","CFRLIN","CWLIN","LAI","LMA","NCON",
                         "NL","NW","NCR","NFR","NSTOR","NFLIT","NFLITA","NFLITB","NCLITB",
                         "NSOIL","NPMIN","NPORG","NFIX","NGL","NGW","NGCR","NGFR","NLITIN",
                         "NCRLIN","NFRLIN","NWLIN","NUP","NGMIN","NMIN","NVOL","NLEACH",
                         "NLRETR","NWRETR","NCRRETR","NFRRETR","APARd","GCd","GAd","GBd",
                         "Betad","PL","PW","PCR","PFR","PSTOR","PFLIT","PFLITA","PFLITB",
                         "PCLITB","PSOIL","PLAB","PSEC","POCC","PPAR","PPMIN","PPORG",
                         "PLITIN","PCRLIN","PFRLIN","PWLIN","PUP","PGMIN","PMIN",
                         "PBIOCHMIN","PLEACH","PGL","PGW","PGCR","PGFR","PLRETR","PWRETR",
                         "PCRRETR","PFRRETR","PWEA","PDEP","PFERT")]
        
        write.csv(outDF, paste0(out.path, "/GDAYP/", sceDF$out.names[i], ".csv"),
                  row.names=F)
    }
    
    
    
    ### end preparing output
    ############################################
    
    
    ### End
    
}