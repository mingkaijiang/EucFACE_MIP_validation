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
    
    ### set-up paths locally
    met.path <- "/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/met_data"
    sim.path <- "/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/GDAY-EucFACE/outputs"
    out.path <- "simulation_output"
    
    ### read in met file
    metDF <- read.csv(paste0(met.path, "/EUC_met_DRY_AMB_daily_2012_2019.csv"), skip=4)
    names(metDF)[1] <- "year"
    
    ### read in corresponding simulation file
    simDF <- read.csv(paste0(sim.path, "/EUC_simulated_DRY_AMB_2012_2019.csv"), skip=1)
    
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
    myDF$par <- (myDF$par_am + myDF$par_pm) * MJ_TO_MOL
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

    myDF$structsurfp <- myDF$structsurfp * tonnes_per_ha_to_g_m2
    myDF$structsoilp <- myDF$structsoilp * tonnes_per_ha_to_g_m2
    myDF$metabsurfp <- myDF$metabsurfp * tonnes_per_ha_to_g_m2
    myDF$metabsoilp <- myDF$metabsoilp * tonnes_per_ha_to_g_m2
    myDF$plittrelease <- myDF$plittrelease * tonnes_per_ha_to_g_m2
    
    myDF$leafretransp <- myDF$leafretransp * tonnes_per_ha_to_g_m2
    
    
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
    names(myDF)[names(myDF) == "pawater_root"] <- "SW"
    names(myDF)[names(myDF) == "pawater_root"] <- "SWPA"
    names(myDF)[names(myDF) == "ndep"] <- "NDEP"
    names(myDF)[names(myDF) == "nfix"] <- "NFIX"
    names(myDF)[names(myDF) == "pdep"] <- "PDEP"
    names(myDF)[names(myDF) == "pfert"] <- "PFERT"
    
    
    ## carbon variables
    names(myDF)[names(myDF) == "nep"] <- "NEP"
    names(myDF)[names(myDF) == "gpp"] <- "GPP"
    names(myDF)[names(myDF) == "npp"] <- "NPP"
    #names(myDF)[names(myDF) == "cex"] <- "CEX"
    #names(myDF)[names(myDF) == "cvoc"] <- "CVOC"
    
    
    
    
    ### end changing variable names
    ############################################
    # empty
    # empty
    # empty
    # empty
    # empty
    
    
    
    ### End
    
}