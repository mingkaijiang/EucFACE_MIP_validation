compile_all_dataset_across_models <- function () {
    
    
    
    ##################################################################
    #### Set up basics
    
    ### get the P fertilization treatment levels
    p.fert.levels <- c("NOP", "MDP", "HIP")
    
    ### get CO2 treatment levels
    co2.levels <- c("AMB", "ELE")
    
    ### variable and fixed climate
    clim.levels <- c("VAR", "FIX")
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/processed_simulation")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ### read obs data
    obsDF1 <- readRDS(paste0(out.dir, "/MIP_OBS_FIX_AMB_daily.rds"))
    obsDF2 <- readRDS(paste0(out.dir, "/MIP_OBS_VAR_AMB_daily.rds"))
    obsDF3 <- readRDS(paste0(out.dir, "/MIP_OBS_FIX_ELE_daily.rds"))
    obsDF4 <- readRDS(paste0(out.dir, "/MIP_OBS_VAR_ELE_daily.rds"))
    
    
    ### get var list
    var.list <- colnames(obsDF1)
    
    ## get model list
    mod.list1 <- unique(obsDF1$ModName)
    
    ##################################################################
    #### loop through models for observed period, ambient CO2 treatment, variable climate
    #### to create rds files 
    
    for (i in clim.levels) {
        
        ### prepare obs DF
        if (i == "FIX") {
            obsDF.amb <- obsDF1
            obsDF.ele <- obsDF3
        } else {
            obsDF.amb <- obsDF2
            obsDF.ele <- obsDF4
        }
        
        
        for (j in p.fert.levels) {
            
            ### read input
            ambDF <- readRDS(paste0(out.dir, 
                                    "/MIP_PRD_", i, "_", j, "_AMB_daily.rds"))
            eleDF <- readRDS(paste0(out.dir, 
                                    "/MIP_PRD_", i, "_", j, "_ELE_daily.rds"))
            
            ### update order of variables
            ambDF <- ambDF[,var.list]
            eleDF <- eleDF[,var.list]
            
            
            ### get model list
            mod.list2 <- unique(ambDF$ModName)
            mod.list <- mod.list1[table(c(mod.list1, mod.list2))==2]
            
            ### add historic data
            outDF1 <- rbind(obsDF.amb, ambDF)
            outDF2 <- rbind(obsDF.ele, eleDF)
            
            ### remove incomplete model results
            outDF1 <- outDF1[outDF1$ModName%in%mod.list,]
            outDF2 <- outDF2[outDF2$ModName%in%mod.list,]
            
        
            ### summarize all fluxes first to obain annual rate
            fluxDF1 <- summaryBy(PREC+NDEP+NEP+GPP+NPP+CEX+CVOC+RECO+
                                     RAU+RL+RW+RCR+RFR+RGR+RHET+ET+
                                     TRANS+ES+EC+RO+DRAIN+CGL+CGW+
                                     CGCR+CGFR+CREPR+CLITIN+CCRLIN+
                                     CFRLIN+CWLIN+NFIX+NGL+NGW+NGCR+NGFR+
                                     NLITIN+NCRLIN+NFRLIN+NWLIN+NUP+
                                     NGMIN+NMIN+NVOL+NLEACH+NLRETR+NWRETR+
                                     NCRRETR+NFRRETR+PLITIN+PCRLIN+PFRLIN+
                                     PWLIN+PUP+PGMIN+PMIN+PBIOCHMIN+PLEACH+
                                     PGL+PGW+PGCR+PGFR+PLRETR+PWRETR+PCRRETR+
                                     PFRRETR+PWEA+PDEP+PFERT~YEAR+ModName, 
                                 data=outDF1, FUN=sum, keep.names=T, na.rm=T)
            
            fluxDF2 <- summaryBy(PREC+NDEP+NEP+GPP+NPP+CEX+CVOC+RECO+
                                     RAU+RL+RW+RCR+RFR+RGR+RHET+ET+
                                     TRANS+ES+EC+RO+DRAIN+CGL+CGW+
                                     CGCR+CGFR+CREPR+CLITIN+CCRLIN+
                                     CFRLIN+CWLIN+NFIX+NGL+NGW+NGCR+NGFR+
                                     NLITIN+NCRLIN+NFRLIN+NWLIN+NUP+
                                     NGMIN+NMIN+NVOL+NLEACH+NLRETR+NWRETR+
                                     NCRRETR+NFRRETR+PLITIN+PCRLIN+PFRLIN+
                                     PWLIN+PUP+PGMIN+PMIN+PBIOCHMIN+PLEACH+
                                     PGL+PGW+PGCR+PGFR+PLRETR+PWRETR+PCRRETR+
                                     PFRRETR+PWEA+PDEP+PFERT~YEAR+ModName, 
                                 data=outDF2, FUN=sum, keep.names=T, na.rm=T)
            
            
            ### subset first day within a year of all pools
            poolDF1 <- outDF1[,c("ModName", "YEAR", "DOY", 
                                "SW",
                                "CL","LAI","CW","CFR","CCR",
                                "NL","NW","NFR","NCR",
                                "PL","PW","PFR","PCR",
                                "CSTOR","NSTOR","PSTOR",
                                "CSOIL","NSOIL","PSOIL",
                                "NPMIN","PPMIN",
                                "PLAB","PSEC","POCC","PPAR",
                                "CFLIT","CFLITA","CFLITB",
                                "NFLITA","NFLITB",
                                "PFLITA","PFLITB",
                                "CCLITB","NCLITB","PCLITB",
                                "NFLIT","PFLIT", 
                                "NPORG", "PPORG")]
            
            
            poolDF2 <- outDF2[,c("ModName", "YEAR", "DOY", 
                                "SW",
                                "CL","LAI","CW","CFR","CCR",
                                "NL","NW","NFR","NCR",
                                "PL","PW","PFR","PCR",
                                "CSTOR","NSTOR","PSTOR",
                                "CSOIL","NSOIL","PSOIL",
                                "NPMIN","PPMIN",
                                "PLAB","PSEC","POCC","PPAR",
                                "CFLIT","CFLITA","CFLITB",
                                "NFLITA","NFLITB",
                                "PFLITA","PFLITB",
                                "CCLITB","NCLITB","PCLITB",
                                "NFLIT","PFLIT", 
                                "NPORG", "PPORG")]
            
            
            poolDF1 <- subset(poolDF1, DOY==1)
            poolDF2 <- subset(poolDF2, DOY==1)
            
            poolDF1$DOY <- NULL
            poolDF2$DOY <- NULL
            
            ### calculate change in pools for mass balance
            deltaDF1 <- poolDF1[poolDF1$YEAR < 2069,]
            deltaDF2 <- poolDF2[poolDF2$YEAR < 2069,]
            
            l <- dim(deltaDF1)[2]
            
            for (m in c(2012:2068)) {
                deltaDF1[deltaDF1$YEAR==m,3:l] <- poolDF1[poolDF1$YEAR==(m+1),3:l]-poolDF1[poolDF1$YEAR==m,3:l]
            }
            
            for (m in c(2012:2068)) {
                deltaDF2[deltaDF2$YEAR==m,3:l] <- poolDF2[poolDF2$YEAR==(m+1),3:l]-poolDF2[poolDF2$YEAR==m,3:l]
            }
            
            
            ### add delta column name to deltaDF
            names(deltaDF1)[3:l] <- paste0("delta", names(deltaDF1[3:l]))
            names(deltaDF2)[3:l] <- paste0("delta", names(deltaDF2[3:l]))
            
            
            ### climate
            climDFx <- outDF1[,c("ModName", "YEAR", "DOY", "CO2", "PAR","TAIR","TSOIL","VPD")]
            climDFy <- outDF2[,c("ModName", "YEAR", "DOY", "CO2", "PAR","TAIR","TSOIL","VPD")]
            
            climDF1 <- summaryBy(CO2+PAR+TAIR+TSOIL+VPD~ModName+YEAR, FUN=mean,
                                 data=climDFx, na.rm=T, keep.names=T)
            
            climDF2 <- summaryBy(CO2+PAR+TAIR+TSOIL+VPD~ModName+YEAR, FUN=mean,
                                 data=climDFy, na.rm=T, keep.names=T)
            
            ### merge all dataframe together
            tmpDF1 <- merge(climDF1, fluxDF1, by=c("ModName", "YEAR"))
            annDF1 <- merge(tmpDF1, poolDF1, by=c("ModName", "YEAR"))
            annDF1 <- merge(annDF1, deltaDF1, by=c("ModName", "YEAR"), all.x=T)
            
            tmpDF2 <- merge(climDF2, fluxDF2, by=c("ModName", "YEAR"))
            annDF2 <- merge(tmpDF2, poolDF2, by=c("ModName", "YEAR"))
            annDF2 <- merge(annDF2, deltaDF2, by=c("ModName", "YEAR"), all.x=T)
            
            
            ### save output
            saveRDS(annDF1, paste0(out.dir, 
                                   "/MIP_ALL_", i, "_", j, "_AMB_annual.rds"))
            saveRDS(annDF2, paste0(out.dir, 
                                   "/MIP_ALL_", i, "_", j, "_ELE_annual.rds"))
            
            
        } # j
    } # i 
    
    
}
