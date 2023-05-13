prepare_microbial_model_input <- function() {
    
    #### Purpose:
    #### read in ORCHIDEE and QUINCY model specific output separately
    
    ### prepare input directory
    indir <- paste0(getwd(), "/simulation_output/")
    
    ### outdirectory to store the processed simulation output
    outdir <- paste0(getwd(), "/output/MIP_output/processed_simulation/microbial_models")
    
    ### create output folder
    if(!dir.exists(outdir)) {
        dir.create(outdir, showWarnings = FALSE)
    }
    
    ### get CO2 treatment levels
    co2.levels <- c("AMB", "ELE")
    
    ### variable and fixed climate
    clim.levels <- c("VAR", "FIX")
    
    ### microbial model list
    m.mod.list <- c("OCHDP", "OCHDX", "QUINC", "QUJSM")
    
    
    ### prepare a complete variable list
    p.mod.names.general <- c("YEAR","DOY","CO2","PREC","PAR","TAIR","TSOIL","VPD","SW",
                             "NDEP","NEP","GPP","NPP","CEX","CVOC","RECO","RAU",
                             "RL","RW","RCR","RFR","RGR","RHET","ET","TRANS","ES","EC",
                             "RO","DRAIN","LE","SH","CL","CW","CCR","CFR","CSTOR","CFLIT",
                             "CFLITA","CFLITB","CCLITB","CSOIL","CGL","CGW","CGCR","CGFR",
                             "CREPR","CLITIN","CCRLIN","CFRLIN","CWLIN","LAI",
                             "NCON","NL","NW","NCR","NFR","NSTOR","NFLIT","NFLITA","NFLITB","NCLITB",
                             "NSOIL","NPMIN","NPORG","NFIX","NGL","NGW","NGCR","NGFR","NLITIN",
                             "NCRLIN","NFRLIN","NWLIN","NUP","NGMIN","NMIN","NVOL","NLEACH",
                             "NLRETR","NWRETR","NCRRETR","NFRRETR","APARd","GCd","GAd","GBd",
                             "Betad","PL","PW","PCR","PFR","PSTOR","PFLIT","PFLITA","PFLITB",
                             "PCLITB","PSOIL","PLAB","PSEC","POCC","PPAR","PPMIN","PPORG",
                             "PLITIN","PCRLIN","PFRLIN","PWLIN","PUP","PGMIN","PMIN",
                             "PBIOCHMIN","PLEACH","PGL","PGW","PGCR","PGFR","PLRETR","PWRETR",
                             "PCRRETR","PFRRETR","PWEA","PDEP") 
    
    
    p.mod.names.ochdx <- c("YEAR","DOY", "CMICR", "NMICR", "PMICR") # OCHDX-specific
    
    
    p.mod.names.qujsm <- c("YEAR","DOY", 
                           "CMIC10", "CMIC30", "CMIC60",
                           "CMOC", "CTMOC",
                           "NMIC10", "NMIC30", "NMIC60",
                           "NMOC", "NTMOC",
                           "PMIC10", "PMIC30", "PMIC60",
                           "PMOC", "PTMOC") # QUJSM-specific
    
    
    
    #library(gtools)
    #do.call(smartbind,l)
    
    
    for (i in co2.levels) {
        for (j in clim.levels) {
            
            ### prepare an empty DF
            outDF <- c()
            
            ### looping CNP models
            for (mod.abb in m.mod.list) {
                modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                         "/EUC_", mod.abb, "_OBS_", j, "_", i, "_NOP_D.csv"))
                
                ## select consistent columns
                modDF <- modDF[,p.mod.names.general]
                
                ## add date to the dataset to help with the plotting
                for (m in 2012:2019) {
                    
                    date.list <- as.Date((modDF$DOY[modDF$YEAR==m]-1), 
                                         origin = paste0(m, "-01-01"))
                    
                    modDF$Date[modDF$YEAR == m] <- as.character(date.list)
                }
                
                modDF$Date <- as.Date(modDF$Date)
                
                ## add model name
                modDF$ModName <- mod.abb
                
                ### merge all models
                outDF <- rbind(outDF, modDF)
            }
            
            
            ### add additional variables to outDF
            #outDF$NMICR <- NA
            #outDF$PMICR <- NA
            #
            #outDF$CMIC10 <- NA
            #outDF$CMIC30 <- NA
            #outDF$CMIC60 <- NA
            #outDF$NMIC10 <- NA
            #outDF$NMIC30 <- NA
            #outDF$NMIC60 <- NA
            #outDF$PMIC10 <- NA
            #outDF$PMIC30 <- NA
            #outDF$PMIC60 <- NA
            #outDF$CMOC <- NA
            #outDF$NMOC <- NA
            #outDF$PMOC <- NA
            #outDF$CTMOC <- NA
            #outDF$NTMOC <- NA
            #outDF$PTMOC <- NA
            
            
            ## read in individual models
            modDF1 <- read.csv(paste0("simulation_output/QUJSM/EUC_QUJSM_OBS_", 
                                      j, "_", i, "_NOP_D.csv"))
            
            modDF2 <- read.csv(paste0("simulation_output/OCHDX/EUC_OCHDX_OBS_", 
                                      j, "_", i, "_NOP_D.csv"))
                
            ### subset
            modDF1 <- modDF1[,p.mod.names.qujsm]
            modDF2 <- modDF2[,p.mod.names.ochdx]
            
            ### add model names
            modDF1$ModName <- "QUJSM"
            modDF2$ModName <- "OCHDX"
            
            ### merge
            outDF <- merge(outDF, modDF1, by=c("YEAR", "DOY", "ModName"),
                           all.x=T)
            outDF <- merge(outDF, modDF2, by=c("YEAR", "DOY", "ModName"),
                           all.x=T)   
            
            ### add PFERT column
            outDF$PFERT <- 0.0
            
            ### assign model ordering 
            outDF$ModName <- gsub("OCHDP", "E_OCHDP", outDF$ModName)
            outDF$ModName <- gsub("QUINC", "F_QUINC", outDF$ModName)
            
            outDF$ModName <- gsub("OCHDX", "G_OCHDX", outDF$ModName)
            outDF$ModName <- gsub("QUJSM", "H_QUJSM", outDF$ModName)
            
            
            ### save the rds
            saveRDS(outDF, paste0(outdir, "/MIP_OBS_", j, "_", i, "_daily.rds"))
            
            
        }
    }
    
    
    ##################################################################
    #### loop through the datasets to generate dataframes on fluxes, 
    #### pools, delta pools,
    #### at annual timesteps
    
    for (i in clim.levels) {
        
        ### read in amb and ele dataframes
        ambDF <- readRDS(paste0(outdir, "/MIP_OBS_", i, "_AMB_daily.rds"))
        eleDF <- readRDS(paste0(outdir, "/MIP_OBS_", i, "_ELE_daily.rds"))
        
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
                             data=ambDF, FUN=sum, keep.names=T, na.rm=T)
        
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
                             data=eleDF, FUN=sum, keep.names=T, na.rm=T)
        
        
        ### subset first day within a year of all pools
        poolDF1 <- ambDF[,c("ModName", "YEAR", "DOY", 
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
                            "NPORG", "PPORG", 
                            #"PTPMIN", "PTPORG",
                            "CMICR", "NMICR",  "PMICR",
                            "CMIC10", "CMIC30", "CMIC60",
                            "CMOC", "CTMOC",
                            "NMIC10", "NMIC30", "NMIC60",
                            "NMOC", "NTMOC",
                            "PMIC10", "PMIC30", "PMIC60",
                            "PMOC", "PTMOC")]
        
        
        poolDF2 <- eleDF[,c("ModName", "YEAR", "DOY", 
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
                            "NPORG", "PPORG", 
                            #"PTPMIN", "PTPORG",
                            "CMICR", "NMICR", "PMICR",
                            "CMIC10", "CMIC30", "CMIC60",
                            "CMOC", "CTMOC",
                            "NMIC10", "NMIC30", "NMIC60",
                            "NMOC", "NTMOC",
                            "PMIC10", "PMIC30", "PMIC60",
                            "PMOC", "PTMOC")]
        
        
        poolDF1 <- subset(poolDF1, DOY==1)
        poolDF2 <- subset(poolDF2, DOY==1)
        
        poolDF1$DOY <- NULL
        poolDF2$DOY <- NULL
        
        ### calculate change in pools for mass balance
        deltaDF1 <- poolDF1[poolDF1$YEAR < 2019,]
        deltaDF2 <- poolDF2[poolDF2$YEAR < 2019,]
        
        l <- dim(deltaDF1)[2]
        
        for (j in c(2012:2018)) {
            deltaDF1[deltaDF1$YEAR==j,3:l] <- poolDF1[poolDF1$YEAR==(j+1),3:l]-poolDF1[poolDF1$YEAR==j,3:l]
        }
        
        for (j in c(2012:2018)) {
            deltaDF2[deltaDF2$YEAR==j,3:l] <- poolDF2[poolDF2$YEAR==(j+1),3:l]-poolDF2[poolDF2$YEAR==j,3:l]
        }
        
        
        ### add delta column name to deltaDF
        names(deltaDF1)[3:l] <- paste0("delta", names(deltaDF1[3:l]))
        names(deltaDF2)[3:l] <- paste0("delta", names(deltaDF2[3:l]))
        
        
        ### climate
        climDFx <- ambDF[,c("ModName", "YEAR", "DOY", "CO2", "PAR","TAIR","TSOIL","VPD")]
        climDFy <- eleDF[,c("ModName", "YEAR", "DOY", "CO2", "PAR","TAIR","TSOIL","VPD")]
        
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
        saveRDS(annDF1, paste0(outdir, "/MIP_OBS_", i, "_AMB_annual.rds"))
        saveRDS(annDF2, paste0(outdir, "/MIP_OBS_", i, "_ELE_annual.rds"))
        
        
    }
    
    
    
}