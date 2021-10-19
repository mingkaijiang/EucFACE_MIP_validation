prepare_Medlyn_2016_input <- function() {
    
    ### purpose:
    ### read in daily model output and convert into annual
    ### to compare against the current simulation output
    
    ### model list
    mod.list.2016 <- c("CABL", "CLM4", "CLMP", "GDAY", 
                       "LPJW", "LPJX", "OCNX", "SDVM")
    
    p.mod.list.2016 <- c("CABL", "CLMP")
    
    n.mod.list.2016 <- c("CLM4", "GDAY", 
                         "LPJW", "LPJX", "OCNX", "SDVM")
    
    ### path where model output is stored
    in.dir <- paste0(getwd(), "/simulation_output/Medlyn2016/")
    
    ### CO2 treatment
    co2.levels <- c("AMB", "ELE")
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/processed_simulation/Medyn2016")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ### prepare variable of interest
    p.var.of.interest <- c("YEAR","DOY","CO2","PPT","PAR","AT","ST","VPD","SW",
                         "NDEP","NEP","GPP","NPP","CEX","CVOC","RECO","RAUTO",
                         "RLEAF","RWOOD","RROOT","RGROW","RHET","ET","T","ES","EC",
                         "RO","DRAIN","LE","SH","CL","CW","CCR","CFR","TNC",
                         "CSOIL","GL","GW","GCR","GR",
                         "GREPR","LAI",
                         "NCAN","NWOOD","NCR","NFR","NSTOR",
                         "NSOIL","NPOOLM","NPOOLO","NFIX","NGL","NGW","NGCR","NGR",
                         "NUP","NGMIN","NMIN","NVOL","NLEACH",
                         "NLRETRANS","NWRETRANS","NCRRETRANS","NFRRETRANS",
                         "PCAN","PWOOD","PCR","PFR","PSTOR",
                         "PSOIL","PLABILE","PSECOND","POCCLUD","PPOOLM","PPOOLO",
                         "PUP","PGMIN","PMIN",
                         "PLEACH","PGL","PGW","PGCR","PGR")
    
    n.var.of.interest <- c("YEAR","DOY","CO2","PPT","PAR","AT","ST","VPD","SW",
                           "NDEP","NEP","GPP","NPP","CEX","CVOC","RECO","RAUTO",
                           "RLEAF","RWOOD","RROOT","RGROW","RHET","ET","T","ES","EC",
                           "RO","DRAIN","LE","SH","CL","CW","CCR","CFR","TNC",
                           "CSOIL","GL","GW","GCR","GR",
                           "GREPR","LAI",
                           "NCAN","NWOOD","NCR","NFR","NSTOR",
                           "NSOIL","NPOOLM","NPOOLO","NFIX","NGL","NGW","NGCR","NGR",
                           "NUP","NGMIN","NMIN","NVOL","NLEACH",
                           "NLRETRANS","NWRETRANS","NCRRETRANS","NFRRETRANS")
    
    
    for (i in co2.levels) {
    
        ### prepare an empty DF
        outDF <- c()
        
        ### looping CNP models
        for (mod.abb in p.mod.list.2016) {
            
            ## read in individual models
            if (mod.abb=="CABL") {
                modDF <- read.csv(paste0(in.dir, mod.abb, "/D1", mod.abb, 
                                         "EUC", i, "AVG.csv"), skip=7)
            } else if (mod.abb=="CLMP") {
                modDF <- read.csv(paste0(in.dir, mod.abb, "/D1", mod.abb, 
                                         "EUC", i, "AVG.csv"))
            } 
            
            ## select consistent columns
            modDF <- modDF[,p.var.of.interest]
            
            ## add date to the dataset to help with the plotting
            for (m in 2012:2023) {
                
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
        
        
        
        
        ### looping CN models
        for (mod.abb in n.mod.list.2016) {
            
            ## read in individual models
            if (mod.abb=="CLM4") {
                modDF <- read.csv(paste0(in.dir, mod.abb, "/D1", mod.abb, 
                                         "EUC", i, "AVG.csv"))
            } else if (mod.abb=="GDAY") {
                modDF <- read.csv(paste0(in.dir, mod.abb, "/D1", mod.abb, 
                                         "EUC", i, "AVG.csv"), skip=3)
            } else if (mod.abb=="LPJW") {
                modDF <- read.csv(paste0(in.dir, mod.abb, "/D1", mod.abb, 
                                         "EUC", i, "AVG.csv"), skip=2)
            } else if (mod.abb=="LPJX") {
                modDF <- read.csv(paste0(in.dir, mod.abb, "/D1", mod.abb, 
                                         "EUC", i, "AVG.csv"), skip=2)
            } else if (mod.abb=="OCNX") {
                modDF <- read.csv(paste0(in.dir, mod.abb, "/D1", mod.abb, 
                                         "EUC", i, "AVG.csv"))
            } else if (mod.abb=="SDVM") {
                modDF <- read.csv(paste0(in.dir, mod.abb, "/D1", mod.abb, 
                                         "EUC", i, "AVG.csv"))
            } 
            
            ## select consistent columns
            modDF <- modDF[,n.var.of.interest]
            
            ## add date to the dataset to help with the plotting
            for (m in 2012:2023) {
                
                date.list <- as.Date((modDF$DOY[modDF$YEAR==m]-1), 
                                     origin = paste0(m, "-01-01"))
                
                modDF$Date[modDF$YEAR == m] <- as.character(date.list)
            }
            
            modDF$Date <- as.Date(modDF$Date)
            
            ## add model name
            modDF$ModName <- mod.abb
            
            ### merge all models
            outDF <- plyr::rbind.fill(outDF, modDF)
        }
        
        
        ### assign model ordering 
        outDF$ModVersion <- ifelse(outDF$ModName%in%p.mod.list.2016, "CNP", "CN")
        
        
        ### replace all -999 values
        outDF[outDF<=-999.9] <- NA
        
        ### revise column names to make it consistent with the current MIP
        names(outDF)[names(outDF)=="PPT"] <- "PREC"
        names(outDF)[names(outDF)=="AT"] <- "TAIR"
        names(outDF)[names(outDF)=="ST"] <- "TSOIL"
        names(outDF)[names(outDF)=="RAUTO"] <- "RAU"
        names(outDF)[names(outDF)=="RLEAF"] <- "RL"
        names(outDF)[names(outDF)=="RWOOD"] <- "RW"
        names(outDF)[names(outDF)=="RROOT"] <- "RFR"
        names(outDF)[names(outDF)=="RGROW"] <- "RGR"
        names(outDF)[names(outDF)=="T"] <- "TRANS"
        names(outDF)[names(outDF)=="TNC"] <- "CSTOR"
        names(outDF)[names(outDF)=="GL"] <- "CGL"
        names(outDF)[names(outDF)=="GW"] <- "CGW"
        names(outDF)[names(outDF)=="GCR"] <- "CGCR"
        names(outDF)[names(outDF)=="GR"] <- "CGFR"
        names(outDF)[names(outDF)=="GREPR"] <- "CREPR"
        names(outDF)[names(outDF)=="NCAN"] <- "NL"
        names(outDF)[names(outDF)=="NWOOD"] <- "NW"
        names(outDF)[names(outDF)=="NPOOLM"] <- "NPMIN"
        names(outDF)[names(outDF)=="NPOOLO"] <- "NPORG"
        names(outDF)[names(outDF)=="NGR"] <- "NGFR"
        names(outDF)[names(outDF)=="NLRETRANS"] <- "NLRETR"
        names(outDF)[names(outDF)=="NWRETRANS"] <- "NWRETR"
        names(outDF)[names(outDF)=="NCRRETRANS"] <- "NCRRETR"
        names(outDF)[names(outDF)=="NFRRETRANS"] <- "NFRRETR"
        names(outDF)[names(outDF)=="PCAN"] <- "PL"
        names(outDF)[names(outDF)=="PWOOD"] <- "PW"
        names(outDF)[names(outDF)=="PLABILE"] <- "PLAB"
        names(outDF)[names(outDF)=="PSECOND"] <- "PSEC"
        names(outDF)[names(outDF)=="POCCLUD"] <- "POCC"
        names(outDF)[names(outDF)=="PPOOLM"] <- "PPMIN"
        names(outDF)[names(outDF)=="PPOOLO"] <- "PPORG"
        names(outDF)[names(outDF)=="PGR"] <- "PGFR"

        
        ### save the rds
        saveRDS(outDF, paste0(out.dir, "/MIP_Medlyn_", i, "_daily.rds"))
        
    }
    
    ##################################################################
    #### loop through the datasets to generate dataframes on fluxes, 
    #### pools, delta pools,
    #### at annual timesteps
    
    
    ### read in amb and ele dataframes
    ambDF <- readRDS(paste0(out.dir, "/MIP_Medlyn_AMB_daily.rds"))
    eleDF <- readRDS(paste0(out.dir, "/MIP_Medlyn_ELE_daily.rds"))
    
    ### summarize all fluxes first to obain annual rate
    fluxDF1 <- summaryBy(PREC+NDEP+NEP+GPP+NPP+CEX+CVOC+RECO+
                             RAU+RL+RW+RFR+RGR+RHET+ET+
                             TRANS+ES+EC+RO+DRAIN+CGL+CGW+
                             CGCR+CGFR+CREPR+NFIX+NGL+NGW+NGCR+NGFR+
                             NUP+
                             NGMIN+NMIN+NVOL+NLEACH+NLRETR+NWRETR+
                             NCRRETR+NFRRETR+
                             PUP+PGMIN+PMIN+PLEACH+
                             PGL+PGW+PGCR+PGFR~YEAR+ModName+ModVersion, 
                         data=ambDF, FUN=sum, keep.names=T, na.rm=T)
    
    fluxDF2 <- summaryBy(PREC+NDEP+NEP+GPP+NPP+CEX+CVOC+RECO+
                             RAU+RL+RW+RFR+RGR+RHET+ET+
                             TRANS+ES+EC+RO+DRAIN+CGL+CGW+
                             CGCR+CGFR+CREPR+NFIX+NGL+NGW+NGCR+NGFR+
                             NUP+
                             NGMIN+NMIN+NVOL+NLEACH+NLRETR+NWRETR+
                             NCRRETR+NFRRETR+
                             PUP+PGMIN+PMIN+PLEACH+
                             PGL+PGW+PGCR+PGFR~YEAR+ModName+ModVersion, 
                         data=eleDF, FUN=sum, keep.names=T, na.rm=T)
    
    
    ### subset first day within a year of all pools
    poolDF1 <- ambDF[,c("ModVersion", "ModName", "YEAR", "DOY", 
                        "SW",
                        "CL","LAI","CW","CFR","CCR",
                        "NL","NW","NFR","NCR",
                        "PL","PW","PFR","PCR",
                        "CSTOR","NSTOR","PSTOR",
                        "CSOIL","NSOIL","PSOIL",
                        "NPMIN","PPMIN",
                        "PLAB","PSEC","POCC",
                        "NPORG", "PPORG")]
    
    
    poolDF2 <- eleDF[,c("ModVersion", "ModName", "YEAR", "DOY", 
                        "SW",
                        "CL","LAI","CW","CFR","CCR",
                        "NL","NW","NFR","NCR",
                        "PL","PW","PFR","PCR",
                        "CSTOR","NSTOR","PSTOR",
                        "CSOIL","NSOIL","PSOIL",
                        "NPMIN","PPMIN",
                        "PLAB","PSEC","POCC",
                        "NPORG", "PPORG")]
    
    
    poolDF1 <- subset(poolDF1, DOY==1)
    poolDF2 <- subset(poolDF2, DOY==1)
    
    poolDF1$DOY <- NULL
    poolDF2$DOY <- NULL
    
    ### calculate change in pools for mass balance
    deltaDF1 <- poolDF1[poolDF1$YEAR < 2023,]
    deltaDF2 <- poolDF2[poolDF2$YEAR < 2023,]
    
    l <- dim(deltaDF1)[2]
    
    for (j in c(2012:2022)) {
        deltaDF1[deltaDF1$YEAR==j,4:l] <- poolDF1[poolDF1$YEAR==(j+1),4:l]-poolDF1[poolDF1$YEAR==j,4:l]
    }
    
    for (j in c(2012:2022)) {
        deltaDF2[deltaDF2$YEAR==j,4:l] <- poolDF2[poolDF2$YEAR==(j+1),4:l]-poolDF2[poolDF2$YEAR==j,4:l]
    }
    
    
    ### add delta column name to deltaDF
    names(deltaDF1)[4:l] <- paste0("delta", names(deltaDF1[4:l]))
    names(deltaDF2)[4:l] <- paste0("delta", names(deltaDF2[4:l]))
    
    
    ### climate
    climDFx <- ambDF[,c("ModVersion", "ModName", "YEAR", "DOY", "CO2", "PAR","TAIR","TSOIL","VPD")]
    climDFy <- eleDF[,c("ModVersion", "ModName", "YEAR", "DOY", "CO2", "PAR","TAIR","TSOIL","VPD")]
    
    climDF1 <- summaryBy(CO2+PAR+TAIR+TSOIL+VPD~ModName+YEAR+ModVersion, FUN=mean,
                         data=climDFx, na.rm=T, keep.names=T)
    
    climDF2 <- summaryBy(CO2+PAR+TAIR+TSOIL+VPD~ModName+YEAR+ModVersion, FUN=mean,
                         data=climDFy, na.rm=T, keep.names=T)
    
    ### merge all dataframe together
    tmpDF1 <- merge(climDF1, fluxDF1, by=c("ModVersion", "ModName", "YEAR"))
    annDF1 <- merge(tmpDF1, poolDF1, by=c("ModVersion", "ModName", "YEAR"))
    annDF1 <- merge(annDF1, deltaDF1, by=c("ModVersion", "ModName", "YEAR"), all.x=T)
    
    tmpDF2 <- merge(climDF2, fluxDF2, by=c("ModVersion", "ModName", "YEAR"))
    annDF2 <- merge(tmpDF2, poolDF2, by=c("ModVersion", "ModName", "YEAR"))
    annDF2 <- merge(annDF2, deltaDF2, by=c("ModVersion", "ModName", "YEAR"), all.x=T)
    
    
    ### save output
    saveRDS(annDF1, paste0(out.dir, "/MIP_Medlyn_AMB_annual.rds"))
    saveRDS(annDF2, paste0(out.dir, "/MIP_Medlyn_ELE_annual.rds"))
    
    
    

    
}