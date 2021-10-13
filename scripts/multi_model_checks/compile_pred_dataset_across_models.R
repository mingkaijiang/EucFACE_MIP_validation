compile_pred_dataset_across_models <- function (p.mod.list, 
                                                n.mod.list) {
    
    
    
    ##################################################################
    #### Set up basics
    
    ### get the P fertilization treatment levels
    p.fert.levels <- c("NOP", "MDP", "HIP")
    
    ### get CO2 treatment levels
    co2.levels <- c("AMB", "ELE")
    
    ### variable and fixed climate
    clim.levels <- c("VAR", "FIX")
    
    ### setting out path to store the files
    for (k in clim.levels) {
        out.dir <- paste0(getwd(), "/pred_", k, "_output")
        
        ### create output folder
        if(!dir.exists(out.dir)) {
            dir.create(out.dir, showWarnings = FALSE)
        }
    }
    
    out.dir.var <- paste0(getwd(), "/pred_VAR_output")
    out.dir.fix <- paste0(getwd(), "/pred_FIX_output")
    
    obs.dir.var <- paste0(getwd(), "/obs_var_output")
    obs.dir.fix <- paste0(getwd(), "/obs_fix_output")
    
    
    ### prepare consistent column names for CNP models
    p.mod.names <- c("YEAR","DOY","CO2","PREC","PAR","TAIR","TSOIL","VPD","SW",
                     #"SWPA",
                     "NDEP","NEP","GPP","NPP","CEX","CVOC","RECO","RAU",
                     "RL","RW","RCR","RFR","RGR","RHET","ET","TRANS","ES","EC",
                     "RO","DRAIN","LE","SH","CL","CW","CCR","CFR","CSTOR","CFLIT",
                     "CFLITA","CFLITB","CCLITB","CSOIL","CGL","CGW","CGCR","CGFR",
                     "CREPR","CLITIN","CCRLIN","CFRLIN","CWLIN","LAI",
                     #"LMA",
                     "NCON","NL","NW","NCR","NFR","NSTOR","NFLIT","NFLITA","NFLITB","NCLITB",
                     "NSOIL","NPMIN","NPORG","NFIX","NGL","NGW","NGCR","NGFR","NLITIN",
                     "NCRLIN","NFRLIN","NWLIN","NUP","NGMIN","NMIN","NVOL","NLEACH",
                     "NLRETR","NWRETR","NCRRETR","NFRRETR","APARd","GCd","GAd","GBd",
                     "Betad","PL","PW","PCR","PFR","PSTOR","PFLIT","PFLITA","PFLITB",
                     "PCLITB","PSOIL","PLAB","PSEC","POCC","PPAR","PPMIN","PPORG",
                     "PLITIN","PCRLIN","PFRLIN","PWLIN","PUP","PGMIN","PMIN",
                     "PBIOCHMIN","PLEACH","PGL","PGW","PGCR","PGFR","PLRETR","PWRETR",
                     "PCRRETR","PFRRETR","PWEA","PDEP","PFERT")
    
    
    ### prepare consistent column names for CN models
    n.mod.names <- c("YEAR","DOY","CO2","PREC","PAR","TAIR","TSOIL","VPD","SW",
                     #"SWPA",
                     "NDEP","NEP","GPP","NPP","CEX","CVOC","RECO","RAU",
                     "RL","RW","RCR","RFR","RGR","RHET","ET","TRANS","ES","EC",
                     "RO","DRAIN","LE","SH","CL","CW","CCR","CFR","CSTOR","CFLIT",
                     "CFLITA","CFLITB","CCLITB","CSOIL","CGL","CGW","CGCR","CGFR",
                     "CREPR","CLITIN","CCRLIN","CFRLIN","CWLIN","LAI",
                     #"LMA",
                     "NCON","NL","NW","NCR","NFR","NSTOR","NFLIT","NFLITA","NFLITB","NCLITB",
                     "NSOIL","NPMIN","NPORG","NFIX","NGL","NGW","NGCR","NGFR","NLITIN",
                     "NCRLIN","NFRLIN","NWLIN","NUP","NGMIN","NMIN","NVOL","NLEACH",
                     "NLRETR","NWRETR","NCRRETR","NFRRETR","APARd","GCd","GAd","GBd",
                     "Betad")
    
    
    ##################################################################
    #### loop through models for observed period, ambient CO2 treatment, variable climate
    #### to create rds files 
    for (j in co2.levels) {
        for (i in p.fert.levels) {
            for (k in clim.levels) {
                
                ### prepare an empty DF
                outDF <- c()
                
                ### looping CNP models
                for (mod.abb in p.mod.list) {
                    
                    ## read in individual models
                    if (mod.abb=="CABLP") {
                        modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                                 "/forest/EUC_", mod.abb, "_PRD_", k, "_", j, "_", i, "_D.csv"))
                    } else if (mod.abb=="LPJGP") {
                        modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                                 "/new_soil/euc_ter/EUC_", mod.abb, "_PRD_", k, "_", j, "_", i, "_D.csv"))
                    } else if (mod.abb=="QUJSM") {
                        modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                                 "/EUC_", mod.abb, "_PRD_", k, "_", j, "_", i, "_D.csv"))
                    } else if (mod.abb=="ELMV1") {
                        modDF <- read.csv(paste0("simulation_output/", mod.abb, "/EUC_", 
                                                 mod.abb, "_PRD_", k, "_", j, "_", i, "_D.csv"))
                    } else {
                        modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                                 "/EUC_", mod.abb, "_PRD_", k, "_", j, "_", i, "_D.csv"))
                    }
                    
                    ## select consistent columns
                    modDF <- modDF[,p.mod.names]
                    
                    ## add date to the dataset to help with the plotting
                    for (m in 2020:2069) {
                        
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
                for (mod.abb in n.mod.list) {
                    
                    ## read in individual models
                    if (mod.abb=="GDAYN") {
                        modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                                 "/EUC_", mod.abb, "_PRD_", k, "_", j, "_NOP_D.csv"))
                    } else if (mod.abb=="LPJGN") {
                        modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                                 "/new_soil/euc_ter/EUC_", mod.abb, "_PRD_", k, "_", j, "_NOP_D.csv"))
                    } 
                    
                    ## select consistent columns
                    modDF <- modDF[,n.mod.names]
                    
                    ## add date to the dataset to help with the plotting
                    for (m in 2020:2069) {
                        
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
                outDF$ModName <- gsub("GDAYP", "A_GDAYP", outDF$ModName)
                outDF$ModName <- gsub("ELMV1", "B_ELMV1", outDF$ModName)
                outDF$ModName <- gsub("CABLP", "C_CABLP", outDF$ModName)
                outDF$ModName <- gsub("LPJGP", "D_LPJGP", outDF$ModName)
                outDF$ModName <- gsub("OCHDP", "E_OCHDP", outDF$ModName)
                outDF$ModName <- gsub("QUINC", "F_QUINC", outDF$ModName)
                
                outDF$ModName <- gsub("OCHDX", "G_OCHDX", outDF$ModName)
                outDF$ModName <- gsub("QUJSM", "H_QUJSM", outDF$ModName)
                
                outDF$ModName <- gsub("GDAYN", "I_GDAYN", outDF$ModName)
                outDF$ModName <- gsub("LPJGN", "J_LPJGN", outDF$ModName)
                
        
                ### save the rds
                out.dir <- ifelse(k=="FIX", out.dir.fix, out.dir.var)
                
                saveRDS(outDF, paste0(out.dir, 
                                      "/MIP_pred_", k, "_", i, "_", j, "_daily.rds"))
                
            } # k - clim.levels
        } # j - co2.levels
    } # i - p.fert.levels 
    
    
    
    
    
    
    
    
    ##################################################################
    #### loop through the datasets to generate dataframes on fluxes, pools, delta pools,
    #### at annual timesteps
    
    ### read in amb and ele dataframes
    for (i in clim.levels) {
        for (j in p.fert.levels) {
            
            out.dir <- ifelse(i=="FIX", out.dir.fix, out.dir.var)
            
            ambDF <- readRDS(paste0(out.dir, "/MIP_pred_", i, "_", j, "_amb_daily.rds"))
            eleDF <- readRDS(paste0(out.dir, "/MIP_pred_", i, "_", j, "_ele_daily.rds"))
            
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
                                "NPORG", "PPORG")]
            
            
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
                                "NPORG", "PPORG")]
            
            
            poolDF1 <- subset(poolDF1, DOY==1)
            poolDF2 <- subset(poolDF2, DOY==1)
            
            poolDF1$DOY <- NULL
            poolDF2$DOY <- NULL
            
            ### calculate change in pools for mass balance
            deltaDF1 <- poolDF1[poolDF1$YEAR < 2069,]
            deltaDF2 <- poolDF2[poolDF2$YEAR < 2069,]
            
            l <- dim(deltaDF1)[2]
            
            for (m in c(2020:2068)) {
                deltaDF1[deltaDF1$YEAR==m,3:l] <- poolDF1[poolDF1$YEAR==(m+1),3:l]-poolDF1[poolDF1$YEAR==m,3:l]
            }
            
            for (m in c(2020:2068)) {
                deltaDF2[deltaDF2$YEAR==m,3:l] <- poolDF2[poolDF2$YEAR==(m+1),3:l]-poolDF2[poolDF2$YEAR==m,3:l]
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
            saveRDS(annDF1, paste0(out.dir, 
                                   "/MIP_pred_", i, "_", j, "_amb_annual.rds"))
            saveRDS(annDF2, paste0(out.dir, 
                                   "/MIP_pred_", i, "_", j, "_ele_annual.rds"))
            
            
        }
    }
    
    
}
