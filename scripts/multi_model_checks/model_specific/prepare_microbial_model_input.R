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
    p.mod.names <- c("YEAR","DOY","CO2","PREC","PAR","TAIR","TSOIL","VPD","SW",
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
                     "PCRRETR","PFRRETR","PWEA","PDEP", "PFERT",
                     "NMICR", "PMICR", # OCHDX-specific
                     "PTPMIN", "PTPORG", "RMAIN", "RNTRANS", "CSTRLIN",  
                     "NGSTR", "NREPR", "NRECYC", "NSTRLIN", "NVEGLIN", "NSEED",  
                     "NFRUIT", "PGSTR", "PREPR", "PRECYC", "PSTRLIN", "PVEGLIN",
                     "PSEED", "PFRUIT") # QUJSM-specific
    
    
    
    library(gtools)
    do.call(smartbind,l)
    
    
    for (i in co2.levels) {
        for (j in clim.levels) {
            
            ### prepare an empty DF
            outDF <- c()
            
            ### looping CNP models
            for (mod.abb in m.mod.list) {
                
                ## read in individual models
                if (mod.abb=="QUJSM") {
                    modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                             "/EUC_", mod.abb, "_OBS_", j, "_", i, "_NOP_D.csv"))
                } else {
                    modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                             "/EUC_", mod.abb, "_OBS_", j, "_", i, "_NOP_D.csv"))
                }
                
                ## select consistent columns
                modDF <- modDF[,p.mod.names]
                
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
            
            
            ### looping vegetation dynamic models
            #for (mod.abb in d.mod.list) {
            #    
            #    ## read in individual models
            #    if (mod.abb=="CABLP") {
            #        modDF <- read.csv(paste0("simulation_output/", mod.abb, 
            #                                 "/tile_averaged/EUC_", mod.abb, "_OBS_VAR_AMB_NOP_D.csv"))
            #    } else if (mod.abb=="LPJGP") {
            #        modDF <- read.csv(paste0("simulation_output/", mod.abb, 
            #                                 "/new_soil/all_pft/EUC_", mod.abb, "_OBS_VAR_AMB_NOP_D.csv"))
            #    } 
            #    
            #    ## select consistent columns
            #    modDF <- modDF[,p.mod.names]
            #    
            #    ## add date to the dataset to help with the plotting
            #    for (i in 2012:2019) {
            #        
            #        date.list <- as.Date((modDF$DOY[modDF$YEAR==i]-1), 
            #                             origin = paste0(i, "-01-01"))
            #        
            #        modDF$Date[modDF$YEAR == i] <- as.character(date.list)
            #    }
            #    
            #    modDF$Date <- as.Date(modDF$Date)
            #    
            #    ## add model name
            #    modDF$ModName <- paste0(mod.abb, "-VD")
            #    
            #    ### merge all models
            #    outDF <- rbind(outDF, modDF)
            #}
            
            
            ### looping CN models
            for (mod.abb in n.mod.list) {
                
                ## read in individual models
                if (mod.abb=="GDAYN") {
                    modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                             "/EUC_", mod.abb, "_OBS_", j, "_", i, "_NOP_D.csv"))
                } else if (mod.abb=="LPJGN") {
                    modDF <- read.csv(paste0("simulation_output/", mod.abb, 
                                             "/new_soil/euc_ter/EUC_", mod.abb, "_OBS_", j, "_", i, "_NOP_D.csv"))
                } 
                
                ## select consistent columns
                modDF <- modDF[,n.mod.names]
                
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
                outDF <- plyr::rbind.fill(outDF, modDF)
            }
            
            ### add PFERT column
            outDF$PFERT <- 0.0
            
            ### assign model ordering 
            #outDF$ModName <- gsub("CABLP-VD", "CVD", outDF$ModName)
            #outDF$ModName <- gsub("LPJGP-VD", "LVD", outDF$ModName)
            
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
            #outDF$ModName <- gsub("CVD", "K_CABLP-VD", outDF$ModName)
            #outDF$ModName <- gsub("LVD", "L_LPJGP-VD", outDF$ModName)
            
            ### save the rds
            saveRDS(outDF, paste0(out.dir, "/MIP_OBS_", j, "_", i, "_daily.rds"))
            
            
        }
    }
    
    
    ##################################################################
    #### ORCHIDEE
    
    ### steps: 1. Read in amb and ele under two climate over historic period
    ###        2. Prepare all necessary grouping and labelling, 
    ###        3. Calculate annual rate, including delta pools
    
    

    ##################################################################
    #### QUINCY
    
    
    ##################################################################
    #### Merge
    
    
    
    return(outDF)
    
}