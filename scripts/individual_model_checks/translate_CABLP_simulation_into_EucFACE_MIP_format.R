translate_CABLP_simulation_into_EucFACE_MIP_format <- function(source.dir) {
    
    #######################################################################################################
    ### This is the script to translate GDAY output into EucFACE Multi-model intercomparison project
    ### requested output format with request output variables
    
    ### met.path is the path where met forcing data is stored
    ### sim.path is the path where model simulation output is stored
    ### out.path is the path where translated output is stored
    
    #######################################################################################################
    
    ### prepare a dataframe to assign correct file names     
    file.names <- c("EUC_CABLP_OBS_VAR_AMB_NOP_D",
                    "EUC_CABLP_OBS_VAR_ELE_NOP_D",
                    "EUC_CABLP_OBS_FIX_AMB_NOP_D",
                    "EUC_CABLP_OBS_FIX_ELE_NOP_D",
                    "EUC_CABLP_PRD_VAR_AMB_NOP_D",
                    "EUC_CABLP_PRD_VAR_AMB_MDP_D",
                    "EUC_CABLP_PRD_VAR_AMB_HIP_D",
                    "EUC_CABLP_PRD_VAR_ELE_NOP_D",
                    "EUC_CABLP_PRD_VAR_ELE_MDP_D",
                    "EUC_CABLP_PRD_VAR_ELE_HIP_D",
                    "EUC_CABLP_PRD_FIX_AMB_NOP_D",
                    "EUC_CABLP_PRD_FIX_AMB_MDP_D",
                    "EUC_CABLP_PRD_FIX_AMB_HIP_D",
                    "EUC_CABLP_PRD_FIX_ELE_NOP_D",
                    "EUC_CABLP_PRD_FIX_ELE_MDP_D",
                    "EUC_CABLP_PRD_FIX_ELE_HIP_D")
    
    ### loop through all data
    for (i in 1:16) {
        ### read in corresponding simulation file
        myDF <- read.csv(paste0(source.dir, "/", file.names[i], ".csv"))
        
        ### precipitation data in the unit of mm/h, so need to convert it to unit of mm/d
        myDF$PREC <- myDF$PREC * 24.0
        
        ### ignore missing variables
        myDF[myDF<=-9999.] <- NA
        
        ### EC also need to convert
        myDF$EC <- myDF$EC * 24.0
        
        ### add missing variables and fill with NAs
        myDF$NWLIN <- NA
        myDF$PWLIN <- NA
        myDF$SWPA <- NA
        myDF$LMA <- NA
        myDF$PFERT <- NA
        
        
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
                         "PCRRETR","PFRRETR","PWEA","PDEP","PFERT", "CLABILE", "NIMM", "PIMM",
                         "NLITMIN", "PLITMIN", "NLITINTOT", "PLITINTOT")]
        
        write.csv(outDF, paste0(getwd(), source.dir, "/CABLP/forest/", file.names[i], ".csv"),
                  row.names=F)
    }
    
    
    
    ### end preparing output
    ############################################
    
    
    ### End
    
}