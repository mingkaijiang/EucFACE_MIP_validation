translate_LPJGP_simulation_into_EucFACE_MIP_format <- function(source.dir) {
    
    #######################################################################################################
    ### This is the script to translate GDAY output into EucFACE Multi-model intercomparison project
    ### requested output format with request output variables
    
    #######################################################################################################
    
    ### prepare a dataframe to assign correct file names     
    euc.file.names <- c("EUC_LPJGP_OBS_VAR_AMB_NOP_D_euc_ter",
                        "EUC_LPJGP_OBS_VAR_ELE_NOP_D_euc_ter",
                        "EUC_LPJGP_OBS_FIX_AMB_NOP_D_euc_ter",
                        "EUC_LPJGP_OBS_FIX_ELE_NOP_D_euc_ter",
                        "EUC_LPJGP_PRD_VAR_AMB_NOP_D_euc_ter",
                        "EUC_LPJGP_PRD_VAR_AMB_MDP_D_euc_ter",
                        "EUC_LPJGP_PRD_VAR_AMB_HIP_D_euc_ter",
                        "EUC_LPJGP_PRD_VAR_ELE_NOP_D_euc_ter",
                        "EUC_LPJGP_PRD_VAR_ELE_MDP_D_euc_ter",
                        "EUC_LPJGP_PRD_VAR_ELE_HIP_D_euc_ter",
                        "EUC_LPJGP_PRD_FIX_AMB_NOP_D_euc_ter",
                        "EUC_LPJGP_PRD_FIX_AMB_MDP_D_euc_ter",
                        "EUC_LPJGP_PRD_FIX_AMB_HIP_D_euc_ter",
                        "EUC_LPJGP_PRD_FIX_ELE_NOP_D_euc_ter",
                        "EUC_LPJGP_PRD_FIX_ELE_MDP_D_euc_ter",
                        "EUC_LPJGP_PRD_FIX_ELE_HIP_D_euc_ter")
    
    all.file.names <- c("EUC_LPJGP_OBS_VAR_AMB_NOP_D",
                        "EUC_LPJGP_OBS_VAR_ELE_NOP_D",
                        "EUC_LPJGP_OBS_FIX_AMB_NOP_D",
                        "EUC_LPJGP_OBS_FIX_ELE_NOP_D",
                        "EUC_LPJGP_PRD_VAR_AMB_NOP_D",
                        "EUC_LPJGP_PRD_VAR_AMB_MDP_D",
                        "EUC_LPJGP_PRD_VAR_AMB_HIP_D",
                        "EUC_LPJGP_PRD_VAR_ELE_NOP_D",
                        "EUC_LPJGP_PRD_VAR_ELE_MDP_D",
                        "EUC_LPJGP_PRD_VAR_ELE_HIP_D",
                        "EUC_LPJGP_PRD_FIX_AMB_NOP_D",
                        "EUC_LPJGP_PRD_FIX_AMB_MDP_D",
                        "EUC_LPJGP_PRD_FIX_AMB_HIP_D",
                        "EUC_LPJGP_PRD_FIX_ELE_NOP_D",
                        "EUC_LPJGP_PRD_FIX_ELE_MDP_D",
                        "EUC_LPJGP_PRD_FIX_ELE_HIP_D")
    
    
    euc.path <- paste0(source.dir, "euc_ter/")
    all.path <- paste0(source.dir, "all_pft/")
    
    
    ### loop through only eucalyptus tree dataset
    for (i in 1:16) {
        ### read in corresponding simulation file
        myDF <- read.csv(paste0(euc.path, euc.file.names[i], ".csv"))
        
        ### ignore missing variables
        myDF[myDF<=-9999.] <- NA
        
        ### revise a few LPJGP output variable names
        names(myDF)[names(myDF)=="Year"]<-"YEAR"
        names(myDF)[names(myDF)=="Day"]<-"DOY"
        names(myDF)[names(myDF)=="T"]<-"TRANS"
        names(myDF)[names(myDF)=="PWEAT"]<-"PWEA"
        
        
        ### ignore lat and long
        myDF$Lon <- NULL
        myDF$Lat <- NULL
        
        
        ### save new file
        write.csv(myDF, paste0(euc.path, all.file.names[i], ".csv"),
                  row.names=F)
        
        ### delete original file
        file.remove(paste0(euc.path, euc.file.names[i], ".csv"))
    }
    
    
    
    ### loop through all pft
    for (i in 1:16) {
        ### read in corresponding simulation file
        myDF <- read.csv(paste0(all.path, all.file.names[i], ".csv"))
        
        ### ignore missing variables
        myDF[myDF<=-9999.] <- NA
        
        ### revise a few LPJGP output variable names
        names(myDF)[names(myDF)=="Year"]<-"YEAR"
        names(myDF)[names(myDF)=="Day"]<-"DOY"
        names(myDF)[names(myDF)=="T"]<-"TRANS"
        names(myDF)[names(myDF)=="PWEAT"]<-"PWEA"
        
        
        ### ignore lat and long
        myDF$Lon <- NULL
        myDF$Lat <- NULL
        
        
        ### save new file
        write.csv(myDF, paste0(all.path, all.file.names[i], ".csv"),
                  row.names=F)
        
    }
    
    
    
    ### end preparing output
    ############################################
    
    
    ### End
    
}