translate_QUINC_simulation_into_EucFACE_MIP_format <- function(source.dir) {
    
    #######################################################################################################
    ### This is the script to translate GDAY output into EucFACE Multi-model intercomparison project
    ### requested output format with request output variables
    
    ### met.path is the path where met forcing data is stored
    ### sim.path is the path where model simulation output is stored
    ### out.path is the path where translated output is stored
    
    #######################################################################################################
    
    ### prepare a dataframe to assign correct file names     
    file.names <- c("EUC_QUINC_OBS_VAR_AMB_NOP_D",
                    "EUC_QUINC_OBS_VAR_ELE_NOP_D",
                    "EUC_QUINC_OBS_FIX_AMB_NOP_D",
                    "EUC_QUINC_OBS_FIX_ELE_NOP_D",
                    "EUC_QUINC_PRD_VAR_AMB_NOP_D",
                    "EUC_QUINC_PRD_VAR_AMB_MDP_D",
                    "EUC_QUINC_PRD_VAR_AMB_HIP_D",
                    "EUC_QUINC_PRD_VAR_ELE_NOP_D",
                    "EUC_QUINC_PRD_VAR_ELE_MDP_D",
                    "EUC_QUINC_PRD_VAR_ELE_HIP_D",
                    "EUC_QUINC_PRD_FIX_AMB_NOP_D",
                    "EUC_QUINC_PRD_FIX_AMB_MDP_D",
                    "EUC_QUINC_PRD_FIX_AMB_HIP_D",
                    "EUC_QUINC_PRD_FIX_ELE_NOP_D",
                    "EUC_QUINC_PRD_FIX_ELE_MDP_D",
                    "EUC_QUINC_PRD_FIX_ELE_HIP_D")
    
    ### loop through all data
    for (i in 1:16) {
        ### read in corresponding simulation file
        myDF <- read.csv(paste0(source.dir, file.names[i], ".csv"))
        
        ### ignore missing variables
        myDF[myDF<=-9999.] <- NA
        
        ### add missing variables and fill with NAs
        myDF$TAIR <- myDF$TAIR - 273.15
        myDF$TSOIL <- myDF$TSOIL - 273.15
        

        ### end changing variable names
        ############################################
        # empty
        # empty
        # empty
        # empty
        # empty
        ############################################
        ### start preparing output
        
        
        write.csv(myDF, paste0(source.dir, file.names[i], ".csv"),
                  row.names=F)
    }
    
    
    
    ### end preparing output
    ############################################
    
    
    ### End
    
}