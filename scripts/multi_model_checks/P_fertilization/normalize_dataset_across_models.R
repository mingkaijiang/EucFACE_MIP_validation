normalize_dataset_across_models <- function (yr.to.normalize) {
    
    
    
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
    
    ##################################################################
    #### loop through the datasets to generate dataframes on fluxes, pools, delta pools,
    #### at annual timesteps
    
    ### read in amb and ele dataframes
    for (i in clim.levels) {
        for (j in p.fert.levels) {
            
            ### read input
            ambDF <- readRDS(paste0(out.dir, "/MIP_ALL_", i, "_", j, "_AMB_annual.rds"))
            eleDF <- readRDS(paste0(out.dir, "/MIP_ALL_", i, "_", j, "_ELE_annual.rds"))
            
            ### subset year as baseline of normalization
            baseDF1 <- subset(ambDF, YEAR==yr.to.normalize)
            baseDF2 <- subset(eleDF, YEAR==yr.to.normalize)
            
            ### get the model list
            mod.list <- unique(ambDF$ModName)
            
            ### prepare outDF
            outDF1 <- ambDF
            outDF2 <- eleDF
            
            ### get dimension
            l <- dim(outDF1)[2]
            yr <- unique(outDF1$YEAR)
            
            for (k in mod.list) {
                for (m in yr) {
                    outDF1[outDF1$ModName==k&outDF1$YEAR==m, 10:l] <- ambDF[ambDF$ModName==k&ambDF$YEAR==m,10:l]/baseDF1[baseDF1$ModName==k,10:l]
                    outDF2[outDF2$ModName==k&outDF2$YEAR==m, 10:l] <- eleDF[eleDF$ModName==k&eleDF$YEAR==m,10:l]/baseDF2[baseDF2$ModName==k,10:l]
                    
                }
            }
            
            ### save output
            saveRDS(outDF1, paste0(out.dir, 
                                   "/MIP_normalized_", yr.to.normalize,"_", i, "_", j, "_AMB_annual.rds"))
            saveRDS(outDF2, paste0(out.dir, 
                                   "/MIP_normalized_", yr.to.normalize,"_", i, "_", j, "_ELE_annual.rds"))
            
            
            ### now we calculate the CO2 effect for each year (ele / amb)
            outDF3 <- ambDF
            outDF3[,3:l] <- eleDF[,3:l]/ambDF[,3:l]
            
            ### save
            saveRDS(outDF3, paste0(out.dir, 
                                   "/MIP_normalized_ALL_", i, "_", j, "_co2_effect_annual.rds"))
            
            
        }
    }
    
    
}
