translate_ORCHD_simulation_into_EucFACE_MIP_format <- function(mod.abb) {
    
    #######################################################################################################
    ### This is the script to translate GDAY output into EucFACE Multi-model intercomparison project
    ### requested output format with request output variables
    
    ### met.path is the path where met forcing data is stored
    ### sim.path is the path where model simulation output is stored
    ### out.path is the path where translated output is stored
    
    #######################################################################################################
    
    source.dir <- paste0("simulation_output/", mod.abb, "/")
    
    
    ### prepare a dataframe to assign correct file names     
    file.names <- list.files(source.dir)
    n <- length(file.names)
    
    
    ### prepare a bunch of P fertilization scenarios
    nop <- c(paste0("EUC_", mod.abb, "_OBS_FIX_AMB_NOP_D.csv"),
             paste0("EUC_", mod.abb, "_OBS_VAR_AMB_NOP_D.csv"),
             paste0("EUC_", mod.abb, "_OBS_FIX_ELE_NOP_D.csv"),
             paste0("EUC_", mod.abb, "_OBS_VAR_ELE_NOP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_FIX_AMB_NOP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_VAR_AMB_NOP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_FIX_ELE_NOP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_VAR_ELE_NOP_D.csv"))
    
    
    mdp <- c(paste0("EUC_", mod.abb, "_PRD_FIX_AMB_MDP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_VAR_AMB_MDP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_FIX_ELE_MDP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_VAR_ELE_MDP_D.csv"))
    
    hip <- c(paste0("EUC_", mod.abb, "_PRD_FIX_AMB_HIP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_VAR_AMB_HIP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_FIX_ELE_HIP_D.csv"),
             paste0("EUC_", mod.abb, "_PRD_VAR_ELE_HIP_D.csv"))
    
    mdp.daily.value <- 0.004109589
    hip.daily.value <- 0.008219178
    
    ### loop through all data
    for (i in 1:n) {
        ### read in corresponding simulation file
        myDF <- read.csv(paste0(source.dir, file.names[i]))
        
        ### ignore missing variables
        myDF[myDF<=-9999.] <- NA
        
        myDF$X <- NULL
        
        ### add PFERT variable, depending on runs
        if (file.names[i]%in%nop) {
            
            myDF$PFERT <- 0.0
            
        } else if (file.names[i]%in%mdp) {
            
            myDF$PFERT <- 0.0
            myDF$PFERT[myDF$YEAR<=2022&myDF$YEAR>=2020] <- mdp.daily.value
            
        } else if (file.names[i]%in%hip) {
            
            myDF$PFERT <- 0.0
            myDF$PFERT[myDF$YEAR<=2022&myDF$YEAR>=2020] <- hip.daily.value
            
        } else {
            print("no P fertilization scenarios")
            
            myDF$PFERT <- 0.0
        }
        
        ############################################
        ### revise PMIN variable
        #myDF$PMIN <- with(myDF, PMIN - PBIOCHMIN)
        
        
        
        ### end changing variable names
        ############################################
        # empty
        # empty
        # empty
        # empty
        # empty
        ############################################
        ### start preparing output
        
        
        write.csv(myDF, paste0(source.dir, file.names[i]),
                  row.names=F)
    }
    
    
    
    ### end preparing output
    ############################################
    
    
    ### End
    
}