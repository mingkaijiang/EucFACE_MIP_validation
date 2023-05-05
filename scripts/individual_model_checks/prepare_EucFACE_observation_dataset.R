prepare_EucFACE_observation_dataset <- function (ignore.understorey) {
    ### Prepare EucFACE observation datasets.
    ### Take codes from EucFACE C and nutrient budget assessment;
    ### Con't use any unpublished data to compromise upcoming data-based publications.
    ### Given that we have ambient validation already, 
    ### this code should focus on the CO2 response ratio.
    ### Prepare annual C pools and fluxes, 
    ### and some N and P pools and fluxes (only published work).
    
    ############################## C budget #####################################
    #### Time-averaged (2013-16) C variables,
    #### taken directly from EucFACE C budget output
    #############################################################################
    
    if (ignore.understorey == T) {
        #### read in C budget data
        cDF <- read_and_process_EucFACE_C_budget_output_ignore_understorey()
        
        ### read in N and P budget data
        pDF <- read_and_process_EucFACE_P_budget_output_ignore_understorey()
    } else {
        #### read in C budget data
        cDF <- read_and_process_EucFACE_C_budget_output_not_ignore_understorey()
        
        ### read in N and P budget data
        pDF <- read_and_process_EucFACE_P_budget_output_not_ignore_understorey()
        
    }
    
    ### merge
    outDF <- cbind(cDF, pDF[,-c(1:2)])
    
    ### return data
    return(outDF)
    
    
}