compare_two_MIP_results <- function() {
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/processed_simulation/Medyn2016")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ### calculate 2016 multi-model means for CNP and CN models
    medDF <- calculate_Medlyn_2016_ModVersion_means(time.period=c(2012, 2019))
    
    ### prepare the current MIP output
    thisDF <- calculate_this_MIP_ModVersion_means(time.period=c(2012, 2019))
    
    ### compare the two
    myDF <- merge_this_MIP_and_Medlyn_2016(medDF=medDF, thisDF=thisDF)
    
    
    
    
}