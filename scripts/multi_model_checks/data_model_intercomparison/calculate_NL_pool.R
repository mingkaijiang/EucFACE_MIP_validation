calculate_NL_pool <- function() {
    
    ### Canopy N concentration
    #canopy_n_concentration <- make_canopy_n_concentration()
    
    myDF <- read.csv("validation_dataset/canopy_n_pool.csv")
    
    ### summary
    outDF1 <- summaryBy(leaf_n_pool~Ring, data=myDF,
                       FUN=mean, na.rm=T, keep.names=T)
    
    outDF1$Treatment <- "eCO2"
    outDF1$Treatment[outDF1$Ring%in%c(2,3,6)] <- "aCO2"
    
    outDF <- summaryBy(leaf_n_pool~Treatment, data=outDF1,
                        FUN=c(mean,sd), na.rm=T, keep.names=T)
    
    
    
    return(outDF)
    
}