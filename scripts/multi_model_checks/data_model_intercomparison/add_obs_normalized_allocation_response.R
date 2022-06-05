add_obs_normalized_allocation_response <- function() {
    
    myDF1 <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/inout.csv")
    myDF2 <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/npp.csv")
    myDF3 <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/delta_pool.csv")
    
    myDF4 <- read.csv("validation_dataset/EucFACE_C_Budget_data/summary/NEP_normalized_method_comparison.csv")

    
    ### calculate the CO2 effect via eCO2 - aCO2 for all major flux components
    gpp.diff <- sum(myDF1$diff[myDF1$term%in%c("GPP overstorey", "GPP understorey")])
    
    rauto.diff <- sum(myDF1$diff[myDF1$term%in%c("Ra leaf", 
                                                 "Ra stem",
                                                 "Ra root",
                                                 "Ra understorey",
                                                 "Rherbivore",
                                                 "Rgrowth")])
    
    rhet.diff <- myDF2$diff[myDF2$term%in%c("R hetero")]
    
    npp.diff1 <- sum(myDF2$diff[myDF2$term%in%c("Leaf NPP", 
                                               "Stem NPP",
                                               "Fine Root NPP",
                                               "Intermediate Root NPP",
                                               "Coarse Root NPP",
                                               "Other NPP",
                                               "Understorey NPP", 
                                               "Understorey Litter",
                                               "Frass production",
                                               "Leaf consumption")])
    
    #myco.diff <- myDF4$eCO2[myDF4$Category=="Figure3inset"&myDF4$Method=="NPP-Rh"] -
    #    myDF4$aCO2[myDF4$Category=="Figure3inset"&myDF4$Method=="NPP-Rh"] -
    #    (myDF4$eCO2[myDF4$Category=="Figure3"&myDF4$Method=="NPP-Rh"] -
    #         myDF4$aCO2[myDF4$Category=="Figure3"&myDF4$Method=="NPP-Rh"])
    
    myco.diff <- gpp.diff - rauto.diff - npp.diff1
    npp.diff <- npp.diff1 + myco.diff
    
    deltaveg.diff <- sum(myDF3$diff[myDF3$term%in%c("Overstorey leaf", 
                                                 "Overstorey wood",
                                                 "Understorey above-ground",
                                                 "Fine Root",
                                                 "Intermediate Root",
                                                 "Coarse Root",
                                                 "Litter",
                                                 "Insects")])
    
    deltasoil.diff <- sum(myDF3$diff[myDF3$term%in%c('Microbial biomass',
                                                    "Soil C",
                                                    "Mycorrhizae")])
    
    
    ### make different dataframes
    sumDF1 <- data.frame("variable"=c("NPP", "RAU"),
                        "norm.value" = c(npp.diff, rauto.diff))
    
    sumDF2 <- data.frame("variable" = c("RAU", "RHET", "deltaCVEG", "deltaCSOIL"),
                        "norm.value" = c(rauto.diff, rhet.diff, deltaveg.diff,
                                    deltasoil.diff))
    
    
    ### calculate the normalized response
    sumDF1$norm.value <- sumDF1$norm.value / gpp.diff 
    sumDF2$norm.value <- sumDF2$norm.value / gpp.diff
    
    
    ### add modname
    sumDF1$ModName <- "OBS"
    sumDF2$ModName <- "OBS"
    
    sumDF1 <- sumDF1[,c("ModName", "variable", "norm.value")]
    sumDF2 <- sumDF2[,c("ModName", "variable", "norm.value")]
    
    out <- list(outDF1=sumDF1,
                outDF2=sumDF2)
    
    
}