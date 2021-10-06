prepare_allocation_coef_DF_for_time_averaged_data_model_intercomparison <- function(eucDF,
                                                                                    ambDF,
                                                                                    eleDF,
                                                                                    difDF) {
    
    ### Purpose:
    ### to prepare allocation coefficients based on time-averaged data
    
    ### get model list
    mod.list <- unique(ambDF$ModName)
    nmod <- length(mod.list)
    
    ### get dimensions of different variables
    var.list <- c("Canopy", "Wood", "Root", "Other")
    nvar <- length(var.list)

    
    ### create a DF for aCO2 and eCO2
    myDF1 <- data.frame(rep(var.list, (1+nmod)*2), 
                        rep(c("obs", mod.list), each=(nvar*2)), 
                        rep(c("aCO2", "eCO2"), each=nvar), 
                        NA, NA)
    colnames(myDF1) <- c("Variable", 
                         "Group",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    ### add obs data
    for (i in c("aCO2", "eCO2")) {
        myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable=="Canopy"] <- eucDF$CGL[eucDF$Group=="mean"&eucDF$Trt==i]/eucDF$NPP[eucDF$Group=="mean"&eucDF$Trt==i]
        myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable=="Wood"] <- eucDF$CGW[eucDF$Group=="mean"&eucDF$Trt==i]/eucDF$NPP[eucDF$Group=="mean"&eucDF$Trt==i]
        myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable=="Root"] <- sum(c(eucDF$CGCR[eucDF$Group=="mean"&eucDF$Trt==i],
                                                                                                eucDF$CGFR[eucDF$Group=="mean"&eucDF$Trt==i]), na.rm=T)/eucDF$NPP[eucDF$Group=="mean"&eucDF$Trt==i]
        myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable=="Other"] <- 1.0 - (myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable=="Canopy"]+
                                                                                               myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable=="Wood"]+
                                                                                               myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable=="Root"])
        
    }
    
    ### add model output
    for (i in mod.list) {
        
        ### means
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="Canopy"] <- ambDF$CGL.mean[ambDF$ModName==i]/ambDF$NPP.mean[ambDF$ModName==i]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="Canopy"] <- eleDF$CGL.mean[eleDF$ModName==i]/eleDF$NPP.mean[eleDF$ModName==i]
        
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="Wood"] <- ambDF$CGW.mean[ambDF$ModName==i]/ambDF$NPP.mean[ambDF$ModName==i]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="Wood"] <- eleDF$CGW.mean[eleDF$ModName==i]/eleDF$NPP.mean[eleDF$ModName==i]
        
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="Root"] <- sum(c(ambDF$CGCR.mean[ambDF$ModName==i],
                                                                                          ambDF$CGFR.mean[ambDF$ModName==i]),na.rm=T)/ambDF$NPP.mean[ambDF$ModName==i]
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="Root"] <- sum(c(eleDF$CGCR.mean[eleDF$ModName==i],
                                                                                          eleDF$CGFR.mean[eleDF$ModName==i]),na.rm=T)/eleDF$NPP.mean[eleDF$ModName==i]
        
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="Other"] <- 1.0 - (myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="Canopy"]+
                                                                                                myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="Wood"]+
                                                                                                myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable=="Root"])
        myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="Other"] <- 1.0 - (myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="Canopy"]+
                                                                                                myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="Wood"]+
                                                                                                myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable=="Root"])
        
    }
    
    ### convert nan to na
    myDF1$meanvalue <- ifelse(is.nan(myDF1$meanvalue), NA, myDF1$meanvalue)

    
    
    
    ### get the diff and % diff DF
    myDF2 <- data.frame(rep(var.list, (1+nmod)*2), 
                        rep(c("obs", mod.list), each=(nvar*2)), 
                        rep(c("diff", "pct_diff"), each=nvar), 
                        NA, NA)
    colnames(myDF2) <- c("Variable", 
                         "Group",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    ### add obs data
    for (i in unique(myDF1$Group)) {
        for (j in var.list) {
            tryCatch({
                myDF2$meanvalue[myDF2$Group==i&myDF2$Trt=="diff"&myDF2$Variable==j] <- myDF1$meanvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="eCO2"] - myDF1$meanvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="aCO2"]
                myDF2$meanvalue[myDF2$Group==i&myDF2$Trt=="pct_diff"&myDF2$Variable==j] <- myDF2$meanvalue[myDF2$Group==i&myDF2$Trt=="diff"&myDF2$Variable==j]/myDF1$meanvalue[myDF1$Group==i&myDF1$Variable==j&myDF1$Trt=="aCO2"] * 100.0
            }, error=function(e){})
        }
    }    
    
    
    
    ### convert nan to na
    myDF2$meanvalue <- ifelse(is.nan(myDF2$meanvalue), NA, myDF2$meanvalue)

    
    ### return
    outDF <- rbind(myDF1, myDF2)
        
    

    return(outDF)
}
