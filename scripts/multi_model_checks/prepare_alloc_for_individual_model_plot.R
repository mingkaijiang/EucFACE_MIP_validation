prepare_alloc_for_individual_model_plot <- function (ambDF=sumDF1,
                                                     eleDF=sumDF2) {
    
    ### Purpose:
    ### to prepare allocation coefficients based on time-averaged data
    
    ### get model list
    mod.list <- unique(ambDF$ModName)
    nmod <- length(mod.list)
    
    ### get dimensions of different variables
    var.list <- c("Canopy", "Wood", "Root", "Other")
    nvar <- length(var.list)
    
    
    ### create a DF for amb and ele
    myDF1 <- data.frame(rep(var.list, nmod*2), 
                        rep(mod.list, each=(nvar*2)), 
                        rep(c("amb", "ele"), each=nvar), 
                        NA, NA)
    colnames(myDF1) <- c("Variable", 
                         "ModName",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    
    ### add model output
    for (i in mod.list) {
        
        ### means
        myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="amb"&myDF1$Variable=="Canopy"] <- ambDF$value.mean[ambDF$ModName==i&ambDF$variable=="CGL"]/ambDF$value.mean[ambDF$ModName==i&ambDF$variable=="NPP"]
        myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="ele"&myDF1$Variable=="Canopy"] <- eleDF$value.mean[eleDF$ModName==i&eleDF$variable=="CGL"]/eleDF$value.mean[eleDF$ModName==i&eleDF$variable=="NPP"]
        
        myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="amb"&myDF1$Variable=="Wood"] <- ambDF$value.mean[ambDF$ModName==i&ambDF$variable=="CGW"]/ambDF$value.mean[ambDF$ModName==i&ambDF$variable=="NPP"]
        myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="ele"&myDF1$Variable=="Wood"] <- eleDF$value.mean[eleDF$ModName==i&eleDF$variable=="CGW"]/eleDF$value.mean[eleDF$ModName==i&eleDF$variable=="NPP"]
        
        myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="amb"&myDF1$Variable=="Root"] <- sum(c(ambDF$value.mean[ambDF$ModName==i&ambDF$variable=="CGCR"],
                                                                                          ambDF$value.mean[ambDF$ModName==i&ambDF$variable=="CGFR"]),na.rm=T)/ambDF$value.mean[ambDF$ModName==i&ambDF$variable=="NPP"]
        myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="ele"&myDF1$Variable=="Root"] <- sum(c(eleDF$value.mean[eleDF$ModName==i&eleDF$variable=="CGCR"],
                                                                                          eleDF$value.mean[eleDF$ModName==i&eleDF$variable=="CGFR"]),na.rm=T)/eleDF$value.mean[eleDF$ModName==i&eleDF$variable=="NPP"]
        
        myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="amb"&myDF1$Variable=="Other"] <- 1.0 - (round(myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="amb"&myDF1$Variable=="Canopy"],2)+
                                                                                                round(myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="amb"&myDF1$Variable=="Wood"],2)+
                                                                                                 round(myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="amb"&myDF1$Variable=="Root"],2))
        
        myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="ele"&myDF1$Variable=="Other"] <- 1.0 - (round(myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="ele"&myDF1$Variable=="Canopy"],2)+
                                                                                                       round(myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="ele"&myDF1$Variable=="Wood"],2)+
                                                                                                                 round(myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt=="ele"&myDF1$Variable=="Root"],2))
        
    }
    
    ### convert nan to na
    myDF1$meanvalue <- ifelse(is.nan(myDF1$meanvalue), NA, myDF1$meanvalue)
    
    ### round the mean
    myDF1$meanvalue <- round(myDF1$meanvalue, 2)
    
    ### now add plotting things
    for (i in mod.list) {
        for (j in c("amb","ele")) {
            # Compute the cumulative percentages (top of each rectangle)
            myDF1$ymax[myDF1$ModName==i&myDF1$Trt==j] <- cumsum(myDF1$meanvalue[myDF1$ModName==i&myDF1$Trt==j])
            
            # Compute the bottom of each rectangle
            myDF1$ymin[myDF1$ModName==i&myDF1$Trt==j] <- c(0, head(myDF1$ymax[myDF1$ModName==i&myDF1$Trt==j], n=-1))
            
            # Compute label position
            myDF1$labelPosition[myDF1$ModName==i&myDF1$Trt==j] <- (myDF1$ymax[myDF1$ModName==i&myDF1$Trt==j] + myDF1$ymin[myDF1$ModName==i&myDF1$Trt==j]) / 2
            
        }
    }
    
    
    ### add label
    myDF1$label <- paste0(myDF1$Variable, "\n ", myDF1$meanvalue)
    
    

    
    return(myDF1)
}