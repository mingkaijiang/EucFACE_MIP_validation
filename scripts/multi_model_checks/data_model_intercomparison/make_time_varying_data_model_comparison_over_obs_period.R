make_time_varying_data_model_comparison_over_obs_period <- function(scenario) {
  
  
    ##################################################################
    ### Purpose:
    ### to compare model predictions against data,
    ### including ambient and elevated treatment means,
    ### stoichiometry, efficiency, residence time, etc.,
    ### and the CO2 response difference and ratio,
    ### try to include all relevant variables.
    
    
    ##################################################################
    #### Set up basics
    ### setting out path to store the files
    ### this is only valid for variable climate
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/", scenario, "/")
  
    ### create output folder
    if(!dir.exists(out.dir)) {
      dir.create(out.dir, showWarnings = FALSE)
    }
  
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_AMB_daily.rds"))
    eleDF <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_OBS_", scenario, "_ELE_daily.rds"))
    
    d <- dim(ambDF)[2]
    
    #### calculate 4-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    ### get the list of models
    mod.list <- unique(ambDF.sum$ModName)
    nmod <- length(mod.list)
    
    
    
    ###########################################################################
    ##### Leaf area index
    #### A time series LAI data over the period of 2012 - 2016 was provided for validation purpose. 
    #### Models should aim to match the magnitude of LAI as well as its temporal patterns. 
    #### Note that in the observed dataset, the LAI data is really indicative of the vegetation structure as well as canopy leaf area. 
    #### validation LAI
    laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    #laiDF <- laiDF[laiDF$Trt=="aCO2",]
    laiDF$Date <- as.Date(as.character(laiDF$Date))
    laiDF$Trt <- gsub("aCO2", "amb", laiDF$Trt)
    laiDF$Trt <- gsub("eCO2", "ele", laiDF$Trt)
    laiDF$ModName <- "OBS"
    names(laiDF)[names(laiDF)=="lai"] <- "LAI"
    laiDF <- laiDF[,c("Date", "ModName", "Trt", "LAI")]
    
    
    ### simulated LAI, subset
    subDF1 <- ambDF[,c("YEAR", "DOY", "Date", "LAI", "ModName")]
    subDF2 <- eleDF[,c("YEAR", "DOY", "Date", "LAI", "ModName")]
    subDF1$Trt <- "amb"
    subDF2$Trt <- "ele"
    subDF <- rbind(subDF1, subDF2)
    
    subDF$Date <- as.Date(as.character(subDF$Date))
    subDF <- subDF[,c("Date", "ModName", "Trt", "LAI")]
        
    ### merge the two dataset
    testDF1 <- rbind(subDF, laiDF)
    
    ### plot all data
    p1 <- ggplot(testDF1, aes(x=Date)) +
        geom_line(aes(y=LAI, color=ModName, lty=ModName), lwd = 1) +
        theme_linedraw() +
        facet_wrap( ~ Trt)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        scale_color_manual(name="Model",
                           values=c(col.values, "OBS"="black"),
                           labels=c(model.labels, "OBS"= "OBS"))+
        scale_linetype_manual(name="Model", 
                              values=c(linetype.values, "OBS"=1),
                              labels=c(model.labels, "OBS"="OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                       lty = c(linetype.values, "OBS"=1))),
               color = guide_legend(nrow=12, byrow=F))+
        ylab("LAI")
    
    
    #### Soil respiration
    ### The measured soil respiration rate represents both root 
    ### and soil heterotrophic respiration flux. 
    ### It was up-scaled from the LICOR chambers by averaging 
    ### all measurements within the same treatment. 
    ### It was a model product, 
    ### in that we used DAMM model to establish relationship with soil temperature, 
    ### and then obtained the daily rate throughout the year. 
    ### Nevertheless, we expect modelers to provide a good match simulation to this dataset. 
    
    ### Note that we didn't ask the modelers to output soil respiration flux in the output protocol. 
    ### Please add heterotrophic respiration and root respiration to obtain soil respiration flux. 
    ### Also, please note that, the unit for all carbon fluxes is given in the output protocol, as gC m-2 d-1. 
    ### validation Rsoil
    rsoilDF <- read.csv("validation_dataset/EucFACE_daily_soil_respiration_flux_2013_2015.csv")
    rsoilDF$Date <- as.Date(as.character(rsoilDF$Date))
    
    rsoilDF$Trt <- gsub("aCO2", "amb", rsoilDF$Trt)
    rsoilDF$Trt <- gsub("eCO2", "ele", rsoilDF$Trt)
    
    ### convert unit, from mg m-2 d-1 to g m-2 d-1
    rsoilDF$Rsoil <- rsoilDF$Rsoil_mg_m2_d / 1000.0
    rsoilDF$ModName <- "OBS"
    rsoilDF <- rsoilDF[,c("Date", "ModName", "Trt", "Rsoil")]
    
    ### simulated Rsoil, subset
    
    subDF1 <- ambDF[,c("YEAR", "DOY", "Date", "RHET", "RCR", "RFR", "ModName")]
    subDF2 <- eleDF[,c("YEAR", "DOY", "Date", "RHET", "RCR", "RFR", "ModName")]
    
    subDF1$Trt <- "amb"
    subDF2$Trt <- "ele"
    subDF <- rbind(subDF1, subDF2)
    
    subDF$Date <- as.Date(as.character(subDF$Date))
    
    subDF[subDF<=-999.] <- NA
    
    subDF$Rsoil<- rowSums(data.frame(subDF$RHET, subDF$RCR, subDF$RFR), na.rm=T)
    subDF <- subDF[,c("Date", "ModName", "Trt", "Rsoil")]
    
    
    ### merge the two dataset
    testDF1 <- rbind(subDF, rsoilDF)
    
    
    ### plot all data
    p2 <- ggplot(testDF1, aes(x=Date)) +
        geom_line(aes(y=Rsoil, color=ModName, lty=ModName), lwd = 1) +
        theme_linedraw() +
        facet_wrap( ~ Trt)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        scale_color_manual(name="Model",
                           values=c(col.values, "OBS"="black"),
                           labels=c(model.labels, "OBS"= "OBS"))+
        scale_linetype_manual(name="Model", 
                              values=c(linetype.values, "OBS"=1),
                              labels=c(model.labels, "OBS"="OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
                                                       lty = c(linetype.values, "OBS"=1))),
               color = guide_legend(nrow=12, byrow=F))+
    ylab(expression(R[soil] * " (g C " * m^-2 * " " * d^-1 * " )"))
    
    
    
    
    ### print plots to file, change numbering if needed
    pdf(paste0(out.dir, '/MIP_Time_varying_variables.pdf',sep=''),
        width=12,height=8)
    grid.arrange(p1, p2, nrow=2, ncol=1)
    dev.off()
    
    
    ###########################################################################
    
    
}