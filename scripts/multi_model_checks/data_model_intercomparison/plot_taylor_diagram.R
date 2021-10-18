plot_taylor_diagram <- function(scenario) {
    
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
    
    
    ### ignore NAs
    ambDF[ambDF<=-999] <- NA
    eleDF[eleDF<=-999] <- NA
    
    ### prepare the years
    ambDF <- subset(ambDF, YEAR<2017&YEAR>2012)
    eleDF <- subset(eleDF, YEAR<2017&YEAR>2012)
    

    ### model list
    mod.list <- unique(ambDF$ModName)
    n <- length(mod.list)
    
    
    ### plot taylor diagram for GPP under ambient CO2
    #for (i in c(1:n)) {
    #    tmpDF <- ambDF3$GPP[ambDF3$ModName==mod.list[i]]
    #    
    #    if (i == 1) {
    #        taylor.diagram(mambDF$GPP,tmpDF, col=col.values[i]) 
    #    } else {
    #        taylor.diagram(mambDF$GPP,tmpDF, col=col.values[i], add=T)   
    #    }
    #}

    
    ### prepare GPP
    gppDF <- prepare_GPP_for_taylor_diagram(ambDF=ambDF, eleDF=eleDF)
    gppDF1 <- gppDF[gppDF$Trt=="amb",]
    gppDF2 <- gppDF[gppDF$Trt=="ele",]
    
    
    ### prepare LAI
    laiDF <- prepare_LAI_for_taylor_diagram(ambDF=ambDF, eleDF=eleDF)
    laiDF1 <- laiDF[laiDF$Trt=="amb",]
    laiDF2 <- laiDF[laiDF$Trt=="ele",]
    
    
    ### prepare Rsoil
    rsoilDF <- prepare_RSOIL_for_taylor_diagram(ambDF=ambDF, eleDF=eleDF)
    rsoilDF1 <- rsoilDF[rsoilDF$Trt=="amb",]
    rsoilDF2 <- rsoilDF[rsoilDF$Trt=="ele",]
    
    
    
    ### plotting script
    require(openair)
    
    
    
    
    pdf(paste0(out.dir, "/Taylor_diagrams_", scenario, ".pdf",sep=''),
        width=12,height=8)
    TaylorDiagram(gppDF1, obs="OBS", mod = "GPP", group="ModName", cols=c(col.values),
                  main="Amb GPP")
    TaylorDiagram(gppDF2, obs="OBS", mod = "GPP", group="ModName", cols=c(col.values),
                  main="Ele GPP")
    TaylorDiagram(laiDF1, obs="OBS", mod = "LAI", group="ModName", cols=c(col.values),
                  main="Amb LAI")
    TaylorDiagram(laiDF2, obs="OBS", mod = "LAI", group="ModName", cols=c(col.values),
                  main="Ele LAI")
    TaylorDiagram(rsoilDF1, obs="OBS", mod = "Rsoil", group="ModName", cols=c(col.values),
                  main="Amb Rsoil")
    TaylorDiagram(rsoilDF2, obs="OBS", mod = "Rsoil", group="ModName", cols=c(col.values),
                  main="Ele Rsoil")
    dev.off()
    
    
    
}