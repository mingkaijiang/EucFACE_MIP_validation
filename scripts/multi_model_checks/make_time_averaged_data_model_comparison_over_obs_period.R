make_time_averaged_data_model_comparison_over_obs_period <- function(eucDF,
                                                                     p.mod.list, 
                                                                     n.mod.list, 
                                                                     d.mod.list) {
    
    ##################################################################
    #### Set up basics
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/obs_var_output")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    annDF.amb <- readRDS(paste0(out.dir, "/MIP_obs_var_amb_annual.rds"))
    annDF.ele <- readRDS(paste0(out.dir, "/MIP_obs_var_ele_annual.rds"))
    
    
    #### calculate 4-yr means in the simulation datasets
    annDF.amb <- subset(annDF.amb, YEAR>2012 & YEAR<2017)
    annDF.ele <- subset(annDF.ele, YEAR>2012 & YEAR<2017)
    
    annDF.pct.diff <- annDF.amb
    annDF.pct.diff[,3:149] <- (annDF.ele[,3:149]-annDF.amb[,3:149])/annDF.amb[,3:149] * 100.0
    
    annDF.amb.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                               data=annDF.amb,
                               keep.names=T, na.rm=T)
    
    annDF.ele.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                               data=annDF.ele,
                               keep.names=T, na.rm=T)
    
    annDF.diff.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                                data=annDF.pct.diff,
                                keep.names=T, na.rm=T)
    
    
    ##########################################################################
    #### add observation data to the model simulation dataset so that we can plot them together
    
    
    
    
    ##########################################################################
    #### plot model simulated ambient
    
    
    
    mod.list <- unique(annDF.amb.sum$ModName)
    nmod <- length(mod.list)
    
    ##########################################################################
    ####  Major carbon pools  
    ### Firstly we will check the major carbon pools, 
    ### as these data are provided in Table 1 in the parameter file. 
    ### Note that:
    ### * CFR combines fineroot (< 2 mm in diameter) and intermediate root (2-3 mm) in the observation;
    ### * CSOIL is for top 10 cm of soil in the observation;
    ### * CL includes overstorey leaf only in the observation;
    ### * CW includes branch and stem in the model simulation.
    
    ### create a DF to store observation data for vegetation carbon stocks
    vegDF <- data.frame(rep(c("CL", "CW", "CFR", "CCR"), (1+nmod)), 
                        rep(c("obs", mod.list), each=4), NA, NA)
    colnames(vegDF) <- c("Variable", 
                         "Group",
                         "meanvalue",
                         "sevalue")
    
    vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CL"] <- 151
    vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CW"] <- 4558
    vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CFR"] <- 227
    vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CCR"] <- 606
    #vegDF$meanvalue[vegDF$Group=="obs"&vegDF$Variable=="CSOIL"] <- 2183
    
    vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CL"] <- 14
    vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CW"] <- 321
    vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CFR"] <- 5
    vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CCR"] <- 60
    #vegDF$sevalue[vegDF$Group=="obs"&vegDF$Variable=="CSOIL"] <- 280
    
    for (i in mod.list) {
        vegDF$meanvalue[vegDF$Group==i&vegDF$Variable=="CL"] <- annDF.amb.sum$CL.mean[annDF.amb.sum$ModName==i]
        vegDF$meanvalue[vegDF$Group==i&vegDF$Variable=="CW"] <- annDF.amb.sum$CW.mean[annDF.amb.sum$ModName==i]
        vegDF$meanvalue[vegDF$Group==i&vegDF$Variable=="CFR"] <- annDF.amb.sum$CFR.mean[annDF.amb.sum$ModName==i]
        vegDF$meanvalue[vegDF$Group==i&vegDF$Variable=="CCR"] <- annDF.amb.sum$CCR.mean[annDF.amb.sum$ModName==i]
        #vegDF$meanvalue[vegDF$Group==i&vegDF$Variable=="CSOIL"] <- annDF.amb.sum$CSOIL.mean[annDF.amb.sum$ModName==i]
        
        vegDF$sevalue[vegDF$Group==i&vegDF$Variable=="CL"] <- annDF.amb.sum$CL.sd[annDF.amb.sum$ModName==i]/sqrt(4)
        vegDF$sevalue[vegDF$Group==i&vegDF$Variable=="CW"] <- annDF.amb.sum$CW.sd[annDF.amb.sum$ModName==i]/sqrt(4)
        vegDF$sevalue[vegDF$Group==i&vegDF$Variable=="CFR"] <- annDF.amb.sum$CFR.sd[annDF.amb.sum$ModName==i]/sqrt(4)
        vegDF$sevalue[vegDF$Group==i&vegDF$Variable=="CCR"] <- annDF.amb.sum$CCR.sd[annDF.amb.sum$ModName==i]/sqrt(4)
        #vegDF$sevalue[vegDF$Group==i&vegDF$Variable=="CSOIL"] <- annDF.amb.sum$CSOIL.sd[annDF.amb.sum$ModName==i]/sqrt(4)
    }
    
    vegDF$meanvalue <- ifelse(vegDF$meanvalue<0, NA, vegDF$meanvalue)
    
    
    ### Plotting
    p1 <- ggplot(data=vegDF, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        #geom_errorbar(aes(x=Group, ymin=meanvalue-sevalue, 
        #                  ymax=meanvalue+sevalue), 
        #              position="dodge", width=0.2) +
        ggtitle("Major ecosystem carbon pools")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Carbon pools (g C " * m^2*")")))+
        scale_x_discrete(limit=c("GDAYN", "LPJGN",
                                 "CABLP", "CABLP-VD",
                                 "LPJGP", "LPJGP-VD",
                                 "GDAYP", 
                                 "OCHDP", "OCHDX",
                                 "QUINC", "QUJSM",
                                 "obs"),
                         label=c("GDAYN", "LPJGN",
                                  "CABLP", "CABLP-VD",
                                  "LPJGP", "LPJGP-VD",
                                  "GDAYP", 
                                  "OCHDP", "OCHDX",
                                  "QUINC", "QUJSM",
                                  "OBS")); p1
    
    
    
    ################# Major carbon fluxes  ####################
    ### create a DF to store observation data for allocation coefficients
    outDF <- data.frame(rep(c("NEP", "GPP", "NPP", "CUE", "RAU"), (1+nmod)), 
                        rep(c("obs", mod.list), each = 5), NA)
    colnames(outDF) <- c("Variable", 
                         "Group",
                         "meanvalue")
    
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="NEP"] <- -8
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="GPP"] <- 1563
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="NPP"] <- 484
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="CUE"] <- 0.31
    outDF$meanvalue[outDF$Group=="obs"&outDF$Variable=="RAU"] <- 1079
    
    
    ### these fluxes were calculated above already
    for (i in mod.list) {
    ### assign values
    outDF$meanvalue[outDF$Group==i&outDF$Variable=="NEP"] <- annDF.amb.sum$NEP.mean[annDF.amb.sum$ModName==i]
    outDF$meanvalue[outDF$Group==i&outDF$Variable=="GPP"] <- annDF.amb.sum$GPP.mean[annDF.amb.sum$ModName==i]
    outDF$meanvalue[outDF$Group==i&outDF$Variable=="NPP"] <- annDF.amb.sum$NPP.mean[annDF.amb.sum$ModName==i]
    #outDF$meanvalue[outDF$Group==i&outDF$Variable=="CUE"] <- annDF.amb.sum$GPP.mean[annDF.amb.sum$ModName==i]
    outDF$meanvalue[outDF$Group==i&outDF$Variable=="RAU"] <- annDF.amb.sum$RAU.mean[annDF.amb.sum$ModName==i]
    
    }
    
    ### plotDF1
    plotDF1 <- outDF[outDF$Variable%in%c("GPP", "NPP", "RAU", "NEP"),]
    
    ### plotting GPP, NPP, and RAU
    p2 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("Major carbon fluxes")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Carbon fluxes (g C " * m^2 * " " * yr^-1 * ")"))); p2
    
    
    
    ################# allocation coefficient  ####################
    ### Allocation coefficients are calculated different comparing the data and the model. 
    ### In the EucFACE data, allocation to leaf includes allocation to overstorey and understorey leaves, 
    ### and allocation to root includes allocation to overstorey and understorey roots. 
    ### In the data, there is also an additional allocation coefficient to Mycorrhizae, 
    ### which can be grouped with allocation to root as total belowground allocation. 
    ### This total belowground allocation is comparable to allocation coefficient to root in the model. 
    
    ### create a DF to store observation data for allocation coefficients
    allocDF <- data.frame(rep(c("leaf", "wood", "root", "exudation", 
                                "belowground"), 2),
                          rep(c("obs", "sim"), each = 5), NA)
    colnames(allocDF) <- c("Variable", 
                           "Group",
                           "meanvalue")
    
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="leaf"] <- 0.48
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="wood"] <- 0.20
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="root"] <- 0.22
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="exudation"] <- 0.10
    allocDF$meanvalue[allocDF$Group=="obs"&allocDF$Variable=="belowground"] <- 0.32
    
    
    ### calcualte annual means in the simulated data
    subDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012)
    
    fluxDF <- summaryBy(GPP+NEP+NPP+CGL+CGW+CGFR+CGCR+CEX+RAU~YEAR, data=subDF, FUN=sum, na.rm=T, keep.names=T)
    
    ### assign values
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="leaf"] <- mean(fluxDF$CGL/fluxDF$NPP)
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="wood"] <- round(mean(fluxDF$CGW/fluxDF$NPP),2)
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="root"] <- round(mean((fluxDF$CGFR+fluxDF$CGCR)/fluxDF$NPP),2)
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="exudation"] <- round(mean((fluxDF$CEX)/fluxDF$NPP),2)
    allocDF$meanvalue[allocDF$Group=="sim"&allocDF$Variable=="belowground"] <- round(mean((fluxDF$CGFR+fluxDF$CGCR+fluxDF$CEX)/fluxDF$NPP),2)
    
    
    allocDF2 <- allocDF[allocDF$Variable%in%c("leaf", "wood", "root", "exudation"),]
    
    ### Plotting
    p2 <- ggplot(data=allocDF2, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        ggtitle("Allocation coefficient")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("allocation coefficients")
    
    
    
    ################# Major nutrient pools  ####################
    ### Below I provide several variables to help constrain the nutrient cycles in the model, 
    ### namely labile inorganic P pool (PLAB), 
    ### soil net N and P mineralization rate (NMIN and PMIN), 
    ### plant N and P uptake (NUP and PUP), 
    ### and soil N and P leaching (NLEACH and PLEACH). 
    ### We did not include total soil P pool, 
    ### because its size could be misleading given that 
    ### the majority of the P in the soil is stored as occluded form unavailable for plants. 
    ### Note that in the table below, simulated results are for top 30 cm of the soil, 
    ### but observed data are for top 10 cm only. 
    
    ### create a DF to store observation data for vegetation carbon stocks
    pDF <- data.frame(rep(c("PLAB", "PMIN", "NMIN", "NUP", "PUP",
                            "NLEACH", "PLEACH"), 2), 
                      rep(c("obs", "sim"), each = 7), NA)
    colnames(pDF) <- c("Variable", 
                       "Group",
                       "meanvalue")
    
    pDF$meanvalue[pDF$Group=="obs"&pDF$Variable=="PLAB"] <- 0.17
    pDF$meanvalue[pDF$Group=="obs"&pDF$Variable=="PMIN"] <- 0.3
    pDF$meanvalue[pDF$Group=="obs"&pDF$Variable=="NMIN"] <- 8.81
    
    
    ### calcualte annual means in the simulated data
    subDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012)
    
    poolDF <- summaryBy(PLAB~YEAR, data=subDF, FUN=mean, na.rm=T, keep.names=T)
    fluxDF <- summaryBy(PMIN+NMIN+NLEACH+PLEACH+NUP+PUP~YEAR, data=subDF, FUN=sum, na.rm=T, keep.names=T)
    
    ### assign values
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="PLAB"] <- round(poolDF$PLAB[poolDF$YEAR=="2016"], 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="PMIN"] <- round(mean(fluxDF$PMIN), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="NMIN"] <- round(mean(fluxDF$NMIN), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="PLEACH"] <- round(mean(fluxDF$PLEACH), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="NLEACH"] <- round(mean(fluxDF$NLEACH), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="PUP"] <- round(mean(fluxDF$PUP), 3)
    pDF$meanvalue[pDF$Group=="sim"&pDF$Variable=="NUP"] <- round(mean(fluxDF$NUP), 3)
    
    
    plotDF1 <- pDF[pDF$Variable%in%c("PLAB", "PMIN"),]
    plotDF2 <- pDF[pDF$Variable%in%c("NMIN"),]
    
    ### plotting
    p4 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("Major phosphorus pools")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Phosphorus pools (g P " * m^2 * ")")))
    
    
    p5 <- ggplot(data=plotDF2, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("Major nitrogen pool")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Nitrogen pool (g N " * m^2 * ")")))
    
    
    
    ################# stoichiometry  ####################
    ### create a DF to store observation data 
    stDF <- data.frame(rep(c("leaf", "sapwood", "wood", "fineroot", "soil"),2), 
                       rep(c("obs", "sim"), each = 5), 
                       NA, NA, NA, NA, NA, NA)
    colnames(stDF) <- c("Variable", "Group", 
                        "CN.mean", "CP.mean",  "NP.mean",
                        "CN.se", "CP.se",  "NP.se")
    
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 35.5
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 101.6
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="wood"] <- 110.2 
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 56.9
    stDF$CN.mean[stDF$Group=="obs"&stDF$Variable=="soil"] <- 13.8 
    
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 722 
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 3705 
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="wood"] <- 7696 
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 1626
    stDF$CP.mean[stDF$Group=="obs"&stDF$Variable=="soil"] <- 224
    
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 22.9 
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 35.6
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="wood"] <- 33.7 
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 28.7
    stDF$NP.mean[stDF$Group=="obs"&stDF$Variable=="soil"] <- 16.4 
    
    
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 2.7
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 14.7
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="wood"] <- 30.3
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 4.6
    stDF$CN.se[stDF$Group=="obs"&stDF$Variable=="soil"] <- 1.0
    
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="leaf"] <- 33
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 702
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="wood"] <- 982
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 81
    stDF$CP.se[stDF$Group=="obs"&stDF$Variable=="soil"] <- 39
    
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="leaf"] <-  0.1
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="sapwood"] <- 2.1
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="wood"] <- 2.7
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="fineroot"] <- 3.3
    stDF$NP.se[stDF$Group=="obs"&stDF$Variable=="soil"] <- 3.4
    
    
    
    
    ### create a DF to store simulated stocks
    tmpDF <- data.frame(rep(c("leaf", "wood", "fineroot", "soil"), 4), 
                        rep(c(2013:2016), each = 4), NA, NA, NA)
    colnames(tmpDF) <- c("Variable", 
                         "Year",
                         "Cstock",
                         "Nstock", 
                         "Pstock")
    
    ### calcualte annual means in the simulated data
    poolDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012 & DOY == 1)
    
    ### tmpDF assignment
    for (i in 2013:2016) {
        tmpDF$Cstock[tmpDF$Year==i&tmpDF$Variable=="leaf"] <- poolDF$CL[poolDF$YEAR == i]
        tmpDF$Cstock[tmpDF$Year==i&tmpDF$Variable=="wood"] <- poolDF$CW[poolDF$YEAR == i]
        tmpDF$Cstock[tmpDF$Year==i&tmpDF$Variable=="fineroot"] <- poolDF$CFR[poolDF$YEAR == i]
        tmpDF$Cstock[tmpDF$Year==i&tmpDF$Variable=="soil"] <- poolDF$CSOIL[poolDF$YEAR == i]
        
        tmpDF$Nstock[tmpDF$Year==i&tmpDF$Variable=="leaf"] <- poolDF$NL[poolDF$YEAR == i]
        tmpDF$Nstock[tmpDF$Year==i&tmpDF$Variable=="wood"] <- poolDF$NW[poolDF$YEAR == i]
        tmpDF$Nstock[tmpDF$Year==i&tmpDF$Variable=="fineroot"] <- poolDF$NFR[poolDF$YEAR == i]
        tmpDF$Nstock[tmpDF$Year==i&tmpDF$Variable=="soil"] <- poolDF$NSOIL[poolDF$YEAR == i]
        
        tmpDF$Pstock[tmpDF$Year==i&tmpDF$Variable=="leaf"] <- poolDF$PL[poolDF$YEAR == i]
        tmpDF$Pstock[tmpDF$Year==i&tmpDF$Variable=="wood"] <- poolDF$PW[poolDF$YEAR == i]
        tmpDF$Pstock[tmpDF$Year==i&tmpDF$Variable=="fineroot"] <- poolDF$PFR[poolDF$YEAR == i]
        tmpDF$Pstock[tmpDF$Year==i&tmpDF$Variable=="soil"] <- poolDF$PSOIL[poolDF$YEAR == i]
        
    }
    
    tmpDF$CN <- with(tmpDF, Cstock/Nstock)
    tmpDF$CP <- with(tmpDF, Cstock/Pstock)
    tmpDF$NP <- with(tmpDF, Nstock/Pstock)
    
    tmpDF2 <- summaryBy(CN+CP+NP~Variable, FUN=c(mean, se), data=tmpDF, keep.names=T)
    
    
    ### assign values
    stDF$CN.mean[stDF$Group=="sim"&stDF$Variable=="leaf"] <- tmpDF2$CN.mean[tmpDF2$Variable=="leaf"]
    stDF$CN.mean[stDF$Group=="sim"&stDF$Variable=="wood"] <-  tmpDF2$CN.mean[tmpDF2$Variable=="wood"]
    stDF$CN.mean[stDF$Group=="sim"&stDF$Variable=="fineroot"] <-  tmpDF2$CN.mean[tmpDF2$Variable=="fineroot"]
    stDF$CN.mean[stDF$Group=="sim"&stDF$Variable=="soil"] <-  tmpDF2$CN.mean[tmpDF2$Variable=="soil"]
    
    
    stDF$CP.mean[stDF$Group=="sim"&stDF$Variable=="leaf"] <-  tmpDF2$CP.mean[tmpDF2$Variable=="leaf"]
    stDF$CP.mean[stDF$Group=="sim"&stDF$Variable=="wood"] <-  tmpDF2$CP.mean[tmpDF2$Variable=="wood"]
    stDF$CP.mean[stDF$Group=="sim"&stDF$Variable=="fineroot"] <- tmpDF2$CP.mean[tmpDF2$Variable=="fineroot"]
    stDF$CP.mean[stDF$Group=="sim"&stDF$Variable=="soil"] <-  tmpDF2$CP.mean[tmpDF2$Variable=="soil"]
    
    stDF$NP.mean[stDF$Group=="sim"&stDF$Variable=="leaf"] <- tmpDF2$NP.mean[tmpDF2$Variable=="leaf"]
    stDF$NP.mean[stDF$Group=="sim"&stDF$Variable=="wood"] <- tmpDF2$NP.mean[tmpDF2$Variable=="wood"]
    stDF$NP.mean[stDF$Group=="sim"&stDF$Variable=="fineroot"] <- tmpDF2$NP.mean[tmpDF2$Variable=="fineroot"]
    stDF$NP.mean[stDF$Group=="sim"&stDF$Variable=="soil"] <- tmpDF2$NP.mean[tmpDF2$Variable=="soil"]
    
    
    
    stDF$CN.se[stDF$Group=="sim"&stDF$Variable=="leaf"] <- tmpDF2$CN.se[tmpDF2$Variable=="leaf"]
    stDF$CN.se[stDF$Group=="sim"&stDF$Variable=="wood"] <-  tmpDF2$CN.se[tmpDF2$Variable=="wood"]
    stDF$CN.se[stDF$Group=="sim"&stDF$Variable=="fineroot"] <-  tmpDF2$CN.se[tmpDF2$Variable=="fineroot"]
    stDF$CN.se[stDF$Group=="sim"&stDF$Variable=="soil"] <-  tmpDF2$CN.se[tmpDF2$Variable=="soil"]
    
    
    stDF$CP.se[stDF$Group=="sim"&stDF$Variable=="leaf"] <-  tmpDF2$CP.se[tmpDF2$Variable=="leaf"]
    stDF$CP.se[stDF$Group=="sim"&stDF$Variable=="wood"] <-  tmpDF2$CP.se[tmpDF2$Variable=="wood"]
    stDF$CP.se[stDF$Group=="sim"&stDF$Variable=="fineroot"] <- tmpDF2$CP.se[tmpDF2$Variable=="fineroot"]
    stDF$CP.se[stDF$Group=="sim"&stDF$Variable=="soil"] <- tmpDF2$CP.se[tmpDF2$Variable=="soil"]
    
    
    stDF$NP.se[stDF$Group=="sim"&stDF$Variable=="leaf"] <- tmpDF2$NP.se[tmpDF2$Variable=="leaf"]
    stDF$NP.se[stDF$Group=="sim"&stDF$Variable=="wood"] <- tmpDF2$NP.se[tmpDF2$Variable=="wood"]
    stDF$NP.se[stDF$Group=="sim"&stDF$Variable=="fineroot"] <- tmpDF2$NP.se[tmpDF2$Variable=="fineroot"]
    stDF$NP.se[stDF$Group=="sim"&stDF$Variable=="soil"] <- tmpDF2$NP.se[tmpDF2$Variable=="soil"]
    
    ### plot
    p6 <- ggplot(data=stDF, 
                 aes(Group, CN.mean, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        #geom_errorbar(aes(x=Group, ymin=CN.mean-CN.se, 
        #                  ymax=CN.mean+CN.se), 
        #              position=position_dodge2(), width=0.2) +
        ggtitle("CN stoichiometry")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("CN stoichiometry")
    
    p7 <- ggplot(data=stDF, 
                 aes(Group, CP.mean, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("CP stoichiometry")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("CP stoichiometry")
    
    
    p8 <- ggplot(data=stDF, 
                 aes(Group, NP.mean, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("NP stoichiometry")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("NP stoichiometry")
    
    
    
    ################# Nutrient retranslocation coefficients  ####################
    
    ### subset and calculate
    subDF <- subset(modDF, YEAR <= 2016 & YEAR > 2012)
    
    tmpDF <- summaryBy(NGL+PGL+NLRETR+PLRETR~YEAR, data=subDF, FUN=sum, keep.names=T, na.rm=T)
    
    
    ### create DF for output
    rtDF <- data.frame(rep(c("leafN", "leafP"), 2), 
                       rep(c("obs", "sim"), each = 2), NA)
    colnames(rtDF) <- c("Variable", "Group", "meanvalue")
    
    ### assign values
    rtDF$meanvalue[rtDF$Group=="obs"&rtDF$Variable=="leafN"] <- 0.31
    rtDF$meanvalue[rtDF$Group=="obs"&rtDF$Variable=="leafP"] <- 0.53
    
    rtDF$meanvalue[rtDF$Group=="sim"&rtDF$Variable=="leafN"] <- round(mean(tmpDF$NLRETR/tmpDF$NGL),2)
    rtDF$meanvalue[rtDF$Group=="sim"&rtDF$Variable=="leafP"] <- round(mean(tmpDF$PLRETR/tmpDF$PGL),2)
    
    
    ### plotting
    p9 <- ggplot(data=rtDF, 
                 aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="dodge", col="black") +
        ggtitle("Leaf retranslocation")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Leaf retranslocation")
    
    
    ### print plots to file, change numbering if needed
    pdf(paste0(out.dir, '/QC_Time_averaged_validation_',mod.abb,'.pdf',sep=''),width=10,height=8)
    for (i in 1:9) {
        print(get(paste("p",i,sep="")))
    }
    dev.off()
    
    
    ##########################################################################
    #### Step 4. Time-varying validation
    
    #### Leaf area index
    ### A time series LAI data over the period of 2012 - 2016 was provided for validation purpose. 
    ### Models should aim to match the magnitude of LAI as well as its temporal patterns. 
    ### Note that in the observed dataset, the LAI data is really indicative of the vegetation structure as well as canopy leaf area. 
    ### validation LAI
    laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    laiDF <- laiDF[laiDF$Trt=="aCO2",]
    laiDF$Date <- as.Date(as.character(laiDF$Date))
    
    ### simulated LAI, subset
    subDF <- subset(modDF, YEAR <= 2016)
    subDF <- subDF[,c("YEAR", "DOY", "Date", "LAI")]
    subDF$Date <- as.Date(as.character(subDF$Date))
    
    ### merge the two dataset
    testDF1 <- merge(subDF, laiDF, by="Date", all=T)
    
    ### plot all data
    p1 <- ggplot(testDF1, aes(x=Date)) +
        geom_errorbar(aes(ymin=lai-laiSD,
                          ymax=lai+laiSD, color="obs"))+
        geom_line(aes(y=LAI, color="sim"), lwd = 1) +
        theme_linedraw() +
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
        ylab("LAI")+
        scale_colour_manual("", 
                            values = c("obs"="black", "sim"="red2"),
                            labels = c("Observed", "Simulated"))
    
    ### Now we can check the goodness-of-fit of all days where observation is available. 
    ### A perfect fit should have slope of 1 and intercept of 0. 
    ### subset only days where observations are available
    testDF2 <- testDF1[complete.cases(testDF1$lai),]
    
    lm.fit <- lm(testDF2$LAI~testDF2$lai)
    
    ### plot all data
    p2 <- ggplot(testDF2, aes(x=lai, y=LAI)) +
        geom_point()+
        theme_linedraw() +
        geom_smooth(method="lm")+
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
        ylab("simulated LAI")+
        xlab("observed LAI")
    
    
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
    rsoilDF <- rsoilDF[rsoilDF$Trt=="aCO2",]
    rsoilDF$Date <- as.Date(as.character(rsoilDF$Date))
    
    ### convert unit, from mg m-2 d-1 to g m-2 d-1
    rsoilDF$Rsoil_g_m2_d <- rsoilDF$Rsoil_mg_m2_d / 1000.0
    rsoilDF$RsoilSD_g <- rsoilDF$RsoilSD / 1000.0
    
    
    ### simulated Rsoil, subset
    subDF <- subset(modDF, YEAR <= 2015 & YEAR > 2012)
    subDF <- subDF[,c("YEAR", "DOY", "Date", "RHET", "RCR", "RFR")]
    subDF$Date <- as.Date(as.character(subDF$Date))
    subDF$Rsoil_sim <- with(subDF, RHET+RCR+RFR)
    
    
    
    ### merge the two dataset
    testDF1 <- merge(subDF, rsoilDF, by="Date", all=T)
    
    ### plot all data
    p3 <- ggplot(testDF1, aes(x=Date)) +
        geom_errorbar(aes(ymin=Rsoil_g_m2_d-RsoilSD_g,
                          ymax=Rsoil_g_m2_d+RsoilSD_g, color="obs"))+
        geom_line(aes(y=Rsoil_sim, color="sim"), lwd = 1) +
        theme_linedraw() +
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
        ylab("Soil respiration flux")+
        scale_colour_manual("", 
                            values = c("obs"="black", "sim"="red2"),
                            labels = c("Observed", "Simulated"))
    
    
    #### Soil water content
    ### Soil water content is more complex and non-linear, 
    ### depending on many model-specific settings. 
    ### The validation dataset only serves as a guidance to evaluate your model performance. 
    ### validation Rsoil
    swcDF <- read.csv("validation_dataset/EucFACE_SWC_2012_2019.csv")
    
    ### convert unit from VWC to kg H2O m-2
    swcDF$multiplier <- ifelse(swcDF$Depth%in%c(25,50,75,100,125,150), 0.25, 0.5)
    
    swcDF$WC_kg_m2 <- swcDF$VWC * swcDF$multiplier * 1000.0 / 100.0
    
    sumDF <- summaryBy(WC_kg_m2~Location+Date, FUN=sum, data=swcDF, keep.names=T, na.rm=T)
    
    ### process simulation data
    subDF <- modDF[,c("YEAR", "DOY", "Date", "SW", "SWPA")]
    
    plotDF <- merge(subDF, sumDF, by="Date", all=T)
    
    
    ### plot all data
    p4 <- ggplot(plotDF, aes(x=Date)) +
        geom_point(aes(y=WC_kg_m2, color="obs"))+
        geom_point(aes(y=SWPA, color="sim"), lwd = 1) +
        theme_linedraw() +
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
        ylab("Soil water (kg m-2)")+
        scale_colour_manual("", 
                            values = c("obs"="black", "sim"="red2"),
                            labels = c("Observed", "Simulated"))
    
    
    ### print plots to file, change numbering if needed
    pdf(paste0(out.dir, '/QC_Time_varying_validation_',mod.abb,'.pdf',sep=''),width=10,height=8)
    for (i in 1:4) {
        print(get(paste("p",i,sep="")))
    }
    dev.off()
    
    ##########################################################################
}