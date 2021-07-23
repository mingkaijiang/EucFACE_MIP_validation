plot_CO2_response_ratio_for_individual_model <- function(source.dir, 
                                                         mod.abb, 
                                                         out.dir,
                                                         sim.period,
                                                         nutrient.trt) {
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE, recursive = TRUE)
    }
    
    ### check for QUJSM
    check.mod <- substr(source.dir, nchar(source.dir)-4, nchar(source.dir))
    
    if (check.mod == "QUJSM") {
        print("QUJSM model")
        mod.abb <- "QUJSM"
    } else {
        print(paste0(mod.abb, " model"))
    }
    
    ### ambient CO2, over observed period (2012-2019)
    ambDF1 <- read.csv(paste0(source.dir, "/EUC_", mod.abb, "_", sim.period, "_VAR_AMB_", nutrient.trt, "_D.csv"))  # dry
    ambDF2 <- read.csv(paste0(source.dir, "/EUC_", mod.abb, "_", sim.period, "_FIX_AMB_", nutrient.trt, "_D.csv"))  # wet
    
    ### elevated CO2, over observed period (2012-2019)
    eleDF1 <- read.csv(paste0(source.dir, "/EUC_", mod.abb, "_", sim.period, "_VAR_ELE_", nutrient.trt, "_D.csv"))  # dry
    eleDF2 <- read.csv(paste0(source.dir, "/EUC_", mod.abb, "_", sim.period, "_FIX_ELE_", nutrient.trt, "_D.csv"))  # wet
    
    ### obtain means, sums for stocks and fluxes
    if (mod.abb == "CABLP") {
        ambDF1 <- convert_into_annual_CABLP(ambDF1)
        ambDF2 <- convert_into_annual_CABLP(ambDF2)
        eleDF1 <- convert_into_annual_CABLP(eleDF1)
        eleDF2 <- convert_into_annual_CABLP(eleDF2)
    } else if (mod.abb == "LPJGP") {
        ambDF1 <- convert_into_annual_LPJGP(ambDF1)
        ambDF2 <- convert_into_annual_LPJGP(ambDF2)
        eleDF1 <- convert_into_annual_LPJGP(eleDF1)
        eleDF2 <- convert_into_annual_LPJGP(eleDF2)
    } else if (mod.abb == "LPJGN") {
        ambDF1 <- convert_into_annual_LPJGN(ambDF1)
        ambDF2 <- convert_into_annual_LPJGN(ambDF2)
        eleDF1 <- convert_into_annual_LPJGN(eleDF1)
        eleDF2 <- convert_into_annual_LPJGN(eleDF2)
    } else if (mod.abb == "GDAYN") {
        ambDF1 <- convert_into_annual_GDAYN(ambDF1)
        ambDF2 <- convert_into_annual_GDAYN(ambDF2)
        eleDF1 <- convert_into_annual_GDAYN(eleDF1)
        eleDF2 <- convert_into_annual_GDAYN(eleDF2)
    } else {
        ambDF1 <- convert_into_annual(ambDF1)
        ambDF2 <- convert_into_annual(ambDF2)
        eleDF1 <- convert_into_annual(eleDF1)
        eleDF2 <- convert_into_annual(eleDF2)
    }

    
    ### get dimension
    d <- dim(ambDF1)
    n <- d[2]
    
    ### calculate CO2 response ratio
    CO2DF1 <- CO2DF2 <- ambDF1
    CO2DF1[,2:n] <- eleDF1[,2:n]/ambDF1[,2:n]  # var
    CO2DF2[,2:n] <- eleDF2[,2:n]/ambDF2[,2:n]  # fix
    
    ### merge
    CO2DF1$Trt <- paste0(sim.period, "_VAR_", nutrient.trt)
    CO2DF2$Trt <- paste0(sim.period, "_FIX_", nutrient.trt)
    
    plotDF <- rbind(CO2DF1, CO2DF2)
    
    ### get column names
    col.names <- names(plotDF)
    
    ### get dimension
    d <- dim(plotDF)
    n <- d[2]
    
    ## reivse the model abbreviation
    if (check.mod == "QUJSM") {
        print("QUJSM model")
        mod.abb <- "QUJSM"
    } else {
        print(paste0(mod.abb, " model"))
    }
    
    ### plot CO2 response ratio
    pdf(paste0(out.dir, "/", mod.abb, "_", sim.period, "_", nutrient.trt, "_D_CO2_ratio.pdf"))
    
    for (i in 2:(n-1)) {
        p1 <- ggplot(plotDF) +
            geom_hline(yintercept=1)+
            geom_point(aes(x = YEAR, y = plotDF[,i], fill = Trt, pch = Trt), size=4)+
            geom_line(aes(x = YEAR, y = plotDF[,i], col=Trt))+
            theme_linedraw() +
            theme(panel.grid.minor=element_blank(),
                  axis.text.x=element_text(size=12),
                  axis.title.x=element_blank(),
                  axis.text.y=element_text(size=12),
                  axis.title.y=element_text(size=14),
                  legend.text=element_text(size=14),
                  legend.title=element_text(size=16),
                  panel.grid.major=element_blank(),
                  legend.position="bottom",
                  legend.box = 'horizontal',
                  legend.box.just = 'left',
                  plot.title = element_text(size=16, face="bold.italic", 
                                            hjust = 0.5))+
            ylab(paste0(colnames(plotDF)[i]))+
            scale_color_manual(name="",
                               limits=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                        paste0(sim.period, "_FIX_", nutrient.trt)),
                               labels=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                        paste0(sim.period, "_FIX_", nutrient.trt)),
                               values=c("orange", "cyan"),
                               guide=guide_legend(nrow=4))+
            scale_fill_manual(name="",
                              limits=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                       paste0(sim.period, "_FIX_", nutrient.trt)),
                              labels=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                       paste0(sim.period, "_FIX_", nutrient.trt)),
                              values=c("orange", "cyan"),
                              guide=guide_legend(nrow=4))+
            scale_linetype_manual(name="",
                                  limits=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                           paste0(sim.period, "_FIX_", nutrient.trt)),
                                  labels=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                           paste0(sim.period, "_FIX_", nutrient.trt)),
                                  values=c("solid", "dotted"),
                                  guide=guide_legend(nrow=4))+
            scale_shape_manual(name="",
                               limits=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                        paste0(sim.period, "_FIX_", nutrient.trt)),
                               labels=c(paste0(sim.period, "_VAR_", nutrient.trt), 
                                        paste0(sim.period, "_FIX_", nutrient.trt)),
                               values=c(24,21),
                               guide=guide_legend(nrow=4))+
            ggtitle(colnames(plotDF)[i])+
            xlab("Year")
        
        
        plot(p1)
    }
    
    dev.off()
    
    
}