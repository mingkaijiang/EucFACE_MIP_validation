plot_CO2_water_interaction_drought_year <- function (deficit.year) {
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/OBS_output/Drought/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ## variable climate, including drought
    ambDF1 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_VAR_NOP_AMB_annual.rds"))
    eleDF1 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_VAR_NOP_ELE_annual.rds"))
    
    ### fixed climate, all wet
    ambDF2 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_FIX_NOP_AMB_annual.rds"))
    eleDF2 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_FIX_NOP_ELE_annual.rds"))
    
    
    ### only look at drought year
    plot_annual_rainfall_deficit(fixDF=ambDF2, varDF=ambDF1,
                                 out.dir=out.dir)
    
    
    ### subset deficit year
    ambDF1 <- ambDF1[ambDF1$YEAR==deficit.year,]
    ambDF2 <- ambDF2[ambDF2$YEAR==deficit.year,]
    eleDF1 <- eleDF1[eleDF1$YEAR==deficit.year,]
    eleDF2 <- eleDF2[eleDF2$YEAR==deficit.year,]
 
    
    
    ### calculate cumulative water budget
    histDF1 <- compile_cumulative_water_budget(ambDF=ambDF1, eleDF=eleDF1)
    histDF2 <- compile_cumulative_water_budget(ambDF=ambDF2, eleDF=eleDF2)
    
    ### merge var and fix together
    histDF1$Climate <- "VAR"
    histDF2$Climate <- "FIX"
    
    histDF <- rbind(histDF1, histDF2)
    
    ### calculate WUE
    wueDF1 <- calculate_water_use_efficiency(ambDF=ambDF1, eleDF=eleDF1)
    wueDF2 <- calculate_water_use_efficiency(ambDF=ambDF2, eleDF=eleDF2)
    
    ### merge var and fix together
    wueDF1$Climate <- "VAR"
    wueDF2$Climate <- "FIX"
    
    wueDF <- rbind(wueDF1, wueDF2)
    
    
    ### calculate WUE proportionality
    propDF1 <- calculate_proportionality_of_WUE(ambDF=ambDF1, eleDF=eleDF1)
    propDF2 <- calculate_proportionality_of_WUE(ambDF=ambDF2, eleDF=eleDF2)
    
    ### merge var and fix together
    propDF1$Climate <- "VAR"
    propDF2$Climate <- "FIX"
    
    propDF <- rbind(propDF1, propDF2)
    
    
    ### get model list
    mod.list <- unique(histDF$ModName)
    
    ### plot
    p1 <- ggplot() +  
        geom_bar(histDF, stat = "identity", 
                 mapping=aes(Group, value, fill=variable),
                 position="stack", col="black") +
        xlab("") + 
        facet_grid(ModName~Climate)+
        ylab(expression(paste("Cumulative water budget (mm)"))) +
        scale_fill_manual(name="Variable",
                          values=c("PREC"=Diverge_hsv_Palette[1],
                                   "ET"=Diverge_hsv_Palette[2],
                                   "RO"=Diverge_hsv_Palette[4],
                                   "DRAIN"=Diverge_hsv_Palette[6],
                                   "deltaSW"=Diverge_hsv_Palette[8]),
                          labels=c("PREC"="PREC", 
                                   "ET"="ET",
                                   "RO"="RO",
                                   "DRAIN"="DRAIN",
                                   "deltaSW"=expression(Delta * SW)))+
        scale_x_discrete(breaks=c("1_PREC", "2_amb", "3_ele"),
                         labels=c("1_PREC"="PREC",
                                  "2_amb"="AMB",
                                  "3_ele"="ELE")) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0,
              strip.text.x = element_text(size = 20)); p1
    
    
    p2 <- ggplot(propDF, aes(ModName, Norm_response, group=Climate)) +  
        geom_bar(stat = "identity", 
                 aes(ModName, Norm_response, fill=Climate),
                 position=position_dodge(), col="black") +
        geom_hline(yintercept=1.0, lwd=2, col="black")+
        xlab("") + 
        ylab(expression("WUE sensitivity to " * eCO[2])) +
        scale_fill_manual(name="",
                          values=c("FIX"="blue1", "VAR"="orange"))+
        scale_x_discrete(breaks=mod.list,
                         labels=model.labels) +
        theme_linedraw() +
        theme(panel.grid.minor=element_line(colour="grey", size=0.5),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0,
              strip.text.x = element_text(size = 20))+
        coord_cartesian(ylim=c(0.8, 1.05)); p2
    
    
    
    pdf(paste0(out.dir, "/water_budget_HIST_comparison_", deficit.year, ".pdf"), width=6, height=16)
    plot(p1)
    dev.off()
    
    
    pdf(paste0(out.dir, "/WUE_HIST_comparison_", deficit.year, ".pdf"), width=10, height=6)
    plot(p2)
    dev.off()
    
}