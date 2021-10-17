plot_gradual_increase_CO2_trajectories <- function (climate.scenario) {
    
    ### purpose:
    ### to plot the normalized predicted trajectories
    ### also plot the CO2 response ratios,
    ### under var vs. fix
    ### under different P fertilization rates
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/output/MIP_output/PRD_output/", climate.scenario, "/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read input - only the ambient CO2 treatment, 
    ### group into fixed and variable climate.
    inDF1 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_", 
                            climate.scenario, "_NOP_AMB_annual.rds"))
    inDF2 <- readRDS(paste0("output/MIP_output/processed_simulation/MIP_ALL_", 
                            climate.scenario, "_NOP_ELE_annual.rds"))
    
    inDF1$Trt <- "amb"
    inDF2$Trt <- "ele"
    
    myDF <- rbind(inDF1, inDF2)
    
    ### plot the CO2 trajectory
    p1 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=CO2, group=Trt))+
        geom_abline(intercept=550, slope=0, col="black")+
        geom_line(aes(col=Trt))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))
    
    
    pdf(paste0(out.dir, 
               "CO2_concentration_", climate.scenario, 
               ".pdf"))
    plot(p1)
    dev.off()
    
    
    ### We can now subset the years of interest
    subDF1 <- inDF1[inDF1$YEAR>=2013&inDF1$YEAR<=2018,]
    subDF2 <- inDF2[inDF2$YEAR>=2013&inDF2$YEAR<=2018,]
    subDF3 <- inDF1[inDF1$YEAR>=2064&inDF1$YEAR<=2069,]
    
    
    ### add scenario
    subDF1$Group <- "1_amb"
    subDF2$Group <- "2_sharp"
    subDF3$Group <- "3_gradual"
    
    
    ### myDF
    myDF <- rbind(subDF1, rbind(subDF2, subDF3))
    
    plotDF1 <- summaryBy(.~ModName+Group, FUN=c(mean,sd), data=myDF,
                         keep.names=T, na.rm=T)
    
    
    
    #### Plot normalized responses
    p1 <- ggplot(data=plotDF1, 
                 aes(x=Group, y=GPP.mean))+
        geom_bar(stat = "identity", 
                 mapping=aes(Group, GPP.mean, fill=Group),
                 position=position_dodge(), col="black") +
        geom_errorbar(aes(x=Group, ymin=GPP.mean-GPP.sd, 
                                  ymax=GPP.mean+GPP.sd), 
                      width=0.1, size=0.6, color="black") + 
        xlab("") + 
        facet_wrap( ~ ModName)+
        ylab(expression(paste("GPP (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(breaks=c("1_amb", 
                                  "2_sharp", 
                                  "3_gradual"),
                         labels=c("1_amb"="Amb",
                                  "2_sharp"="Sharp",
                                  "3_gradual"="Gradual")) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0,
              strip.text.x = element_text(size = 20))
    
    
    
    ### plot
    pdf(paste0(out.dir, 
               "gradual_vs_sharp_CO2_", climate.scenario, 
               "_key_variables.pdf"))
    plot(p1)
    dev.off()
    
    
}