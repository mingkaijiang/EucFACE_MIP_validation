plot_normalized_pred_trajectories <- function (climate.scenario) {
    
    ### purpose:
    ### to plot the normalized predicted trajectories
    ### also plot the CO2 response ratios,
    ### under var vs. fix
    ### under different P fertilization rates
    
    ### read input - only the ambient CO2 treatment, 
    ### group into fixed and variable climate.
    inDF1 <- readRDS(paste0("pred_", climate.scenario, 
                            "_output/MIP_normalized_pred_", 
                            climate.scenario, "_NOP_amb_annual.rds"))
    inDF2 <- readRDS(paste0("pred_", climate.scenario, 
                            "_output/MIP_normalized_pred_", 
                            climate.scenario, "_MDP_amb_annual.rds"))
    inDF3 <- readRDS(paste0("pred_", climate.scenario, 
                            "_output/MIP_normalized_pred_", 
                            climate.scenario, "_HIP_amb_annual.rds"))
    
    inDF1$PTRT <- "NOP"
    inDF2$PTRT <- "MDP"
    inDF3$PTRT <- "HIP"
    
    myDF <- rbind(inDF1, rbind(inDF2, inDF3))
    
 
    
    #### Plot normalized responses
    p1 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=GPP))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p1
    
    
    p2 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=NPP))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p2
    
    
    p3 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=PMIN))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p3
    
    
    p4 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=PGL))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p4
    
    
    p5 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=CL))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p5
    
    
    p6 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=CGL))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p6
    
    
    p7 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=PUP))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p7
    
    
    p9 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=LAI))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p9
    
    
    p10 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=RHET))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p10

    
    p11 <- ggplot(data=myDF, 
                 aes(x=YEAR, y=CW))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p11
    
    
    p12 <- ggplot(data=myDF, 
                  aes(x=YEAR, y=CFR))+
        geom_abline(intercept=1, slope=0, col="black")+
        geom_vline(xintercept=2022, col="black", lty=2)+
        geom_line(aes(col=PTRT))+
        #geom_point(aes(pch=PTRT, fill=PTRT), col="black")+
        scale_shape_manual(name="Fertilization",
                           values=c("NOP"=21,
                                    "MDP"=22,
                                    "HIP"=23))+
        scale_color_manual(name="Fertilization",
                           values=c("NOP"="pink",
                                    "MDP"="orange",
                                    "HIP"="red3"))+
        facet_wrap(ModName~., ncol=2)+
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
                                        hjust = 0.5)); p12
    
    
    
    pdf(paste0("pred_", climate.scenario, "_output/normalized_pred_amb_key_variables.pdf"))
    for (i in 1:12) {
        print(get(paste("p",i,sep="")))
    }
    dev.off()
}