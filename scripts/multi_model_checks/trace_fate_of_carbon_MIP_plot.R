trace_fate_of_carbon_MIP_plot <- function(scenario) {
    
    ### setting out path to store the files
    out.dir <- paste0(getwd(), "/obs_", scenario, "_output")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    ### read in anual datasets
    ambDF <- readRDS(paste0(out.dir, "/MIP_obs_", scenario, "_amb_annual.rds"))
    eleDF <- readRDS(paste0(out.dir, "/MIP_obs_", scenario, "_ele_annual.rds"))
    
    
    ### calculate 4-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2017)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2017)
    
    d<-dim(ambDF)[2]
    
    ### summaryby
    co2DF <- ambDF
    co2DF[,3:d] <- eleDF[,3:d]-ambDF[,3:d]
    
    ### extract only a subset of variables
    outDF <- co2DF[,c("ModName", "YEAR", 
                      "GPP", 
                      "RAU", "RHET", "CVOC",
                      "CGL", "CGW", "CGFR", "CGCR", "CEX", "CREPR", 
                      "CLITIN", "CWLIN", "CFRLIN", "CCRLIN", 
                      "CL", "CW", "CFR", "CCR", "CSOIL", "CFLIT", "CSTOR",
                      "deltaCL", "deltaCW", "deltaCFR",
                      "deltaCCR", "deltaCSOIL", "deltaCFLIT", "deltaCSTOR")]
    
    d <- dim(outDF)[2]
    mod.list <- unique(outDF$ModName)
    nmod <- length(mod.list)
    
    longDF <- reshape2::melt(outDF, id.vars = c("ModName", "YEAR"))
    
    sumDF <- summaryBy(value~ModName+variable, FUN=c(mean, sd),
                      data=longDF, na.rm=T, keep.names=T)
    
    
    #### exclude all pools
    sumDF$Category <- rep(c(rep("gpp", 1),  
                           rep("resp", 3), 
                           rep("prod", 6),
                           rep("litter", 4),
                           rep("pool", 7), 
                           rep("change_in_pool", 7)), 12)
    
    
    sumDF$conf_low <- sumDF$value.mean - sumDF$value.sd
    sumDF$conf_high <- sumDF$value.mean + sumDF$value.sd
    
    ### Subset GPP, NPP, change in pools, and out fluxes
    plotDF1 <- subset(sumDF, variable %in% c("ModName", 
                                             "GPP", 
                                             "RAU", "RHET", "CVOC",
                                             "CGL", "CGW", "CGFR", "CGCR", "CEX", "CREPR", 
                                             "deltaCL", "deltaCW", "deltaCFR",
                                             "deltaCCR", "deltaCSOIL", "deltaCFLIT", "deltaCSTOR"))
    
    ### Add plot category
    plotDF1$plot.cat[plotDF1$Category=="gpp"] <- "Influxes"
    plotDF1$plot.cat[plotDF1$Category=="change_in_pool"] <- "Change_in_pools"
    plotDF1$plot.cat[plotDF1$Category=="resp"] <- "Outfluxes"
    plotDF1$plot.cat[plotDF1$Category=="prod"] <- "NPP"

    ### obtain overall fluxes, i.e. influx, outflux, change in pools and npp sums
    plotDF2 <- summaryBy(value.mean~plot.cat+ModName, data=plotDF1,
                         FUN=sum, keep.names=T, na.rm=T)
    
    ### obtain their sd
    for (i in mod.list) {
        for (j in c("Influxes", "Change_in_pools", "Outfluxes", "NPP")) {
            tmpDF <- plotDF1$value.sd[plotDF1$ModName==i&plotDF1$plot.cat==j]
            l <- length(tmpDF)
            val <- sqrt(sum(tmpDF^2, na.rm=T)/l)
            plotDF2$value.sd[plotDF2$ModName==i&plotDF2$plot.cat==j] <- val
            
        }
    }
    
    ### Assign plot cat 2
    plotDF2$plot.cat2[plotDF2$plot.cat=="Influxes"] <- "A"
    plotDF2$plot.cat2[plotDF2$plot.cat=="NPP"] <- "B"
    plotDF2$plot.cat2[plotDF2$plot.cat=="Outfluxes"] <- "C"
    plotDF2$plot.cat2[plotDF2$plot.cat=="Change_in_pools"] <- "C"
    
    ### need to add Ra as B
    tmpDF <- plotDF1[plotDF1$variable=="RAU",]
    tmpDF$plot.cat <- "Total_Ra"
    tmpDF$plot.cat2 <- "B"
    tmpDF <- tmpDF[,c("plot.cat", "ModName", "value.mean", "value.sd", "plot.cat2")]
    
    plotDF2 <- rbind(plotDF2, tmpDF)
    
    ### next to include individual fluxes on NPP
    tmpDF <- plotDF1[plotDF1$plot.cat=="NPP",]
    tmpDF$plot.cat <- tmpDF$variable
    tmpDF$plot.cat2 <- "D"
    tmpDF <- tmpDF[,c("plot.cat", "ModName", "value.mean", "value.sd", "plot.cat2")]
    
    plotDF2 <- rbind(plotDF2, tmpDF)
    
    
    ### next to include individual fluxes on outfluxes
    tmpDF <- plotDF1[plotDF1$plot.cat=="Outfluxes",]
    tmpDF$plot.cat <- tmpDF$variable
    tmpDF$plot.cat2 <- "E"
    tmpDF <- tmpDF[,c("plot.cat", "ModName", "value.mean", "value.sd", "plot.cat2")]
    
    plotDF2 <- rbind(plotDF2, tmpDF)
    
    
    ### next to include individual fluxes on change in pools
    tmpDF <- plotDF1[plotDF1$plot.cat=="Change_in_pools",]
    tmpDF$plot.cat <- tmpDF$variable
    tmpDF$plot.cat2 <- "F"
    tmpDF <- tmpDF[,c("plot.cat", "ModName", "value.mean", "value.sd", "plot.cat2")]
    
    plotDF2 <- rbind(plotDF2, tmpDF)
    
    
    ### Order plot DF
    plotDF2 <- plotDF2[order(plotDF2$plot.cat2),]
    
    ## gpp
    A.col.list <- viridis(6)[1] 
    
    # 2nd column bar - NPP + Ra 
    B.col.list <- viridis(6)[3:4] 
    
    # 3rd column bar - Change in pools + all R
    C.col.list <- viridis(6)[5:6] 
    
    ## NPP
    D.col.list <- brewer.pal(6,"Greens")[1:6]
    
    ## R
    E.col.list <- brewer.pal(6,"YlOrRd")[4:6]
    
    ### Change in pools
    F.col.list <- brewer.pal(7,"Blues")[1:7]
    
    
    ### Combine all color list
    col.list2 <- c("Influxes"=A.col.list[1],                    
                   "NPP"=B.col.list[1],   
                   "Total_Ra"=B.col.list[2],      
                   "Change_in_pools"=C.col.list[1],   
                   "Outfluxes"=C.col.list[2],
                   "CGL"=D.col.list[1],                
                   "CGW"=D.col.list[2],
                   "CGFR"=D.col.list[3],           
                   "CGCR"=D.col.list[4],
                   "CEX"=D.col.list[5], 
                   "CREPR"=D.col.list[6], 
                   "RAU"=E.col.list[1],           
                   "RHET"=E.col.list[2],           
                   "CVOC"=E.col.list[3],
                   "deltaCL"=F.col.list[1],        
                   "deltaCW"=F.col.list[2],
                   "deltaCFR"=F.col.list[3],    
                   "deltaCCR"=F.col.list[4],
                   "deltaCSOIL"=F.col.list[5],
                   "deltaCFLIT"=F.col.list[6],
                   "deltaCSTOR"=F.col.list[7])  
    
    
    # y label
    y.lab2 <- c("Influxes"=expression(GPP[o]),                    
                "NPP"="NPP",                                      
                "Total_Ra"=expression(R[a]),                      
                "Change_in_pools"=expression(Delta*C[pools]),
                "Outfluxes"="R",                           
                "CGL"=expression(NPP[ol]),                
                "CGW"=expression(NPP[stem]),               
                "CGFR"=expression(NPP[froot]),           
                "CGCR"=expression(NPP[croot]),               
                "CEX"=expression(NPP[exud]),          
                "CREPR"=expression(C[rprod]),           
                "RAU"=expression(R[au]),           
                "RHET"=expression(R[het]),      
                "CVOC"=expression(R[voc]),      
                "deltaCL"=expression(Delta*C[leaf]),        
                "deltaCW"=expression(Delta*C[stem]),         
                "deltaCFR"=expression(Delta*C[froot]),    
                "deltaCCR"=expression(Delta*C[croot]),
                "deltaCSOIL"=expression(Delta*C[soil]),
                "deltaCFLIT"=expression(Delta*C[lit]),
                "deltaCSTOR"=expression(Delta*C[store]))
    
    
    
    ###################### prepare individual models #########################
    #### CABLE POP
    i <- "A_CABLP"
    
    subDF <- plotDF2[plotDF2$ModName==i,]
    
    ### split the groups and make separate plots
    plotDF.sub1 <- subset(subDF, plot.cat2%in%c("A", "B", "C"))
    
    plotDF.sub2 <- subset(subDF, plot.cat2=="D")
    
    plotDF.sub3 <- subset(subDF, plot.cat2=="E")
    
    plotDF.sub4 <- subset(subDF, plot.cat2=="F")
    
    ### make plots
    p1_1 <- ggplot(plotDF.sub1,
                 aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + 
        #ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        ylab(" ") +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools]))))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2),legend.justification = c(0, 1))+
        annotate(geom="text", x=1, y=-100, label="CABLP", size=7)
    
    
    p1_2 <- ggplot(plotDF.sub2,
                 aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
        #annotate(geom="text", x=0.5, y=500, label="b", size=7)
    
    
    p1_3 <- ggplot(plotDF.sub3,
                 aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("R"))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2, nrow=3))#+
        #annotate(geom="text",x=0.5, y=500, label="c", size=7)
    
    
    p1_4 <- ggplot(plotDF.sub4,
                 aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
        #annotate(geom="text", x=0.5, y=500, label="d", size=7)
    
    
    plots_A_CABLP <- plot_grid(p1_1, p1_2, p1_3, p1_4, 
                               labels=c("(a1)", "(a2)", "(a3)", "(a4)"),
                               ncol=4, align="h", axis = "l",
                               label_x=c(0.86,0.6,0.6,0.6), label_y=0.95,
                               label_size = 18,
                               rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    
    #### GDAYP
    i <- "B_GDAYP"
    
    subDF <- plotDF2[plotDF2$ModName==i,]
    
    ### split the groups and make separate plots
    plotDF.sub1 <- subset(subDF, plot.cat2%in%c("A", "B", "C"))
    
    plotDF.sub2 <- subset(subDF, plot.cat2=="D")
    
    plotDF.sub3 <- subset(subDF, plot.cat2=="E")
    
    plotDF.sub4 <- subset(subDF, plot.cat2=="F")
    
    ### make plots
    p2_1 <- ggplot(plotDF.sub1,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("")         +
        #ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        ylab(" ") +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools]))))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        #scale_y_continuous(limits=c(-205, 600), 
        #                   breaks=c(-200, -100, 0, 100, 200, 400, 600),
        #                   labels=c(-200, -100, 0, 100, 200, 400, 600))+
        scale_y_continuous(limits=c(-50, 100), 
                           breaks=c(-50, 0, 25, 50, 75, 100),
                           labels=c(-50, 0, 25, 50, 75, 100))+
        guides(fill=guide_legend(ncol=3),legend.justification = c(0, 1))+
    annotate(geom="text", x=1, y=-100, label="GDAYP", size=7)
    
    
    p2_2 <- ggplot(plotDF.sub2,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-50, 100), 
                           breaks=c(-50, 0, 25, 50, 75, 100),
                           labels=c(-50, 0, 25, 50, 75, 100))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="b", size=7)
    
    
    p2_3 <- ggplot(plotDF.sub3,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("R"))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-50, 100), 
                           breaks=c(-50, 0, 25, 50, 75, 100),
                           labels=c(-50, 0, 25, 50, 75, 100))+
        guides(fill=guide_legend(ncol=2, nrow=3))#+
    #annotate(geom="text",x=0.5, y=500, label="c", size=7)
    
    
    p2_4 <- ggplot(plotDF.sub4,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-50, 100), 
                           breaks=c(-50, 0, 25, 50, 75, 100),
                           labels=c(-50, 0, 25, 50, 75, 100))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="d", size=7)
    
    
    plots_B_GDAYP <- plot_grid(p2_1, p2_2, p2_3, p2_4, 
                               labels=c("(b1)", "(b2)", "(b3)", "(b4)"),
                               ncol=4, align="h", axis = "l",
                               label_x=c(0.86,0.6,0.6,0.6), label_y=0.95,
                               label_size = 18,
                               rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    
    #### LPJGP
    i <- "C_LPJGP"
    
    subDF <- plotDF2[plotDF2$ModName==i,]
    
    ### split the groups and make separate plots
    plotDF.sub1 <- subset(subDF, plot.cat2%in%c("A", "B", "C"))
    
    plotDF.sub2 <- subset(subDF, plot.cat2=="D")
    
    plotDF.sub3 <- subset(subDF, plot.cat2=="E")
    
    plotDF.sub4 <- subset(subDF, plot.cat2=="F")
    
    ### make plots
    p3_1 <- ggplot(plotDF.sub1,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools]))))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=3),legend.justification = c(0, 1))+
    annotate(geom="text", x=1, y=-100, label="LPJGP", size=7)
    
    
    p3_2 <- ggplot(plotDF.sub2,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="b", size=7)
    
    
    p3_3 <- ggplot(plotDF.sub3,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("R"))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2, nrow=3))#+
    #annotate(geom="text",x=0.5, y=500, label="c", size=7)
    
    
    p3_4 <- ggplot(plotDF.sub4,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="d", size=7)
    
    
    plots_C_LPJGP <- plot_grid(p3_1, p3_2, p3_3, p3_4, 
                               labels=c("(c1)", "(c2)", "(c3)", "(c4)"),
                               ncol=4, align="h", axis = "l",
                               label_x=c(0.86,0.6,0.6,0.6), label_y=0.95,
                               label_size = 18,
                               rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    #### D_OCHDP
    i <- "D_OCHDP"
    
    subDF <- plotDF2[plotDF2$ModName==i,]
    
    ### split the groups and make separate plots
    plotDF.sub1 <- subset(subDF, plot.cat2%in%c("A", "B", "C"))
    
    plotDF.sub2 <- subset(subDF, plot.cat2=="D")
    
    plotDF.sub3 <- subset(subDF, plot.cat2=="E")
    
    plotDF.sub4 <- subset(subDF, plot.cat2=="F")
    
    ### make plots
    p4_1 <- ggplot(plotDF.sub1,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + 
        #ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        ylab(" ") +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools]))))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=3),legend.justification = c(0, 1))+
    annotate(geom="text", x=1, y=-100, label="OCHDP", size=7)
    
    
    p4_2 <- ggplot(plotDF.sub2,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="b", size=7)
    
    
    p4_3 <- ggplot(plotDF.sub3,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("R"))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2, nrow=3))#+
    #annotate(geom="text",x=0.5, y=500, label="c", size=7)
    
    
    p4_4 <- ggplot(plotDF.sub4,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="d", size=7)
    
    
    plots_D_OCHDP <- plot_grid(p4_1, p4_2, p4_3, p4_4, 
                               labels=c("(d1)", "(d2)", "(d3)", "(d4)"),
                               ncol=4, align="h", axis = "l",
                               label_x=c(0.86,0.6,0.6,0.6), label_y=0.95,
                               label_size = 18,
                               rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    
    #### E-QUINC
    i <- "E_QUINC"
    
    subDF <- plotDF2[plotDF2$ModName==i,]
    
    ### split the groups and make separate plots
    plotDF.sub1 <- subset(subDF, plot.cat2%in%c("A", "B", "C"))
    
    plotDF.sub2 <- subset(subDF, plot.cat2=="D")
    
    plotDF.sub3 <- subset(subDF, plot.cat2=="E")
    
    plotDF.sub4 <- subset(subDF, plot.cat2=="F")
    
    ### make plots
    p5_1 <- ggplot(plotDF.sub1,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + 
        #ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        ylab(" ") +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools]))))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=20),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=3),legend.justification = c(0, 1))+
    annotate(geom="text", x=1, y=-100, label="QUINC", size=7)
    
    
    p5_2 <- ggplot(plotDF.sub2,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=20),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="b", size=7)
    
    
    p5_3 <- ggplot(plotDF.sub3,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("R"))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=20),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2, nrow=3))#+
    #annotate(geom="text",x=0.5, y=500, label="c", size=7)
    
    
    p5_4 <- ggplot(plotDF.sub4,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=20),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="d", size=7)
    
    
    plots_E_QUINC <- plot_grid(p5_1, p5_2, p5_3, p5_4, 
                               labels=c("(e1)", "(e2)", "(e3)", "(e4)"),
                               ncol=4, align="h", axis = "l",
                               label_x=c(0.86,0.6,0.6,0.6), label_y=0.95,
                               label_size = 18,
                               rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    #### Legend
    legend_a <- get_legend(p1_1 + theme(legend.position="bottom",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    
    legend_b <- get_legend(p1_2 + theme(legend.position="bottom",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    legend_c <- get_legend(p1_3 + theme(legend.position="bottom",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    legend_d <- get_legend(p1_4 + theme(legend.position="bottom",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    
    plots_legend <- plot_grid(legend_a, legend_b, legend_c, legend_d, 
                               #labels=c("(a)", "(b)", "(c)", "(d)"),
                               ncol=4, align="h", axis = "l",
                              rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    ############################## Finalize the plot #############################
    ### Plotting
    pdf(paste0(out.dir, "/MIP_fate_of_carbon_CNP_models.pdf"), width=12, height=16)

    plot_grid(plots_A_CABLP,
              plots_B_GDAYP,
              plots_C_LPJGP,
              plots_D_OCHDP,
              plots_E_QUINC,
              plots_legend,
              ncol=1, rel_heights=c(1,1,1,1,1,0.4))
    
    dev.off()
        
    
    
    
    
    
    
    ###################### prepare individual models -microbial #########################
    #### ELMV1
    i <- "F_ELMV1"
    
    subDF <- plotDF2[plotDF2$ModName==i,]
    
    ### split the groups and make separate plots
    plotDF.sub1 <- subset(subDF, plot.cat2%in%c("A", "B", "C"))
    
    plotDF.sub2 <- subset(subDF, plot.cat2=="D")
    
    plotDF.sub3 <- subset(subDF, plot.cat2=="E")
    
    plotDF.sub4 <- subset(subDF, plot.cat2=="F")
    
    ### make plots
    p6_1 <- ggplot(plotDF.sub1,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + 
        #ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        ylab(" ") +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools]))))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2),legend.justification = c(0, 1))+
        annotate(geom="text", x=1, y=-100, label="ELMV1", size=7)
    
    
    p6_2 <- ggplot(plotDF.sub2,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="b", size=7)
    
    
    p6_3 <- ggplot(plotDF.sub3,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("R"))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2, nrow=3))#+
    #annotate(geom="text",x=0.5, y=500, label="c", size=7)
    
    
    p6_4 <- ggplot(plotDF.sub4,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="d", size=7)
    
    
    plots_F_ELMV1 <- plot_grid(p6_1, p6_2, p6_3, p6_4, 
                               labels=c("(a1)", "(a2)", "(a3)", "(a4)"),
                               ncol=4, align="h", axis = "l",
                               label_x=c(0.86,0.6,0.6,0.6), label_y=0.95,
                               label_size = 18,
                               rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    
    #### G-OCHDX
    i <- "G_OCHDX"
    
    subDF <- plotDF2[plotDF2$ModName==i,]
    
    ### split the groups and make separate plots
    plotDF.sub1 <- subset(subDF, plot.cat2%in%c("A", "B", "C"))
    
    plotDF.sub2 <- subset(subDF, plot.cat2=="D")
    
    plotDF.sub3 <- subset(subDF, plot.cat2=="E")
    
    plotDF.sub4 <- subset(subDF, plot.cat2=="F")
    
    ### make plots
    p7_1 <- ggplot(plotDF.sub1,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools]))))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=3),legend.justification = c(0, 1))+
        annotate(geom="text", x=1, y=-100, label="OCHDX", size=7)
    
    
    p7_2 <- ggplot(plotDF.sub2,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="b", size=7)
    
    
    p7_3 <- ggplot(plotDF.sub3,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("R"))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2, nrow=3))#+
    #annotate(geom="text",x=0.5, y=500, label="c", size=7)
    
    
    p7_4 <- ggplot(plotDF.sub4,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="d", size=7)
    
    
    plots_G_OCHDX <- plot_grid(p7_1, p7_2, p7_3, p7_4, 
                               labels=c("(b1)", "(b2)", "(b3)", "(b4)"),
                               ncol=4, align="h", axis = "l",
                               label_x=c(0.86,0.6,0.6,0.6), label_y=0.95,
                               label_size = 18,
                               rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    
    #### QUJSM
    i <- "H_QUJSM"
    
    subDF <- plotDF2[plotDF2$ModName==i,]
    
    ### split the groups and make separate plots
    plotDF.sub1 <- subset(subDF, plot.cat2%in%c("A", "B", "C"))
    
    plotDF.sub2 <- subset(subDF, plot.cat2=="D")
    
    plotDF.sub3 <- subset(subDF, plot.cat2=="E")
    
    plotDF.sub4 <- subset(subDF, plot.cat2=="F")
    
    ### make plots
    p8_1 <- ggplot(plotDF.sub1,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub1, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub1, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + 
        #ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        ylab(" ") +
        scale_x_discrete(labels=c("GPP", 
                                  expression(paste("NPP+", R[a])),
                                  expression(paste("R+",Delta*C[pools]))))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=14),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=3),legend.justification = c(0, 1))+
        annotate(geom="text", x=1, y=-100, label="QUJSM", size=7)
    
    
    p8_2 <- ggplot(plotDF.sub2,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub2, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub2, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("NPP", 
                                  "R",
                                  expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="b", size=7)
    
    
    p8_3 <- ggplot(plotDF.sub3,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub3, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub3, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c("R"))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2, nrow=3))#+
    #annotate(geom="text",x=0.5, y=500, label="c", size=7)
    
    
    p8_4 <- ggplot(plotDF.sub4,
                   aes(plot.cat2, value.mean)) +  
        geom_hline(yintercept=0)+
        geom_bar(stat = "identity", aes(fill=plot.cat),
                 position="stack") +
        #geom_errorbar(data=confDF.sub4, mapping=aes(x=plot.cat2, ymin=conf_low, ymax=conf_high), 
        #              width=0.1, size=0.6, color="black") + 
        #geom_point(data=confDF.sub4, mapping=aes(x=plot.cat2, y=effect_size), size=2, shape=21, fill="white")+
        xlab("") + ylab(expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")"))) +
        scale_x_discrete(labels=c(expression(Delta*C[pools])))+
        scale_fill_manual(name="", 
                          breaks = plotDF2$plot.cat,
                          values = col.list2,
                          labels=y.lab2) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=18),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        scale_y_continuous(limits=c(-205, 600), 
                           breaks=c(-200, -100, 0, 100, 200, 400, 600),
                           labels=c(-200, -100, 0, 100, 200, 400, 600))+
        guides(fill=guide_legend(ncol=2))#+
    #annotate(geom="text", x=0.5, y=500, label="d", size=7)
    
    
    plots_H_QUJSM <- plot_grid(p8_1, p8_2, p8_3, p8_4, 
                               labels=c("(c1)", "(c2)", "(c3)", "(c4)"),
                               ncol=4, align="h", axis = "l",
                               label_x=c(0.86,0.6,0.6,0.6), label_y=0.95,
                               label_size = 18,
                               rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    
    
    #### Legend
    legend_a <- get_legend(p6_1 + theme(legend.position="bottom",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    
    legend_b <- get_legend(p6_2 + theme(legend.position="bottom",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    legend_c <- get_legend(p6_3 + theme(legend.position="bottom",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    legend_d <- get_legend(p6_4 + theme(legend.position="bottom",
                                        legend.box = 'horizontal',
                                        legend.box.just = 'left'))
    
    
    plots_legend <- plot_grid(legend_a, legend_b, legend_c, legend_d, 
                              #labels=c("(a)", "(b)", "(c)", "(d)"),
                              ncol=4, align="h", axis = "l",
                              rel_widths=c(1.5,0.5,0.5,0.5))
    
    
    ############################## Finalize the plot #############################
    ### Plotting
    pdf(paste0(out.dir, "/MIP_fate_of_carbon_Microbial_models.pdf"), width=12, height=10)
    
    plot_grid(plots_F_ELMV1,
              plots_G_OCHDX,
              plots_H_QUJSM,
              plots_legend,
              ncol=1, rel_heights=c(1,1,1,0.4))
    
    dev.off()
    
    
    
    
    
    
    
    
    
    
    
    #  
}