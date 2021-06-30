make_C_budget_response_ratio_plots <- function(inDF) {
    
    #########. Plot abs, no interactions, and change in pools, with covariates
    ### read in the csv file to plot the treatment effect and confidence interval
    myDF <- rbind(inDF$inout, inDF$npp, inDF$pool, inDF$delta_pool)
    
    myDF$diff_mean <- myDF$eCO2 - myDF$aCO2
    myDF$diff_sd <- sqrt((myDF$aCO2_sd^2 + myDF$eCO2_sd^2 ) / 2)
    
    myDF <- myDF[complete.cases(myDF$diff_mean),]
    
    ### Drop mycorrhizal production - inferred flux
    myDF <- subset(myDF, term != c("Mycorrhizal production"))
    
    myDF$term <- c("over_gpp", "understorey_gpp", "ch4",
                   "over_leaf_respiration", "wood_respiration", "root_respiration",
                   "understorey_respiration","voc", "herbivory_respiration",
                   "doc", "soil_respiration", "growth_respiration",
                   "leaf_prod", "wood_prod", "fineroot_prod",
                   "intermediate_root_prod", "coarse_root_prod",
                   "other_prod", "understorey_prod", 
                   "understorey_lit", "frass_prod", "herb_consump", 
                   "hetero_respiration", "over_leaf", "wood", "und_aboveground",
                   "fineroot", "intermediate_root","coarse_root",
                   "litter", "microbe",
                   "soil", "mycorrhizae", "insects", 
                   "delta_leaf_c", "delta_wood_c", "delta_understorey_c",
                   "delta_fineroot_c", "delta_intermediate_root_c", "delta_coarse_root_c",
                   "delta_litter_c","delta_microbial_c","delta_soil_c",
                   "delta_mycorrhizal_c", "delta_insect_c")
    
    #### exclude all pools
    myDF$Category <- c(rep("gpp", 2),  
                       rep("resp", 10), 
                       rep("prod", 7),
                       rep("litter", 2), 
                       rep("prod", 1), 
                       rep("resp", 1), 
                       rep("pool", 11), 
                       rep("change_in_pool", 11))  
    
    ### Drop redundant pools and fluxes
    myDF <- subset(myDF, term != c("understorey_lit"))
    
    myDF <- myDF[,c("term", "diff_mean", "diff_sd", "Category")]
    colnames(myDF) <- c("Variable", "effect_size", "sd", "Category")
    myDF$conf_low <- myDF$effect_size - myDF$sd
    myDF$conf_high <- myDF$effect_size + myDF$sd
    
    myDF[myDF$Variable=="ch4", "Category"] <- "gpp"
    
    
    plotDF1 <- subset(myDF, Category == "change_in_pool")
    plotDF2 <- subset(myDF, Category == "resp")
    plotDF3 <- subset(myDF, Category == "prod")
    plotDF4 <- subset(myDF, Category == "gpp")
    
    ### transform the data so that we could have break point on x axis
    #Function to transform data to y positions
    trans <- function(x) {
        if (x < 0) {
            pmax(x,-50) + 0.2*pmin(x+50,0)
        } else {
            pmin(x,50) + 0.2*pmax(x-50,0)
        }
    }
    
    xticks.brk <- xticks <- c(-200,-50, -25, 0, 25, 50, 200, 400)
    
    #Transform the data onto the display scale
    for (i in 1:length(plotDF1$Variable)) {
        plotDF1$effect_size_t[i] <- trans(plotDF1$effect_size[i])
        plotDF1$conf_low_t[i] <- trans(plotDF1$conf_low[i])
        plotDF1$conf_high_t[i] <- trans(plotDF1$conf_high[i])
    }   
    
    for (i in 1:length(plotDF2$Variable)) {
        plotDF2$effect_size_t[i] <- trans(plotDF2$effect_size[i])
        plotDF2$conf_low_t[i] <- trans(plotDF2$conf_low[i])
        plotDF2$conf_high_t[i] <- trans(plotDF2$conf_high[i])
    }  
    
    for (i in 1:length(plotDF3$Variable)) {
        plotDF3$effect_size_t[i] <- trans(plotDF3$effect_size[i])
        plotDF3$conf_low_t[i] <- trans(plotDF3$conf_low[i])
        plotDF3$conf_high_t[i] <- trans(plotDF3$conf_high[i])
    }  
    
    for (i in 1:length(plotDF4$Variable)) {
        plotDF4$effect_size_t[i] <- trans(plotDF4$effect_size[i])
        plotDF4$conf_low_t[i] <- trans(plotDF4$conf_low[i])
        plotDF4$conf_high_t[i] <- trans(plotDF4$conf_high[i])
    }  
    
    for (i in 1:length(xticks)) {
        xticks.brk[i] <- trans(xticks[i])
    }
    
    y.lab1 <- c("delta_soil_c"=expression(Delta*C[soil]),
                "delta_leaf_c"=expression(Delta*C[ol]),
                "delta_wood_c"=expression(Delta*C[stem]),
                "delta_fineroot_c"=expression(Delta*C[froot]),
                "delta_intermediate_root_c"=expression(Delta*C[iroot]),
                "delta_coarse_root_c"=expression(Delta*C[croot]),
                "delta_understorey_c"=expression(Delta*C[ua]),
                "delta_understorey_c_live"=expression(Delta*C[ua_live]),
                "delta_understorey_c_dead"=expression(Delta*C[ua_dead]),
                "delta_microbial_c"=expression(Delta*C[micr]),
                "delta_mycorrhizal_c"=expression(Delta*C[myco]),
                "delta_litter_c"=expression(Delta*C[lit]),
                "delta_insect_c"=expression(Delta*C[ins]))
    
    y.lab2 <- c("wood_respiration"=expression(R[stem]),
                "root_respiration"=expression(R[root]),
                "understorey_respiration"=expression(R[ua]),
                "herbivory_respiration"=expression(R[ins]),
                "soil_respiration"=expression(R[soil]),
                "doc"=expression(DOC),
                "voc"=expression(VC),
                "hetero_respiration"=expression(R[hetero]),
                "over_leaf_respiration"=expression(R[ol]),
                "frass_prod"=expression(Frass),
                "growth_respiration"=expression(R[grow]))
    
    y.lab3 <- c("herb_consump"=expression(NPP[ins]),
                #"mycorrhizal_prod"=expression(NPP[myco]),
                "lerp_prod"=expression(NPP[lerp]),
                "leaf_prod"=expression(NPP[ol]),
                "other_prod"=expression(NPP[other]),
                "wood_prod"=expression(NPP[stem]),
                "fineroot_prod"=expression(NPP[froot]),
                "intermediate_root_prod"=expression(NPP[iroot]),
                "coarse_root_prod"=expression(NPP[croot]),
                "understorey_prod"=expression(NPP[ua]))
    
    y.lab4 <- c("over_gpp"=expression(GPP[o]),
                "understorey_gpp"=expression(GPP[u]),
                "ch4"=expression(CH[4]))
    
    
    ### add conditional color to data frames
    plotDF1$col.con <- ifelse(plotDF1$effect_size_t<0, "#DF6747", "#37AFA9")
    plotDF2$col.con <- ifelse(plotDF2$effect_size_t<0, "#DF6747", "#37AFA9")
    plotDF3$col.con <- ifelse(plotDF3$effect_size_t<0, "#DF6747", "#37AFA9")
    plotDF4$col.con <- ifelse(plotDF4$effect_size_t<0, "#DF6747", "#37AFA9")
    
    #### Plotting
    p8 <- ggplot(plotDF4)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color=plotDF4$col.con)+
        labs(x="eC minus aC", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        geom_vline(xintercept = 0.0)+
        geom_vline(xintercept = -50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = -55, linetype="dashed", color="grey")+
        geom_vline(xintercept = 50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = 55, linetype="dashed", color="grey")+
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab4)
    
    
    p5 <- ggplot(plotDF1)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color=plotDF1$col.con)+
        #scale_color_manual(name="Treatment significance", 
        #                   labels = c("Non-sig", "Sig (P<0.1)"), 
        #                   values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
        #scale_shape_manual(name="Date Significance", 
        #                   labels = c("Non-sig", "Sig (P<0.05)"), 
        #                   values = c(1, 2))+ 
        labs(x="eC minus aC", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        geom_vline(xintercept = 0.0)+
        geom_vline(xintercept = -50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = -55, linetype="dashed", color="grey")+
        geom_vline(xintercept = 50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = 55, linetype="dashed", color="grey")+
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab1)
    
    p6 <- ggplot(plotDF3)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color=plotDF3$col.con)+
        labs(x="eC minus aC", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        geom_vline(xintercept = 0.0)+
        geom_vline(xintercept = -50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = -55, linetype="dashed", color="grey")+
        geom_vline(xintercept = 50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = 55, linetype="dashed", color="grey")+
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab3)
    
    p7 <- ggplot(plotDF2)+ 
        geom_segment(aes(y=reorder(Variable, effect_size_t), x=conf_low_t, 
                         yend=reorder(Variable, effect_size_t), xend=conf_high_t), 
                     size=6, color="grey")+
        geom_point(aes(y=reorder(Variable, effect_size_t), x=effect_size_t), 
                   stat='identity', size=4, shape=19, color=plotDF2$col.con)+
        scale_color_manual(name="Treatment significance", 
                           labels = c("Non-sig", "Sig (P<0.1)"), 
                           values = c("non-sig"="#f8766d","sig"="#00ba38"))+ 
        labs(x=expression(paste(CO[2], " effect (g C ", m^-2, " ", yr^-1, ")")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom")+
        geom_vline(xintercept = 0.0)+
        geom_vline(xintercept = -50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = -55, linetype="dashed", color="grey")+
        geom_vline(xintercept = 50, linetype="dashed", color="grey")+
        #geom_vline(xintercept = 55, linetype="dashed", color="grey")+
        #geom_rect(aes(xmin=-50, xmax=-40, ymin=0, ymax=18), fill="white")+
        scale_x_continuous(limits=c(min(xticks.brk), max(xticks.brk)), breaks=xticks.brk, labels=xticks)+
        scale_y_discrete(labels=y.lab2)
    
    
    pdf("output/ED_Figure_5.pdf", width=8, height=12)
    require(cowplot)    
    plot_grid(p8, p5, 
              p6, p7, 
              labels="auto", ncol=1, align="v", axis = "l",
              rel_heights=c(0.3,1,1,1))
    dev.off()
    
}