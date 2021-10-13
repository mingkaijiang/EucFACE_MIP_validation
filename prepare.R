#### prepare all necessary global settings
#### Create data folder
output.folders <- c(paste0(getwd(), "/output"),
                    paste0(getwd(), "/output/QC"),
                    paste0(getwd(), "/output/analysis_output"),
                    paste0(getwd(), "/output/MIP_output"),
                    paste0(getwd(), "/output/MIP_output/obs_fix_output"),
                    paste0(getwd(), "/output/MIP_output/obs_var_output"))


#### Create output folder
for (y in output.folders) {
    if(!dir.exists(y)) {
        dir.create(y, showWarnings = FALSE)
    }
}


#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               readxl,
               lubridate,
               ggplot2,
               knitr,
               cowplot,
               viridis,
               sciplot,
               RColorBrewer,
               ncdf4,
               plantecophys,
               lattice,
               reshape2,
               tinytex,
               qdap,
               data.table,
               gridExtra,
               grid)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)


#### color palette:
model.names <- c("A_GDAYP",
                 "B_ELMV1",
                 "C_CABLP",
                 "D_LPJGP",
                 "E_OCHDP",
                 "F_QUINC",
                 "G_OCHDX",
                 "H_QUJSM",
                 "I_GDAYN",
                 "J_LPJGN")#,
                 #"K_CABLP-VD",
                 #"L_LPJGP-VD")


obs.color <- c("#000000") # black

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set3Palette <- brewer.pal(n = 10, name = "Set3")

YlOrRdPalette <- rev(brewer.pal(n = 9, name = "YlOrRd"))

GreensPalette <- rev(brewer.pal(n = 9, name = "Greens"))

SpectralPalette <- brewer.pal(n = 8, name = "Spectral")


#col.values <- c("A_CABLP" = YlOrRdPalette[1],
#                "B_GDAYP" = YlOrRdPalette[3],
#                "C_LPJGP" = YlOrRdPalette[5],
#                "D_OCHDP" = YlOrRdPalette[7],
#                "E_QUINC" = "Yellow",
#                "F_ELMXX" = GreensPalette[1],
#                "G_OCHDX" = GreensPalette[3],
#                "H_QUJSM" = GreensPalette[5],
#                "I_GDAYN" = YlOrRdPalette[3],
#                "J_LPJGN" = YlOrRdPalette[5],
#                "K_CABLP-VD" = YlOrRdPalette[1],
#                "L_LPJGP-VD" = YlOrRdPalette[5])


#col.values <- c("A_GDAYP" = set3Palette[1],
#                "B_ELMV1" = set3Palette[2],
#                "C_CABLP" = set3Palette[3],
#                "D_LPJGP" = set3Palette[4],
#                "E_OCHDP" = set3Palette[5],
#                "F_QUINC" = set3Palette[6],
#                "G_OCHDX" = set3Palette[7],
#                "H_QUJSM" = set3Palette[8],
#                "I_GDAYN" = set3Palette[1],
#                "J_LPJGN" = set3Palette[4])

col.values <- c("A_GDAYP" = SpectralPalette[1],
                "B_ELMV1" = SpectralPalette[2],
                "C_CABLP" = SpectralPalette[3],
                "D_LPJGP" = SpectralPalette[4],
                "E_OCHDP" = SpectralPalette[5],
                "F_QUINC" = SpectralPalette[6],
                "G_OCHDX" = SpectralPalette[7],
                "H_QUJSM" = SpectralPalette[8],
                "I_GDAYN" = SpectralPalette[1],
                "J_LPJGN" = SpectralPalette[4])#,
#"K_CABLP-VD" = SpectralPalette[3],
#"L_LPJGP-VD" = SpectralPalette[4])

linetype.values <- c("A_GDAYP" = 1,
                     "B_ELMV1" = 1,
                     "C_CABLP" = 1,
                     "D_LPJGP" = 1,
                     "E_OCHDP" = 1,
                     "F_QUINC" = 1,
                     "G_OCHDX" = 1,
                     "H_QUJSM" = 1,
                     "I_GDAYN" = 2,
                     "J_LPJGN" = 2)#,
                     #"K_CABLP-VD" = 3,
                     #"L_LPJGP-VD" = 3)

model.labels <- c("A_GDAYP" = "GDAYP",
                  "B_ELMV1" = "ELMV1",
                  "C_CABLP" = "CABLP",
                  "D_LPJGP" = "LPJGP",
                  "E_OCHDP" = "OCDHP",
                  "F_QUINC" = "QUINC",
                  "G_OCHDX" = "OCHDX",
                  "H_QUJSM" = "QUJSM",
                  "I_GDAYN" = "GDAYN",
                  "J_LPJGN" = "LPJGN")#,
                  #"K_CABLP-VD" = "CABLE-VD",
                  #"L_LPJGP-VD" = "LPJGP-VD")





#### end
