#### prepare all necessary global settings
#### Create data folder
output.folders <- c(paste0(getwd(), "/validation_output"),
                    paste0(getwd(), "/analysis_output"),
                    paste0(getwd(), "/obs_var_output"),
                    paste0(getwd(), "/obs_fix_output"))


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
               gridExtra)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)


#### color palette:
model.names <- c("A_CABLP",
                 "B_GDAYP",
                 "C_LPJGP",
                 "D_OCHDP",
                 "E_QUINC",
                 "F_ELMXX",
                 "G_OCHDX",
                 "H_QUJSM",
                 "I_GDAYN",
                 "J_LPJGN",
                 "K_CABLP-VD",
                 "L_LPJGP-VD")


obs.color <- c("#000000") # black

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set3Palette <- brewer.pal(n = 12, name = "Set3")

YlOrRdPalette <- rev(brewer.pal(n = 9, name = "YlOrRd"))

GreensPalette <- rev(brewer.pal(n = 9, name = "Greens"))

SpectralPalette <- brewer.pal(n = 11, name = "Spectral")


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


col.values <- c("A_CABLP" = SpectralPalette[1],
                "B_GDAYP" = SpectralPalette[2],
                "C_LPJGP" = SpectralPalette[3],
                "D_OCHDP" = SpectralPalette[4],
                "E_QUINC" = SpectralPalette[5],
                "F_ELMXX" = SpectralPalette[9],
                "G_OCHDX" = SpectralPalette[10],
                "H_QUJSM" = SpectralPalette[11],
                "I_GDAYN" = SpectralPalette[2],
                "J_LPJGN" = SpectralPalette[3],
                "K_CABLP-VD" = SpectralPalette[1],
                "L_LPJGP-VD" = SpectralPalette[3])

linetype.values <- c("A_CABLP" = 1,
                     "B_GDAYP" = 1,
                     "C_LPJGP" = 1,
                     "D_OCHDP" = 1,
                     "E_QUINC" = 1,
                     "F_ELMXX" = 1,
                     "G_OCHDX" = 1,
                     "H_QUJSM" = 1,
                     "I_GDAYN" = 2,
                     "J_LPJGN" = 2,
                     "K_CABLP-VD" = 3,
                     "L_LPJGP-VD" = 3)

model.labels <- c("A_CABLP" = "CABLP",
                  "B_GDAYP" = "GDAYP",
                  "C_LPJGP" = "LPJGP",
                  "D_OCHDP" = "OCDHP",
                  "E_QUINC" = "QUINC",
                  "F_ELMXX" = "ELMXX",
                  "G_OCHDX" = "OCHDX",
                  "H_QUJSM" = "QUJSM",
                  "I_GDAYN" = "GDAYN",
                  "J_LPJGN" = "LPJGN",
                  "K_CABLP-VD" = "CABLE-VD",
                  "L_LPJGP-VD" = "LPJGP-VD")





#### end
