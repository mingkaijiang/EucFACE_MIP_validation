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


col.values <- c("A_GDAYP" = set3Palette[1],#SpectralPalette[1],
                "B_ELMV1" = set3Palette[2],#SpectralPalette[3],
                "C_CABLP" = set3Palette[3],#SpectralPalette[5],
                "D_LPJGP" = set3Palette[4],#SpectralPalette[7],
                "E_OCHDP" = set3Palette[5],#SpectralPalette[9],
                "F_QUINC" = set3Palette[6],#SpectralPalette[11],
                "G_OCHDX" = set3Palette[7],#SpectralPalette[12],
                "H_QUJSM" = set3Palette[8],#SpectralPalette[11],
                "I_GDAYN" = set3Palette[1],#SpectralPalette[1],
                "J_LPJGN" = set3Palette[4])#,#SpectralPalette[4],
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
