#### prepare
#### Create data folder
#output.folders <- c("outputs",
#                    "outputs")

#### Create output folder
#for (y in output.folders) {
#    if(!dir.exists(y)) {
#        dir.create(y, showWarnings = FALSE)
#    }
#}


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
               tinytex)    

#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("scripts", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)
