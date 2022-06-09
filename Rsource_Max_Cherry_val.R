library(devtools); suppressMessages(install_github("DrFrEdison/r4dt", dependencies = T) ); library(r4dt); dt <- list()

# general parameters ####
dt$para$customer = "PepsiCo"
dt$para$beverage = "Max_Cherry"

setwd(paste0(dt$wd <- paste0(wd$fe$Pepsi$Mastermodelle, dt$para$beverage)))
setwd( print( this.path::this.dir() ) )
setwd("..")
dt$wd.git <- print( getwd() )

dt$para$location = "Nieder_Roden"
dt$para$line = "L3_PET_CSD"
dt$para$main = paste0(dt$para$beverage, " in ", dt$para$location, ", line ", dt$para$line)
dt$para$model.date <- c("220524")
dt$para$model.pl <- c("00300")
dt$para$wl1 <- c(190)
dt$para$wl2 <- c(598)
dt$para$wl[[1]] <- seq(dt$para$wl1, dt$para$wl2, 1)

dt$para$substance <- c("TA",	"Coffein",	"Aspartam",	"Acesulfam")
dt$para$unit <- c( bquote("mg L"-1),  bquote("mg L"-1),  bquote("mg L"-1),  bquote("mg L"-1) )
dt$para$ylab <- c(bquote("TA in mg / 100mL"^-1), bquote("Coffein in mg / L"^-1), bquote("Aspartam in mg / L"^-1), bquote("Acesulfam in mg / L"^-1))
dt$para$mop.date <- "220522"
dt$para$SOLL <- c(14.15, 119.84, 595.01, 49.9)

dt$para$val.date <- "220522"

# Linearity
setwd(dt$wd)
setwd("./Modellvalidierung")
setwd("./Linearitaet")
dt$lin$raw <- read.csv2( "220602_Max_Cherry_Linearitaet_TA_Acesulfam_Aspartam_Coffein.csv" , sep = "\t")
dt$lin$raw <- dt$lin$raw[ order(dt$lin$raw$Dilution) , ]
dt$lin$trs <- transfer_csv(dt$lin$raw)

dt$para$eingriff <- c(.2, 8, NA, NA)
dt$para$sperr <- c(.4, 10, NA, NA)

dt$para$Charge <- c("22-8900-0003", "22-8900-0010", "22-8900-0003", "22-8900-0003")
dt$para$Charge.Sirup <- "2214000938"
# rename R files (run only once)
# dt$para$Rfiles <- list.files(getwd(), pattern = ".R$", recursive = T)
# file.rename(dt$para$Rfiles, gsub("beverage", dt$para$beverage, dt$para$Rfiles))

