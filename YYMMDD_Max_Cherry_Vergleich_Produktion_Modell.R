# beverage parameter ####
setwd(this.path::this.dir())
dir( pattern = "Rsource" )
source.file <- print(dir( pattern = "Rsource" )[ length( dir( pattern = "Rsource" ))])
source( paste0(getwd(), "/", source.file) )

# Compare production spectra with model spectra
setwd(dt$wd)
setwd("./Modellerstellung")
setwd(paste0("./", dt$para$model.date[1], "_", dt$para$model.pl[1]))
setwd("./csv")

dir()
bev <- list()
bev$raw$prod <- fread("220101_220524_Nieder_Roden_L3_PET_CSD_Max Cherry_15_spc.csv", sep = ";", dec = ",")
bev$raw$prod <- bev$raw$prod[ bev$raw$prod$X300 > .1 , ]
bev$raw$prod <- bev$raw$prod[ round(seq(1, nrow(bev$raw$prod), len = 100), 0) , ]

# bev$raw$Ausmischung <- read.csv2("220413_Mezzo_Mix_Zero_Modellspektren_Ausmischung_match.csv")
# bev$raw$Ausmischung <- bev$raw$Ausmischung[ bev$raw$Ausmischung$Probe_Anteil != "SL" , ]

# bev$raw$altes.model <- read.csv2("220413_Mezzo_Mix_Zero_Modellspektren_Ausmischung_match.csv")
# bev$raw$altes.model <- bev$raw$altes.model[ bev$raw$altes.model$Probe_Anteil != "SL" , ]

bev$trs <- lapply(bev$raw, function(x) .transfer_csv(x))

png(paste0(.date(),"_", dt$para$beverage, "_Spektrenvergleich.png"),xxx<-4800,xxx/16*9,"px",12,"white",res=500,"sans",T,"cairo")

par(mfrow = c(2,1), mar = c(4,5,1,1))

matplot( bev$trs$Ausmischung$wl
        , t(bev$trs$Ausmischung$spc)
        , type = "l", lty = 1, col = "red", xlab = .lambda, ylab = "AU", xlim = c(200, 450), ylim = c(0, 1))

matplot( bev$trs$prod$wl
        , t(bev$trs$prod$spc)[ ]
        , type = "l", lty = 1, col = "darkgreen", add = F)

matplot( bev$trs$altes.model$wl
        , t(bev$trs$altes.model$spc)[ ]
        , type = "l", lty = 1, col = "blue", add = T)

legend("topright", c("Ausmischung"
                     , "Produktion"
                     , "Altes Modell"
                     )
       , lty = 1, col = c("red"
                          , "darkgreen"
                          , "blue"
                          ), xpd = F)

matplot(bev$trs$Ausmischung$wl
        , t(bev$trs$Ausmischung$spc1st)
        , type = "l", lty = 1, col = "red", xlab = .lambda, ylab = .ylab_1st, xlim = c(200, 450), ylim = c(-.05, 0.02))

matplot(bev$trs$prod$wl
        , t(bev$trs$prod$spc1st)[]
        , type = "l", lty = 1, col = "darkgreen", add = T)

matplot( bev$trs$altes.model$wl
         , t(bev$trs$altes.model$spc1st)[ ]
         , type = "l", lty = 1, col = "blue", add = T)

legend("topright", c("Ausmischung"
                     , "Produktion"
                     , "Altes Modell"
)
, lty = 1, col = c("red"
                   , "darkgreen"
                   , "blue"
), xpd = F)
dev.off()
