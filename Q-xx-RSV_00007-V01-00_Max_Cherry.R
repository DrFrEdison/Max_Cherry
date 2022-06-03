# beverage parameter ####
setwd(this.path::this.dir())
dir( pattern = "Rsource" )
source.file <- print(dir( pattern = "Rsource" )[ length( dir( pattern = "Rsource" ))])
source( paste0(getwd(), "/", source.file) )

# spectra ####
setwd(dt$wd)
setwd("./Modellvalidierung")
setwd("./Produktionsdaten")

dt$para$files <- dir(pattern = ".csv$")
dt$para$txt <- .txt.file(dt$para$files)

dt$raw <- lapply(dt$para$files, \(x) fread(x, sep = ";", dec = ","))
names(dt$raw) <- dt$para$txt$type

dt$para$trs <- lapply(dt$raw, .transfer_csv.num.col)

dt$raw  <- mapply(function(x , y) y[ , c(1 : (min(x$numcol) - 1), x$numcol[ x$wl %in% dt$para$wl[[1]] ]), with = F]
                  , x = dt$para$trs
                  , y = dt$raw)

dt$para$trs <- lapply(dt$raw, .transfer_csv.num.col)

# validate drk ####
matplot(dt$para$trs$drk$wl
        , t(dt$raw$drk[ , dt$para$trs$drk$numcol, with = F])
        , lty = 1, type = "l")

dt$val$drk <- apply(dt$raw$drk[ , dt$para$trs$drk$numcol, with = F], 1, spectra.validation.drk)
unique(dt$val$drk)
dt$raw$spc <- dt$raw$spc[ spectra.validation.range(valid.vector = dt$val$drk
                                                    , drkref.datetime = dt$raw$drk$datetime
                                                    , spc.datetime = dt$raw$spc$datetime
                                                    , pattern = "invalid") , ]
dt$val$drk <- apply(dt$raw$drk[ , dt$para$trs$drk$numcol, with = F], 1, spectra.validation.drk)
dt$raw$spc <- dt$raw$spc[ spectra.validation.range(valid.vector = dt$val$drk
                                                    , drkref.datetime = dt$raw$drk$datetime
                                                    , spc.datetime = dt$raw$spc$datetime
                                                    , pattern = "empty") , ]

# validate ref ####
matplot(dt$para$trs$ref$wl
        , t(dt$raw$ref[ , dt$para$trs$ref$numcol, with = F])
        , lty = 1, type = "l")

# validate spc ####
matplot(dt$para$trs$spc$wl
        , t(dt$raw$spc[ , dt$para$trs$spc$numcol, with = F])
        , lty = 1, type = "l")

plot(dt$raw$spc$X279)
dt$raw$spc <- dt$raw$spc[ dt$raw$spc$X279 > .3 , ]

matplot(dt$para$trs$spc$wl
        , t(dt$raw$spc[ , dt$para$trs$spc$numcol, with = F])
        , lty = 1, type = "l")

# export clean spc csv ####
fwrite(dt$raw$spc
       , gsub("_spc.csv", "_spc_validated.csv", dt$para$files[ grep("_spc.csv", dt$para$files) ])
       , sep = ";", dec = ",")


