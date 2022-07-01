# beverage parameter ####
setwd(this.path::this.dir())
dir( pattern = "_val_" )
source.file <- "Rsource_Max_Cherry_mtx_mop_val_V01.R"
source( paste0(getwd(), "/", source.file) )

# para ####
dt$para$substance
dt$para$i = 4
dt$para$substance[dt$para$i]

# keep out ####
keep.out.unsb.model(customer = dt$para$customer
                    , beverage = dt$para$beverage
                    , LG = dt_customer$LG[which(dt_customer$line == as.character(dt$para$line))]
                    , parameter = dt$para$substance[dt$para$i])


# spectra ####
setwd(dt$wd)
setwd("./Modellvalidierung")
setwd("./Produktionsdaten")

dt$para$files <- dir(pattern = "validated.csv$")
dt$para$txt <- txt.file(dt$para$files)

dt$raw <- lapply(dt$para$files, \(x) fread(x, sep = ";", dec = ","))
names(dt$raw) <- dt$para$txt$type

dt$para$trs <- lapply(dt$raw, transfer_csv.num.col)
dt$trs <- lapply(dt$raw, transfer_csv)

# Modellmatrix ####
setwd(dt$wd)
setwd("./Modellvalidierung")
dir.create(paste0("./", dt$para$val.date, "_", dt$para$model.raw.pl[1], "_", dt$para$substance[dt$para$i]), showWarnings = F)
setwd(paste0("./", dt$para$val.date, "_", dt$para$model.raw.pl[1], "_", dt$para$substance[dt$para$i]))
dir.create("Modellmatrix", showWarnings = F)
setwd("./Modellmatrix")

# Prediction
dt$pred <- lapply(dt$trs, function( x ) use_model_on_device(customer = dt$para$customer
                                                            , beverage = dt$para$beverage
                                                            , LG = dt_customer$LG[which(dt_customer$line == as.character(dt$para$line))]
                                                            , parameter = dt$para$substance[dt$para$i]
                                                            , csv_transfered = x
                                                            , return_type = "prediction")
)

dt$pred.lin <- use_model_on_device(customer = dt$para$customer
                                   , beverage = dt$para$beverage
                                   , LG = dt_customer$LG[which(dt_customer$line == as.character(dt$para$line))]
                                   , parameter = dt$para$substance[dt$para$i]
                                   , csv_transfered = dt$lin$trs
                                   , return_type = "prediction")

# Bias ####
dt$pred <- mapply(function( x, datetime ) ma.date( x = x , time = datetime$data$datetime, diff.time.max = 1200, n = 5)
                  , x = dt$pred
                  , datetime = dt$trs
                  , SIMPLIFY = F)
dt$bias <- print( lapply(dt$pred, function( x ) round( bias( median( x , na.rm = T), 0, dt$para$SOLL[ dt$para$i ] , 2), 3)) )
dt$pred <- mapply(function( x,y ) x + y, x = dt$pred, y = dt$bias, SIMPLIFY = F)

dt$bias.lin <- round( bias( mean( dt$pred.lin, na.rm = T), 0, mean(dt$lin$trs$data[ , grep( dt$para$substance[dt$para$i], colnames(dt$lin$trs$data) )])  ), 2)
dt$pred.lin <- dt$pred.lin - dt$bias.lin

par( mfrow = c(1,1))
plot(dt$pred.lin
     , xlab = "", ylab = dt$para$ylab[ dt$para$i ], main = dt$para$txt$loc.line[ dt$para$i ]
     , ylim = dt$para$SOLL[ dt$para$i] * c(85, 115) / 100, axes = T
     , sub = paste("Bias =", dt$bias))
points(dt$lin$trs$data[ , grep( dt$para$substance[dt$para$i], colnames(dt$lin$trs$data) )], col = "red")

par(mfrow = c(length( dt$pred ), 1))
for(i in 1:length(dt$pred)){
  plot(dt$pred[[ i ]]
       , xlab = "", ylab = dt$para$ylab[ dt$para$i ], main = dt$para$txt$loc.line[ i ]
       , ylim = dt$para$SOLL[ dt$para$i] * c(95, 105) / 100, axes = F
       , sub = paste("Bias =", dt$bias[ i ]))
  xaxisdate(dt$trs[[ i ]]$data$datetime)
  abline( h = dt$para$SOLL[ dt$para$i ], col = "darkgreen", lty = 3, lwd = 1.5)
  abline( h = dt$para$SOLL[ dt$para$i ] + dt$para$eingriff[ dt$para$i ] * c(1, -1), col = "orange", lty = 3, lwd = 1.5)
  # abline( h = dt$para$SOLL[ dt$para$i ] + dt$para$sperr[ dt$para$i ] * c(1, -1), col = "red", lty = 3, lwd = 1.5)
}

# Export for damn xlsx ####
# Modell name ####
setwd(dt$wd)
setwd(paste0("./Mastermodell_", dt$para$model.raw.pl))
dt$para$model.name <- grep("41", grep(  dt$para$substance[dt$para$i] , dir(), value = T), value = T)
dt$para$model.name
dt$para$model.name <- dt$para$model.name[length(dt$para$model.name)]
dt$para$model.name <- gsub(".41M", "", dt$para$model.name)
dt$para$model.name <- gsub( "-00", "-01", dt$para$model.name)

# Validation table copy ####
setwd(dt$wd)
setwd("./Modellvalidierung")
setwd(paste0("./", dt$para$val.date, "_", dt$para$model.raw.pl[1], "_", dt$para$substance[dt$para$i]))

dir( paste0(dt$wd, "/", "Modellvalidierung/", dt$para$val.date, "_", dt$para$model.raw.pl[1], "_", dt$para$substance[2]) )
dir(path = "D:/OneDrive - Dausch Technologies GmbH/Dokumentation/QM/06_FO_Formblaetter/", pattern = "FO-223")

file.copy(paste0(dt$wd, "/", "Modellvalidierung/", dt$para$val.date, "_", dt$para$model.raw.pl[1], "_", dt$para$substance[2], "/"
                 , "220701_Validierung_PepsiCo_Max_Cherry_Coffein_00300_V01_01.xlsx")
          , dt$xlsx$file <- paste0(date(), "_Validierung_", dt$para$customer, "_", dt$para$beverage, "_", dt$para$substance[ dt$para$i ], "_", dt$para$model.raw.pl
                                   , "_V01_01.xlsx")
          , overwrite = F)

# Validierungsinfo ####
dt$xlsx$info <- read.xlsx(dt$xlsx$file, sheet = "Validation_info", colNames = F)
dt$xlsx$info$X1
dt$xlsx$info$X2 <- c(dt$para$location
                     , dt$para$line
                     , dt$para$beverage
                     , dt$para$beverage
                     , dt$para$substance[ dt$par$i ]
                     , gsub('"', '', dt$para$unit[ dt$para$i ])
                     , ""
                     , date()
                     , dt$xlsx$file
                     , dt$para$model.name
                     , gsub("\\.", "\\,", as.character( dt$bias[[ i ]]) )
                     , "Ja"
                     , "Date, Signature")

dt$xlsx$wb <- loadWorkbook(dt$xlsx$file)
dt$xlsx$sheets <- print( sheets(dt$xlsx$wb) )

writeData(dt$xlsx$wb, "Validation_info", dt$xlsx$info, colNames = F)
saveWorkbook(dt$xlsx$wb, dt$xlsx$file, overwrite = TRUE)

# Robustness ####
dt$xlsx$wb <- loadWorkbook(dt$xlsx$file)
dt$xlsx$Robustness <- read.xlsx(dt$xlsx$file, sheet = "Robustness", colNames = T, skipEmptyRows = F, skipEmptyCols = F)
dt$xlsx$Robustness <- read.xlsx(dt$xlsx$file, sheet = "Robustness", colNames = T
                                , startRow = dt$xlsx$row <- (which ( dt$xlsx$Robustness[ , 1] == "Nr.") + 1)
                                , skipEmptyCols = F, skipEmptyRows = F)

dt$xlsx$maxrow <- nrow(dt$xlsx$Robustness)
dt$xlsx$Robustness <- dt$xlsx$Robustness[1:length( which( !is.na( dt$pred[[ i ]])) ),]

dt$xlsx$Robustness$Nr. <- 1 : length( which( !is.na( dt$pred[[ i ]])) )
dt$xlsx$Robustness$date <- as.Date( dt$trs[[ i ]]$data$datetime, tz = "UTC")[ which( !is.na( dt$pred[[ i ]] ))]
dt$xlsx$Robustness$time <- strftime(dt$trs[[ i ]]$data$datetime, format = "%H:%M:%S", tz = "UTC")[ which( !is.na( dt$pred[[ i ]] ))]

# LG values
dt$xlsx$Robustness[ , grep("time", colnames(dt$xlsx$Robustness)) + 2] <- as.numeric(dt$pred[[ i ]][ which( !is.na( dt$pred[[ i ]])) ])

# Sirup
dt$xlsx$Robustness$`Batch.(Sirup)` <- ifelse(is.null(dt$para$Charge.Sirup), NA, dt$para$Charge.Sirup)
dt$xlsx$Robustness$`IBC.Batch-number` <- ifelse(is.null(dt$para$Charge[ dt$para$i ]), NA, dt$para$Charge[ dt$para$i ])

dt$xlsx$Robustness$Target <- dt$para$SOLL[ dt$para$i ]
head(dt$xlsx$Robustness)
dt$xlsx$Robustness[ , which( colnames( dt$xlsx$Robustness ) == "Target") - 1] <- dt$para$eingriff[ 1, dt$para$i ]
dt$xlsx$Robustness[ , which( colnames( dt$xlsx$Robustness ) == "Target") + 1] <- dt$para$eingriff[ 2, dt$para$i ]

writeData(dt$xlsx$wb, "Robustness", dt$xlsx$Robustness, colNames = F, startRow = dt$xlsx$row + 1)
saveWorkbook(dt$xlsx$wb, dt$xlsx$file, overwrite = TRUE)
# =WENNS(UND(F11<>"";G11<>"");"yes"; UND(F11="";G11<>"");"noLG";UND(G11="";F11<>"");"noLab")
# =WENN(UND(G11>0; F11>0);G11-F11;"")

# Linearity ####
dt$xlsx$lin <- data.frame(date = tapply(dt$lin$trs$data$date, factor( dt$lin$trs$data$Dilution ), function( x ) unique( as.character( x))[ 1 ])
                          , time = tapply(dt$lin$trs$data$time, factor( dt$lin$trs$data$Dilution ), function( x ) unique( as.character( x))[ 1 ])
                          , package = ""
                          , lab_yes_no = NA
                          , LG = tapply(dt$pred.lin, factor( dt$lin$trs$data$Dilution ), mean)
                          , Lab = tapply(dt$lin$trs$data[ , grep( dt$para$substance[dt$para$i], colnames(dt$lin$trs$data) )]
                                         , factor( dt$lin$trs$data$Dilution ), mean))

dt$xlsx$Linearity <- read.xlsx(dt$xlsx$file, sheet = "Linearity", colNames = T, skipEmptyRows = F, skipEmptyCols = F)
dt$xlsx$Linearity <- read.xlsx(dt$xlsx$file, sheet = "Linearity", colNames = T
                               , startRow = dt$xlsx$row <- (which ( dt$xlsx$Linearity[ , 1] == "Nr.") + 1)
                               , skipEmptyCols = F, skipEmptyRows = F)
dt$xlsx$Linearity$Nr. <- 1:nrow(dt$xlsx$lin)
dt$xlsx$Linearity$date <- dt$xlsx$lin$date
dt$xlsx$Linearity$time <- dt$xlsx$lin$time

dt$xlsx$Linearity[ , grep("time", colnames(dt$xlsx$Linearity)) + 2] <- dt$xlsx$lin$LG
dt$xlsx$Linearity[ , grep("time", colnames(dt$xlsx$Linearity)) + 3] <- dt$xlsx$lin$Lab

writeData(dt$xlsx$wb, "Linearity", dt$xlsx$Linearity, colNames = F, startRow = dt$xlsx$row + 1)
saveWorkbook(dt$xlsx$wb, dt$xlsx$file, overwrite = TRUE)
