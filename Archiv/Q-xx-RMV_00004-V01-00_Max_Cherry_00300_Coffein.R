# beverage parameter ####
setwd(this.path::this.dir())
dir( pattern = "Rsource" )
source.file <- print("Rsource_Max_Cherry_val.R")
source( paste0(getwd(), "/", source.file) )

# spectra ####
dt$para$substance
dt$para$i = 2
dt$para$substance[dt$para$i]
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
dir.create(paste0("./", dt$para$val.date, "_", dt$para$model.pl[1], "_", dt$para$substance[dt$para$i]), showWarnings = F)
setwd(paste0("./", dt$para$val.date, "_", dt$para$model.pl[1], "_", dt$para$substance[dt$para$i]))
dir.create("Modellmatrix", showWarnings = F)
setwd("./Modellmatrix")

dt$model <- fread(print(dir(pattern = ".txt")), sep = ";", dec = ",")
dt$model <- transfer_csv(dt$model)

# Prediction
dt$pred <- lapply(dt$trs, function( x ) use_model_on_device(customer = dt$para$customer
                                                            , beverage = dt$para$beverage
                                                            , LG = dt_customer$LG[dt_customer$line == dt$para$line]
                                                            , parameter = dt$para$substance[dt$para$i]
                                                            , csv_transfered = x
                                                            , return_type = "prediction")
)

dt$pred.lin <- use_model_on_device(customer = dt$para$customer
                                   , beverage = dt$para$beverage
                                   , LG = dt_customer$LG[dt_customer$line == dt$para$line]
                                   , parameter = dt$para$substance[dt$para$i]
                                   , csv_transfered = dt$lin$trs
                                   , return_type = "prediction")

# Bias ####
dt$pred <- lapply(dt$pred, function( x ) ma( x , 5))
dt$bias <- lapply(dt$pred, function( x ) round( bias( median( x , na.rm = T), 0, dt$para$SOLL[ dt$para$i ] , 2), 3))
dt$pred <- mapply(function( x,y ) x + y, x = dt$pred, y = dt$bias, SIMPLIFY = F)

dt$bias.lin <- round( bias( mean( dt$pred.lin, na.rm = T), 0, mean(dt$lin$trs$data[ , grep( dt$para$substance[dt$para$i], colnames(dt$lin$trs$data) )])  ), 2)
dt$pred.lin <- dt$pred.lin - dt$bias.lin

par( mfrow = c(1,1))
plot(dt$pred.lin
     , xlab = "", ylab = dt$para$ylab[ dt$para$i ], main = dt$para$txt$loc.line[ dt$para$i ]
     , ylim = dt$para$SOLL[ dt$para$i] * c(85, 105) / 100, axes = T
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
  abline( h = dt$para$SOLL[ dt$para$i ] + dt$para$sperr[ dt$para$i ] * c(1, -1), col = "red", lty = 3, lwd = 1.5)
}

# Export for damn xlsx ####
for(i in 1:length(dt$pred)){
  
  
}

# Modell name ####
setwd(dt$wd)
setwd(paste0("./Mastermodell_", dt$para$model.pl))
dt$para$model.name <- grep(".unsb", grep( dt$para$substance[ dt$par$i ] , dir(), value = T), value = T)
dt$para$model.name <- gsub(".unsb", "", dt$para$model.name)

# Validation table copy ####
setwd(dt$wd)
setwd("./Modellvalidierung")
setwd(paste0("./", dt$para$val.date, "_", dt$para$model.pl[1], "_", dt$para$substance[dt$para$i]))

file.copy("D:/OneDrive - Dausch Technologies GmbH/Dokumentation/QM/06_FO_Formblaetter/FO-223-V01-0_Validierungstabelle_Mastermodell.xlsx"
          , dt$xlsx$file <- paste0(date(), "_Valdierung_", dt$para$customer, "_", dt$para$beverage, "_", dt$para$substance[ dt$para$i ], "_V01_00.xlsx")
          , overwrite = F)

# Validierungsinfo ####
dt$xlsx$info <- read.xlsx(dt$xlsx$file, sheet = "Validierungsinfo", colNames = F)
dt$xlsx$info$X1
dt$xlsx$info$X2 <- c(dt$para$location
                     , dt$para$line
                     , dt$para$beverage
                     , dt$para$beverage
                     , dt$para$substance[ dt$par$i ]
                     , ""
                     , date()
                     , dt$xlsx$file
                     , dt$para$model.name
                     , gsub("\\.", "\\,", as.character( dt$bias[[ i ]]) )
                     , "Ja"
                     , "Datum, Unterschrift")

dt$xlsx$wb <- loadWorkbook(dt$xlsx$file)
dt$xlsx$sheets <- print( sheets(dt$xlsx$wb) )

writeData(dt$xlsx$wb, "Validierungsinfo", dt$xlsx$info, colNames = F)
saveWorkbook(dt$xlsx$wb, dt$xlsx$file, overwrite = TRUE)

# Robustheit ####
dt$xlsx$wb <- loadWorkbook(dt$xlsx$file)
dt$xlsx$robustheit <- read.xlsx(dt$xlsx$file, sheet = "Robustheit", colNames = T)
dt$xlsx$robustheit <- read.xlsx(dt$xlsx$file, sheet = "Robustheit", colNames = T
                                , startRow = dt$xlsx$row <- (which ( dt$xlsx$robustheit[ , 1] == "Lfd. Nr.") + 1)
                                , skipEmptyCols = F)

dt$xlsx$maxrow <- nrow(dt$xlsx$robustheit)
dt$xlsx$robustheit <- dt$xlsx$robustheit[1:length( which( !is.na( dt$pred[[ i ]])) ),]

dt$xlsx$robustheit$Lfd..Nr. <- 1 : length( which( !is.na( dt$pred[[ i ]])) )
dt$xlsx$robustheit$date <- as.Date( dt$trs[[ i ]]$data$datetime, tz = "UTC")[ which( !is.na( dt$pred[[ i ]] ))]
dt$xlsx$robustheit$time <- strftime(dt$trs[[ i ]]$data$datetime, format = "%H:%M:%S", tz = "UTC")[ which( !is.na( dt$pred[[ i ]] ))]
dt$xlsx$robustheit$package <- ""

dt$xlsx$robustheit$`Liquiguard.[%]` <- dt$pred[[ i ]][ which( !is.na( dt$pred[[ i ]])) ]

dt$xlsx$robustheit$`Batch.(Sirup)` <- dt$para$Charge.Sirup
dt$xlsx$robustheit$`IBC.Batch-number` <- dt$para$Charge[ dt$para$i ]

dt$xlsx$robustheit$Target <- dt$para$SOLL[ dt$para$i ] 
dt$xlsx$robustheit[ , which( colnames( dt$xlsx$robustheit ) == "Target") - 1] <- dt$para$SOLL[ dt$para$i ] - dt$para$eingriff[ dt$para$i ]
dt$xlsx$robustheit[ , which( colnames( dt$xlsx$robustheit ) == "Target") + 1] <- dt$para$SOLL[ dt$para$i ] + dt$para$eingriff[ dt$para$i ]

dt$xlsx$robustheit <- rbind(dt$xlsx$robustheit
                            , dt$xlsx$robustheit[ (nrow(dt$xlsx$robustheit) + 1) : dt$xlsx$maxrow , ])

writeData(dt$xlsx$wb, "Robustheit", dt$xlsx$robustheit, colNames = F, startRow = dt$xlsx$row + 1)
saveWorkbook(dt$xlsx$wb, dt$xlsx$file, overwrite = TRUE)

# dt$xlsx$wb <- loadWorkbook(dt$xlsx$file)
# writeFormula(wb = dt$xlsx$wb
#              , sheet = "Robustheit"
#              , x = '=WENNS(UND(F11<>"";G11<>"");"yes"; UND(F11="";G11<>"");"noLG";UND(G11="";F11<>"");"noLab")'
#              , startCol = grep("yes", names(dt$xlsx$robustheit))
#              , startRow = dt$xlsx$row + 1)
# saveWorkbook(dt$xlsx$wb, dt$xlsx$file, overwrite = TRUE)