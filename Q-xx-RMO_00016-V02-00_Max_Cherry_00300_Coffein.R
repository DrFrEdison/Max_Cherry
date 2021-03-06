# beverage parameter ####
setwd(this.path::this.dir())
dir( pattern = "Rsource" )
source.file <- print("Rsource_Max_Cherry_mtx_mop_val_V01.R")
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

dt$raw <- lapply(dt$para$files, \( x ) fread(x, sep = ";", dec = ","))
lapply(dt$raw, nrow)
# dt$raw <- lapply(dt$raw, \( x ) x[ seq(1, nrow(x), 3) , ])
names(dt$raw) <- dt$para$txt$loc.line

dt$trsnumcol <- lapply(dt$raw, transfer_csv.num.col)
dt$trs <- lapply(dt$raw, transfer_csv)

# Write model file into Modellmatrix
setwd(dt$wd)
setwd("./Modelloptimierung")
dir()
dir.create(paste0("./", dt$para$mop.date, "_", dt$para$model.raw.pl[1], "_", dt$para$substance[dt$para$i]), showWarnings = F)
setwd(paste0("./", dt$para$mop.date, "_", dt$para$model.raw.pl[1], "_", dt$para$substance[dt$para$i]))
dir.create("Modellmatrix", showWarnings = F)
setwd("./Modellmatrix")

fwrite(dt$model.raw, paste0(datetime(), "_", dt$para$beverage, "_", dt$para$substance[dt$para$i], "_matrix.csv"), row.names = F, dec = ",", sep = ";")
dt$model.raw <- transfer_csv(csv.file = dt$model.raw)
dt$SL <- transfer_csv(csv.file = dt$SL)

# Plot ####
par( mfrow = c(1,1))
matplot(dt$para$wl[[1]]
        , t( dt$SL$spc[ grep(dt$para$substance[ dt$para$i ], dt$SL$data$Probe) , ])
        , type = "l", lty = 1, xlab = lambda, ylab = "AU", main = "SL vs Modellspektren"
        , col = "blue", xlim = c(190, 400))
matplot(dt$para$wl[[1]]
        , t( dt$model.raw$spc )
        , type = "l", lty = 1, xlab = lambda, ylab = "AU", main = "SL vs Modellspektren"
        , col = "red", add = T)
legend("topright", c(paste0("SL ", dt$para$substance[ dt$para$i]), "Ausmischung"), lty = 1, col = c("blue", "red"))

# PLS para ####
dt$para.pls$wl <- 200:300
dt$model.raw$data$Probe == dt$para$substance[dt$para$i]
dt$para.pls$wlr <- wlr_function(dt$para.pls$wl, dt$para.pls$wl, 10); nrow(dt$para.pls$wlr)
dt$para.pls$wlm <- wlr_function_multi(dt$para.pls$wl, dt$para.pls$wl, 10); nrow(dt$para.pls$wlm)
dt$para.pls$wl <- rbind.fill(dt$para.pls$wlm, dt$para.pls$wlr); nrow(dt$para.pls$wl); dt$para.pls$wlr <- NULL; dt$para.pls$wlm <- NULL
dt$para.pls$ncomp <- 6

# RAM ####
gc()

# PLS and LM ####
dt$pls$pls <- pls_function(csv_transfered = dt$model.raw
                           , substance = dt$para$substance[dt$para$i]
                           , wlr = dt$para.pls$wl
                           , ncomp = dt$para.pls$ncomp)

dt$pls$lm <- pls_lm_function(dt$pls$pls
                             , csv_transfered = dt$model.raw
                             , substance = dt$para$substance[dt$para$i]
                             , wlr = dt$para.pls$wl
                             , ncomp = dt$para.pls$ncomp)
# Prediction ####
dt$pls$pred <- lapply(dt$trs, function( x ) produktion_prediction(csv_transfered = x, pls_function_obj = dt$pls$pls, ncomp = dt$para.pls$ncomp))
dt$pls$merge <- lapply(dt$pls$pred, function( x ) merge_pls(pls_pred = x, pls_lm = dt$pls$lm, mean = c(dt$para$SOLL[dt$para$i] * c(.8, 1.2)), R2=.8))
dt$pls$merge <- lapply(dt$pls$merge, function( x ) x[ order(x$sd) , ])
lapply(dt$pls$merge, head)

if( length(dt$pls$merge) > 1){ 
  dt$pls$mergesite <- merge_pls_site(merge_pls_lm_predict_ls = dt$pls$merge, number = 2000, ncomp = dt$para.pls$ncomp)
  head(dt$pls$mergesite)} else{ 
    dt$pls$mergesite <- dt$pls$merge[[1]]
    head(dt$pls$mergesite)}
# Prediciton lin ####
dt$pls$pred.lin <- produktion_prediction(csv_transfered = dt$lin$trs, pls_function_obj = dt$pls$pls, ncomp = dt$para.pls$ncomp)

# Lin
dt$lin$diff <- print( diff ( range(dt$lin$trs$data$Dilution * dt$para$SOLL[dt$para$i] / 100) ) )

dt$pls$lin <- linearitaet_filter( linearitaet_prediction =  dt$pls$pred.lin$prediction
                                  , ncomp = dt$para.pls$ncomp
                                  , linearitaet_limit_1 = dt$lin$diff * .90
                                  , linearitaet_limit_2 = dt$lin$diff * 1.1
                                  , R_2 = .75
                                  , SOLL = dt$lin$trs$data$Dilution * dt$para$SOLL[dt$para$i] / 100
                                  , pls_merge = dt$pls$mergesite )

dat1 <- merge.data.frame(dt$pls$lin, dt$pls$mergesite)
dat1 <- dat1[order(dat1$sd, decreasing = F) , ]
head(dat1)
dat1 <- dat1[order(dat1$mad, decreasing = F) , ]
head(dat1)
dat1 <- dat1[dat1$spc != "spc" , ]
head(dat1)

# Prediciton ####
dt$mop$ncomp <- 6
dt$mop$wl1 <- 250
dt$mop$wl2 <- 270
dt$mop$wl3 <- 280
dt$mop$wl4 <- 300
dt$mop$spc <- "1st"
dt$mop$model <- pls_function(dt$model.raw, dt$para$substance[ dt$para$i ], data.frame(dt$mop$wl1, dt$mop$wl2, dt$mop$wl3, dt$mop$wl4), dt$mop$ncomp, spc = dt$mop$spc)
dt$mop$model  <- dt$mop$model [[grep(dt$mop$spc, names(dt$mop$model))[1]]][[1]]

dt$mop$pred <- lapply(dt$trs, function(x) pred_of_new_model(dt$model.raw
                                                            , dt$para$substance[ dt$para$i ]
                                                            , dt$mop$wl1
                                                            , dt$mop$wl2
                                                            , dt$mop$wl3, dt$mop$wl4
                                                            , dt$mop$ncomp
                                                            , dt$mop$spc
                                                            , x))
dt$mop$pred.lin <- pred_of_new_model(dt$model.raw
                                     , dt$para$substance[ dt$para$i ]
                                     , dt$mop$wl1
                                     , dt$mop$wl2
                                     , dt$mop$wl3, dt$mop$wl4
                                     , dt$mop$ncomp
                                     , dt$mop$spc
                                     , dt$lin$trs)

dt$mop$pred <- lapply(dt$mop$pred, function( x ) as.numeric(ma( x, 5)))
dt$mop$bias <- lapply(dt$mop$pred, function( x ) round( bias( median( x, na.rm = T), 0, dt$para$SOLL[ dt$para$i] ), 3))
dt$mop$bias
dt$mop$bias.lin <- round( bias( median( dt$mop$pred.lin, na.rm = T), 0, median(dt$lin$trs$data$Dilution * dt$para$SOLL[dt$para$i] / 100) ), 3)
dt$mop$bias.lin
dt$mop$pred <- mapply( function( x,y ) x - y
                       , x = dt$mop$pred
                       , y = dt$mop$bias
                       , SIMPLIFY = F)
dt$mop$pred.lin <- dt$mop$pred.lin - dt$mop$bias.lin

par( mfrow = c(1,1))
plot(dt$mop$pred.lin
     , xlab = "", ylab = dt$para$ylab[ dt$para$i ], main = dt$para$txt$loc.line[ i ]
     , ylim = dt$para$SOLL[ dt$para$i] * c(85, 105) / 100, axes = T
     , sub = paste("Bias =", dt$mop$bias[ i ]))
points(dt$lin$trs$data$Dilution * dt$para$SOLL[dt$para$i] / 100, col = "red")

par(mfrow = c(length( dt$mop$pred ), 1))
for(i in 1:length(dt$mop$pred)){
  plot(dt$mop$pred[[ i ]]
       , xlab = "", ylab = dt$para$ylab[ dt$para$i ], main = dt$para$txt$loc.line[ i ]
       , ylim = dt$para$SOLL[ dt$para$i] * c(85, 105) / 100, axes = F
       , sub = paste("Bias =", dt$mop$bias[ i ]))
  xaxisdate(dt$trs[[ i ]]$data$datetime)
  abline( h = dt$para$eingriff[[ dt$para$i ]], col = "orange", lty = 2 )
  abline( h = dt$para$sperr[[ dt$para$i ]], col = "red", lty = 2 )
}

keep.out.unsb(model = dt$model.raw, dt$mop$wl1, dt$mop$wl2, dt$mop$wl3, dt$mop$wl4)

# write to model data
model_parameter_write(dt$para$customer, NA, NA, dt$para$beverage, dt$para$substance[ dt$para$i ]
                      , NA
                      , dt$mop$ncomp
                      , dt$mop$wl1, dt$mop$wl2, dt$mop$wl3, dt$mop$wl4
                      , dt$mop$spc)
setwd(dt$wd.git)

