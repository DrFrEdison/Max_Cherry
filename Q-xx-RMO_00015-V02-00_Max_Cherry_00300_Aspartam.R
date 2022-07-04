# beverage parameter ####
setwd(this.path::this.dir())
dir( pattern = "Rsource" )
source.file <- print("Rsource_Max_Cherry_mtx_mop_val_V01.R")
source( paste0(getwd(), "/", source.file) )

# spectra ####
dt$para$substance
dt$para$i = 3
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
dt$para.pls$wl <- 200:280
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
dt$pls$merge <- lapply(dt$pls$pred, function( x ) merge_pls(pls_pred = x, pls_lm = dt$pls$lm, mean = c(dt$para$SOLL[dt$para$i] * c(.7, 1.3)), R2=.8))
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
dt$lin$diff <- print( diff ( range(dt$lin$trs$data[ , grep( dt$para$substance[ dt$para$i ], colnames( dt$lin$trs$data ))]) ) )

dt$pls$lin <- linearitaet_filter( linearitaet_prediction =  dt$pls$pred.lin$prediction
                                  , ncomp = dt$para.pls$ncomp
                                  , linearitaet_limit_1 = dt$lin$diff * .80
                                  , linearitaet_limit_2 = dt$lin$diff * 1.2
                                  , R_2 = .75
                                  , SOLL = dt$lin$trs$data[ , grep( dt$para$substance[ dt$para$i ], colnames( dt$lin$trs$data ))]
                                  , pls_merge = dt$pls$mergesite )

dat1 <- merge.data.frame(dt$pls$lin, dt$pls$mergesite)
dat1 <- dat1[order(dat1$sd, decreasing = F) , ]
head(dat1)
dat1 <- dat1[order(dat1$mad, decreasing = F) , ]
head(dat1)
# dat1 <- dat1[dat1$spc != "spc" , ]
# head(dat1)

# Prediciton ####
dt$mop$ncomp <-2
dt$mop$wl1 <- 210
dt$mop$wl2 <- 220
dt$mop$wl3 <- 270
dt$mop$wl4 <- 280
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
dt$mop$bias.lin <- round( bias( median( dt$mop$pred.lin, na.rm = T), 0, median( dt$lin$trs$data[ , grep( dt$para$substance[ dt$para$i ], colnames( dt$lin$trs$data ))] )), 3)
dt$mop$bias.lin
dt$mop$pred <- mapply( function( x,y ) x - y
                       , x = dt$mop$pred
                       , y = dt$mop$bias
                       , SIMPLIFY = F)
dt$mop$pred.lin <- dt$mop$pred.lin - dt$mop$bias.lin

par( mfrow = c(1,1))
plot(dt$mop$pred.lin
     , xlab = "", ylab = dt$para$ylab[ dt$para$i ], main = dt$para$txt$loc.line[ i ]
     , ylim = dt$para$SOLL[ dt$para$i] * c(75, 90) / 100, axes = T
     , sub = paste("Bias =", dt$mop$bias[ i ]))
points(dt$lin$trs$data[ , grep( dt$para$substance[ dt$para$i ], colnames( dt$lin$trs$data ))], col = "red")

sd( tapply(dt$mop$pred.lin, factor(dt$lin$trs$data$Dilution), mean) - 
      tapply(dt$lin$trs$data$Acesulfam, factor(dt$lin$trs$data$Dilution), mean) )


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
setwd(wd$data)
dt$model.overview <- read_ods("dt_model_overview.ods")

head10(dt$model.overview)
head(dt$model.overview)

length(which(dt$model.overview$beverage == dt$para$beverage))
length(which(dt$model.overview$beverage == dt$para$beverage & dt$model.overview$substance == dt$para$substance[ dt$para$i ]))

if(length(which(dt$model.overview$beverage == dt$para$beverage & dt$model.overview$substance == dt$para$substance[ dt$para$i ])) == 0){
  dt$model.overview <- rbind(dt$model.overview
                             , data.frame(customer = dt$para$customer
                                          , location = NA
                                          , unit = NA
                                          , beverage = dt$para$beverage
                                          , LG = dt_customer[ dt_customer$location == as.character(dt$para$location) , "LG"][1]
                                          , Parameter = dt$para$substance[ dt$para$i ]
                                          , wl1 = dt$mop$wl1
                                          , wl2 = dt$mop$wl2
                                          , wl3 = dt$mop$wl3
                                          , wl4 = dt$mop$wl4
                                          , PC = dt$mop$ncomp
                                          , transform = dt$mop$spc
                                          , p = NA, n1 = NA, n2 = NA, seg = NA, Slope = NA, subset = NA)
  )
}

if(length(which(dt$model.overview$beverage == dt$para$beverage & dt$model.overview$substance == dt$para$substance[ dt$para$i ])) == 1){
  
  dt$model.overview[which(dt$model.overview$beverage == dt$para$beverage & dt$model.overview$substance == dt$para$substance[ dt$para$i ]) , "wl1"] <- dt$mop$wl1
  dt$model.overview[which(dt$model.overview$beverage == dt$para$beverage & dt$model.overview$substance == dt$para$substance[ dt$para$i ]) , "wl2"] <- dt$mop$wl2
  dt$model.overview[which(dt$model.overview$beverage == dt$para$beverage & dt$model.overview$substance == dt$para$substance[ dt$para$i ]) , "wl3"] <- dt$mop$wl3
  dt$model.overview[which(dt$model.overview$beverage == dt$para$beverage & dt$model.overview$substance == dt$para$substance[ dt$para$i ]) , "wl4"] <- dt$mop$wl4
  dt$model.overview[which(dt$model.overview$beverage == dt$para$beverage & dt$model.overview$substance == dt$para$substance[ dt$para$i ]) , "ncomp"] <- dt$mop$ncomp
  dt$model.overview[which(dt$model.overview$beverage == dt$para$beverage & dt$model.overview$substance == dt$para$substance[ dt$para$i ]) , "spc"] <- dt$mop$spc
}

dt$model.overview <- dt$model.overview[ order(dt$model.overview$customer, dt$model.overview$beverage, dt$model.overview$substance),]
write_ods(x = dt$model.overview, path = "dt_model_overview.ods", overwrite = T)

setwd("./model")
setwd(paste0("./", dt$para$customer))

fwrite(x = cbind(dt$model.raw$data, dt$model.raw$spc)
       , file = paste0( paste(dt$para$customer
                              , dt$para$beverage
                              , dt$para$substance[ dt$para$i ]
                              , paste0("LG", as.character(dt_customer[ dt_customer$location == as.character(dt$para$location), "LG"][1]))
                              , sep = "_"), ".csv")
       , sep = ";", dec = ",", na = NA)
setwd(dt$wd.git)



