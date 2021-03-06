# beverage parameter ####
setwd(this.path::this.dir())
dir( pattern = "Rsource" )
source.file <- print(dir( pattern = "Rsource" )[ length( dir( pattern = "Rsource" ))])
source( paste0(getwd(), "/", source.file) )

Acid <- T # Acid column in MTX file ? 

# read in data ####
setwd(dt$wd)
setwd("./Modellerstellung")
setwd(paste0("./", dt$para$model.date[1], "_", dt$para$model.pl[1]))
setwd("./spc")
setwd( dt$wd.mastermodel <- getwd() )

# data from 
# read Q-xx-MTX
setwd("..")
require(openxlsx)
dt$qxxmtx <- print( dir( pattern = "Q-xx-MTX-")[length(dir( pattern = "Q-xx-MTX-"))] )
istprozent <- openxlsx::read.xlsx(dt$qxxmtx, sheet = "p_IST_g")
istprozent <- rbind.fill(istprozent)
names(istprozent)[1] <- "Probe_Anteil"

istprozent$Probe_Anteil <- gsub("Acid", "acid", istprozent$Probe_Anteil)

istprozent <- istprozent[!is.na(istprozent$Citric_Acid) , ]

if( Acid == T){
  Acid <- openxlsx::read.xlsx(dt$qxxmtx, sheet = "SA")
  Acid <- Acid[ - which(is.na(Acid$`SL.(IST)`))[1] , ]
  Acid <- as.numeric(gsub(",",".",Acid$NaOH[ - 1]))
  istprozent <- cbind(istprozent, Acid)
}

istprozent$Coffein[istprozent$SK > 100] <- istprozent$SK[istprozent$SK > 100]

# Model parameter ####
setwd(dt$wd.mastermodel)
dt$mod$para <-  colnames(istprozent)[ - c(1) ]
dt$mod$para
dt$mod$para[1] <- "Citric_acid"

# read spc files ####
meapre_s <- data.table()
mea_s <- data.table()

meapre_s$files_spc <- dir(pattern=".spc", recursive = T)[grep("_c0",dir(pattern=".spc", recursive = T))]
meapre_s$file_path <- paste(paste(getwd(),"//",sep=""),meapre_s$files_spc,sep="")
ifelse(length(meapre_s$file_path)>1,
       meapre_s$spccomplete <- lapply(meapre_s$file_path,read.spc,keys.hdr2data = T),
       meapre_s$spccomplete[[1]] <- lapply(meapre_s$file_path,read.spc,keys.hdr2data = T))
meapre_s$length <- sapply(meapre_s$spccomplete,function(x) length(x$ffirst))
ifelse(length(meapre_s$file_path)>1,
       meapre_s$wavelength <- data.frame(sapply(meapre_s$spccomplete,function(x) x@wavelength[ grep(dt$para$wl1, x@wavelength) : grep(dt$para$wl2, x@wavelength) ])),
       meapre_s$wavelength[[1]] <- data.frame(sapply(meapre_s$spccomplete,function(x) x@wavelength[ grep(dt$para$wl1, x@wavelength) : grep(dt$para$wl2, x@wavelength) ])))

spc <- data.frame(row.names = min(dt$para$wl1):max(dt$para$wl2))
for(i in 1:length(meapre_s$file_path)){
  for(j in 1:meapre_s$length[i]){
    spc <- cbind(spc,meapre_s$spccomplete[[i]]$spc[j,][grep(dt$para$wl1, names(meapre_s$spccomplete[[i]]$spc[j,])) : grep(dt$para$wl2, names(meapre_s$spccomplete[[i]]$spc[j,]))])}}

wavelength <- data.frame(row.names = dt$para$wl[[1]])
for(i in 1:length(meapre_s$file_path)){
  for(j in 1:meapre_s$length[i]){
    wavelength <- cbind(wavelength,meapre_s$wavelength[[i]])}}

meapre_s$timetoformat <- sapply(meapre_s$spccomplete,function(x) as.character(x@data$fdate[1]))
meapre_s$time <- as.POSIXct(meapre_s$timetoformat,origin="Europe/Berlin")
meapre_s$xlab <- sapply(meapre_s$spccomplete,function(x) as.character(x@data$fxtype[1]))

mea2files <- list()
mea2time <- list()
mea2xlab <- list()
for(i in 1:length(meapre_s$files_spc)){
  mea2files[[i]] <- rep(meapre_s$files_spc[i],meapre_s$length[i])
  mea2time[[i]] <- rep(meapre_s$time[i],meapre_s$length[i])
  mea2xlab[[i]] <- rep(meapre_s$xlab[i],meapre_s$length[i])
}

mea2files <- unlist(mea2files)
mea2time <- unlist(mea2time)
mea2time <- anytime(mea2time)
mea2xlab <- unlist(mea2xlab)

mea_s$files_spc <- mea2files;rm(mea2files)
mea_s$time <- mea2time;rm(mea2time)
mea_s$xlab <- mea2xlab;rm(mea2xlab)

mea_s$spc <- spc;rm(spc)
mea_s$wavelength <- wavelength;rm(wavelength)

setorder(mea_s,origin=time)
setorder(meapre_s,origin=time)

# match data with spc ####
rm(i,j,meapre_s)

mea_s_spc <- t(data.frame(mea_s$spc))
mea_s_spc <- cbind(files_spc=mea_s$files_spc,mea_s_spc)
mea_s_spc <- data.frame(mea_s_spc)
mea_s_spc <- cbind(Probe=NA, Probe_Anteil = NA, mea_s_spc)

# Probe
mea_s_spc$Probe[grep("_SL_",mea_s_spc$files_spc)] <- "SL"

for(i in 1:length(dt$mod$para))
  mea_s_spc$Probe[ grep( paste0( "_", dt$mod$para[i], "_") , mea_s_spc$files_spc) ] <- dt$mod$para[i]

mea_s_spc$Probe[grep("FG",mea_s_spc$files_spc)] <- "FG"

mea_s_spc$Probe[which(is.na(mea_s_spc$Probe))]
mea_s_spc$Probe

# Probe Anteil
mea_s_spc$Probe_Anteil[grep("_FG_",mea_s_spc$files_spc)] <- "FG_100%"
istprozent$Probe_Anteil[ grep(paste("FG"), istprozent$Probe_Anteil) ] <- paste0("FG", "_100%")

for(i in 1:length(dt$mod$para)){
  
  para_prozent <- gsub(" ", "", gsub("_", ",", gsub("%", "", gsub(dt$mod$para[i], "", istprozent[ , 1][ grep( dt$mod$para[i] , istprozent[ , 1]) ]))))
  
  para_prozent_flag <- formatC(as.numeric(para_prozent), width = 2, format = "d", flag = "0")
  
  for(j in 1:length(para_prozent)){
    
    mea_s_spc$Probe_Anteil[ grep( paste0("_", dt$mod$para[i], "_", para_prozent[j]), mea_s_spc$files_spc)] <- paste0( dt$mod$para[i], "_", para_prozent_flag[j], "%")
    
    istprozent$Probe_Anteil[ grep(paste(dt$mod$para[i], para_prozent[j], "%"), istprozent$Probe_Anteil) ] <- paste0(dt$mod$para[i], "_", para_prozent_flag[j], "%")
    
  }
}

mea_s_spc$Probe_Anteil[grep("_SL_",mea_s_spc$files_spc)] <- "SL"
mea_s_spc$files_spc[which(is.na(mea_s_spc$Probe_Anteil))]

# merge ####
istprozentmeaspc <- merge.data.frame(istprozent,mea_s_spc,by=intersect(names(istprozent), names(mea_s_spc)), all=T)
nrow(istprozentmeaspc)
View(istprozentmeaspc)

istprozentmeaspc <- istprozentmeaspc[order(istprozentmeaspc$Probe_Anteil), ]

istprozentmeaspc <- istprozentmeaspc[,.moveme(names(istprozentmeaspc), "Probe first; files_spc first")]
istprozentmeaspc$files_spc <- basename(istprozentmeaspc$files_spc)
names(istprozentmeaspc) <- gsub("X","",names(istprozentmeaspc))

for(i in dt$para$wl[[1]]) istprozentmeaspc[ , which(names(istprozentmeaspc) == i)] <- as.numeric(as.character(istprozentmeaspc[ , which(names(istprozentmeaspc) == i)]))
if(any(duplicated(istprozentmeaspc))) istprozentmeaspc <- istprozentmeaspc[ - which(duplicated(istprozentmeaspc)) , ]

# write ####
setwd(dt$wd.mastermodel)
setwd("..")
setwd("./csv")

write.csv2(istprozentmeaspc, paste0(dt$para$model.date[1], "_", dt$para$model.pl[1], "_", dt$para$beverage, "_Modellspektren_Ausmischung_match.csv"), row.names = F)
