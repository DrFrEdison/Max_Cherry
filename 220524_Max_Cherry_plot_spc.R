# beverage parameter ####
setwd(this.path::this.dir())
dir( pattern = "Rsource" )
source.file <- print(dir( pattern = "Rsource" )[ length( dir( pattern = "Rsource" ))])
source( paste0(getwd(), "/", source.file) )

# Plot functions for model spectra ####
setwd(dt$wd)
setwd("./Modellerstellung")
setwd(paste0("./", dt$para$model.date[1], "_", dt$para$model.pl[1]))
setwd("./spc")
setwd( dt$wd.mastermodel <- getwd() )

# read Q-xx-MTX ####
setwd("..")
require(openxlsx)
dir()
dt$qxxmtx <- print( dir( pattern = "Q-xx-MTX-")[length(dir( pattern = "Q-xx-MTX-"))] )
dt$qxxmtx <- openxlsx::read.xlsx(dt$qxxmtx, sheet = "Zugabe_p")
dt$qxxmtx
head(dt$qxxmtx)

# find parameter in Q-xx-MTX ####
dt$parameter <- colnames(dt$qxxmtx)[ - c(1,2) ]
dt$parameter <- c(dt$parameter, "FG")
dt$parameter
dt$parameter[1] <- "Citric_acid"

# set validation parameter ####
dt$valparameter <- dt$parameter
dt$valparameter <- dt$valparameter[ - grep("FG", dt$valparameter)]

# create directory structure for spc ####
for(i in 1:length(dt$parameter)){
  setwd(dt$wd.mastermodel)
  dir.create(dt$parameter[i], showWarnings = F)
  setwd( paste0( "./", dt$parameter[i]))
  dir.create("Ausmischung", showWarnings = F)
  if(dt$parameter[i] != "H2O" & dt$parameter[i] != "FG")  dir.create("SL", showWarnings = F)
}

# create directory structure for validation set ####
for(i in 1:length(dt$parameter)){
  setwd(dt$wd.mastermodel)
  setwd("..")
  setwd("./Validierungsset")
  dt$wd.valset <- getwd()
  dir.create(dt$parameter[i], showWarnings = F)
}

# move spectra from Tidas ####
setwd(wd$tidas)
dt$spcfiles <- dir( pattern = ".spc$")
dt$spcfiles
dt$parameter
if( length(dt$spcfiles) > 0 )
  for(i in 1:length(dt$parameter)){
    
    # Ausmischung spc without _VAS_ and without "_SL"
    dt$copy$ausmischung <- paste0(wd$tidas, "/"
                                  , grep("_VAS_", grep("_SL_", grep(dt$parameter[i], dt$spcfiles, value = T), invert = T, value = T), invert = T, value = T))
    
    if(length(dt$copy$ausmischung) > 0)
      file.copy(from = dt$copy$ausmischung
                , to = paste0(dt$wd.mastermodel, "/", dt$parameter[i], "/Ausmischung/", basename(dt$copy$ausmischung))
                , overwrite = T
                #, recursive = F
      )
    
    # SL spc without _VAS_ and without Ausmischung
    dt$copy$SL <- paste0(wd$tidas, "/"
                         , grep("_VAS_", grep("_SL_", grep(dt$parameter[i], dt$spcfiles, value = T), invert = F, value = T), invert = T, value = T))
    
    if(length(dt$copy$SL) > 0)
      file.copy(from = dt$copy$SL
                , to = paste0(dt$wd.mastermodel, "/", dt$parameter[i], "/SL/", basename(dt$copy$SL))
                , overwrite = T
                , recursive = F)
    
    # Ausmischung spc with _VAS_
    dt$copy$VAS <- paste0(wd$tidas, "/"
                          , grep("_VAS_", grep("_SL_", grep(dt$valparameter[i], dt$spcfiles, value = T), invert = T, value = T), invert = F, value = T))
    
    if(length(dt$copy$VAS) > 0)
      file.copy(from = dt$copy$VAS
                , to = paste0(dt$wd.valset, "/", dt$valparameter[i], "/", basename(dt$copy$VAS))
                , overwrite = T
                , recursive = F)
  }

# Plot and write spectra ####
dt$baseline = NA
dt$pngplot <- F
dt$plotlyplot <- T
dt$filestext <- NA
dt$colp <- NA
dt$subfiles <- NA
spc_daten <- list()
dt$recursive <- T

dt$fileslist <-  list.files(dt$wd.mastermodel, pattern = ".spc$", recursive = T)
if(length( dt$fileslist > 0)){
  
  dt$filestext <- substr(dt$fileslist
                         , unlist(gregexpr("_DT", dt$fileslist, ignore.case = F)) + 8
                         , unlist(gregexpr("_c01_", dt$fileslist, ignore.case = F)) - 1)
  dt$filestext <- dt$filestext [which(nchar(dt$filestext) > 0)]
  
  spc_daten <-  read_spc_files(directory = dt$wd.mastermodel
                               , baseline = dt$baseline
                               , pngplot = dt$pngplot
                               , plotlyplot = dt$plotlyplot
                               , recursive = dt$recursive
                               , filestext = dt$filestext
                               , colp = dt$colp
                               , subfiles = dt$subfiles)
  
  write_spc_files(spc_daten$au, write = T, filename = paste0(.date(), "_", dt$para$beverage, "_", "spc"), return_R = F)
  
  if(length(dir( pattern = paste0(.date(), "_ref.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.html") ), paste0(.date(), "_", dt$para$beverage, "_", "ref.html"))
  if(length(dir( pattern = paste0(.date(), "_drk.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.html") ), paste0(.date(), "_", dt$para$beverage, "_", "drk.html"))
  if(length(dir( pattern = paste0(.date(), "_spc.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.html") ), paste0(.date(), "_", dt$para$beverage, "_", "spc.html"))
  if(length(dir( pattern = paste0(.date(), "_trans.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.html") ), paste0(.date(), "_", dt$para$beverage, "_", "trans.html"))
  
  if(length(dir( pattern = paste0(.date(), "_ref.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "ref.csv"))
  if(length(dir( pattern = paste0(.date(), "_drk.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "drk.csv"))
  if(length(dir( pattern = paste0(.date(), "_spc.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "spc.csv"))
  if(length(dir( pattern = paste0(.date(), "_trans.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "trans.csv"))
  
}

# Plot and write subspectra ####
dt$parameter
for(i in 1:length(dt$parameter)){
  # for(i in 7){
  setwd(dt$wd.mastermodel)
  setwd( paste0( "./", dt$parameter[i]))
  
  dt$fileslist <-  list.files(getwd(), pattern = ".spc$", recursive = T)
  if(length( dt$fileslist > 0)){
    
    dt$filestext <- substr(dt$fileslist
                           , unlist(gregexpr("_DT", dt$fileslist, ignore.case = F)) + 8
                           , unlist(gregexpr("_c01_", dt$fileslist, ignore.case = F)) - 1)
    dt$filestext <- dt$filestext [which(nchar(dt$filestext) > 0)]
    
    spc_daten <-  read_spc_files(directory = getwd()
                                 , baseline = dt$baseline
                                 , pngplot = dt$pngplot
                                 , plotlyplot = dt$plotlyplot
                                 , recursive = T
                                 , filestext = dt$filestext
                                 , colp = dt$colp
                                 , subfiles = dt$subfiles)
    
    if(length(dir( pattern = paste0(.date(), "_ref.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "ref.html"))
    if(length(dir( pattern = paste0(.date(), "_drk.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "drk.html"))
    if(dt$parameter[i] != "FG" | dt$parameter[i] != "H2O") if(length(dir( pattern = paste0(.date(), "_spc.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_und_SL_", "spc.html"))
    if(dt$parameter[i] == "FG" | dt$parameter[i] == "H2O") if(length(dir( pattern = paste0(.date(), "_spc.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "spc.html"))
    if(length(dir( pattern = paste0(.date(), "_trans.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "trans.html"))
    
    if(length(dir( pattern = paste0(.date(), "_ref.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "ref.csv"))
    if(length(dir( pattern = paste0(.date(), "_drk.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "drk.csv"))
    if(length(dir( pattern = paste0(.date(), "_spc.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "spc.csv"))
    if(length(dir( pattern = paste0(.date(), "_trans.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "trans.csv"))
  }
  
  if(dt$parameter[i] != "FG"){  
    
    setwd("./Ausmischung")
    
    dt$fileslist <-  list.files(getwd(), pattern = ".spc$", recursive = T)
    if(length( dt$fileslist > 0)){
      
      dt$filestext <- substr(dt$fileslist
                             , unlist(gregexpr("_DT", dt$fileslist, ignore.case = F)) + 8
                             , unlist(gregexpr("_c01_", dt$fileslist, ignore.case = F)) - 1)
      dt$filestext <- dt$filestext [which(nchar(dt$filestext) > 0)]
      
      spc_daten <-  read_spc_files(directory = getwd()
                                   , baseline = dt$baseline
                                   , pngplot = dt$pngplot
                                   , plotlyplot = dt$plotlyplot
                                   , recursive = F
                                   , filestext = dt$filestext
                                   , colp = dt$colp
                                   , subfiles = dt$subfiles)
      
      if(length(dir( pattern = paste0(.date(), "_ref.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "ref.html"))
      if(length(dir( pattern = paste0(.date(), "_drk.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "drk.html"))
      if(length(dir( pattern = paste0(.date(), "_spc.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "spc.html"))
      if(length(dir( pattern = paste0(.date(), "_trans.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "trans.html"))
      
      if(length(dir( pattern = paste0(.date(), "_ref.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "ref.csv"))
      if(length(dir( pattern = paste0(.date(), "_drk.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "drk.csv"))
      if(length(dir( pattern = paste0(.date(), "_spc.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "spc.csv"))
      if(length(dir( pattern = paste0(.date(), "_trans.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "trans.csv"))
      
    }
  }
  
  if(dt$parameter[i] != "H2O" & dt$parameter[i] != "FG"){  
    setwd(dt$wd.mastermodel)
    setwd( paste0( "./", dt$parameter[i]))
    setwd("./SL")
    
    dt$fileslist <-  list.files(getwd(), pattern = ".spc$", recursive = T)
    if(length( dt$fileslist > 0)){
      
      dt$filestext <- substr(dt$fileslist
                             , unlist(gregexpr("_DT", dt$fileslist, ignore.case = F)) + 8
                             , unlist(gregexpr("_c01_", dt$fileslist, ignore.case = F)) - 1)
      dt$filestext <- dt$filestext [which(nchar(dt$filestext) > 0)]
      
      spc_daten <-  read_spc_files(directory = getwd()
                                   , baseline = dt$baseline
                                   , pngplot = dt$pngplot
                                   , plotlyplot = dt$plotlyplot
                                   , recursive = F
                                   , filestext = dt$filestext
                                   , colp = dt$colp
                                   , subfiles = dt$subfiles)
      
      if(length(dir( pattern = paste0(.date(), "_ref.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "ref.html"))
      if(length(dir( pattern = paste0(.date(), "_drk.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "drk.html"))
      if(length(dir( pattern = paste0(.date(), "_spc.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_SL_", "spc.html"))
      if(length(dir( pattern = paste0(.date(), "_trans.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.html") ), paste0(.date(), "_", dt$para$beverage, "_", dt$parameter[i], "_", "trans.html"))
      
      if(length(dir( pattern = paste0(.date(), "_ref.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "ref.csv"))
      if(length(dir( pattern = paste0(.date(), "_drk.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "drk.csv"))
      if(length(dir( pattern = paste0(.date(), "_spc.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "spc.csv"))
      if(length(dir( pattern = paste0(.date(), "_trans.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "trans.csv"))
      
    }
  }  
}

# Plot and write  spectra VAS ####
setwd(dt$wd.valset)

dt$fileslist <-  list.files(getwd(), pattern = ".spc$", recursive = T)

dt$filestext <- substr(dt$fileslist
                       , unlist(gregexpr("_DT", dt$fileslist, ignore.case = F)) + 8
                       , unlist(gregexpr("_c01_", dt$fileslist, ignore.case = F)) - 1)
dt$filestext <- dt$filestext [which(nchar(dt$filestext) > 0)]

if(length( dt$fileslist > 0))spc_daten <-  read_spc_files(directory = getwd()
                                                          , baseline = dt$baseline
                                                          , pngplot = dt$pngplot
                                                          , plotlyplot = dt$plotlyplot
                                                          , recursive = T
                                                          , filestext = dt$filestext
                                                          , colp = dt$colp
                                                          , subfiles = dt$subfiles)

write_spc_files(spc_daten$au, write = T, filename = paste0(.date(), "_", dt$para$beverage, "_", "VAS_spc"), return_R = F)

if(length(dir( pattern = paste0(.date(), "_ref.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.html") ), paste0(.date(), "_", dt$para$beverage, "_", "VAS_ref.html"))
if(length(dir( pattern = paste0(.date(), "_drk.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.html") ), paste0(.date(), "_", dt$para$beverage, "_", "VAS_drk.html"))
if(length(dir( pattern = paste0(.date(), "_spc.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.html") ), paste0(.date(), "_", dt$para$beverage,  "_VAS_spc.html"))
if(length(dir( pattern = paste0(.date(), "_trans.html") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.html") ), paste0(.date(), "_", dt$para$beverage,  "_VAS_trans.html"))

if(length(dir( pattern = paste0(.date(), "_ref.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_ref.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "ref.csv"))
if(length(dir( pattern = paste0(.date(), "_drk.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_drk.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "drk.csv"))
if(length(dir( pattern = paste0(.date(), "_spc.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_spc.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "_VAS_spc.csv"))
if(length(dir( pattern = paste0(.date(), "_trans.csv") )) > 0) file.rename(dir( pattern = paste0(.date(), "_trans.csv") ), paste0(.date(), "_", dt$para$beverage, "_", "trans.csv"))


