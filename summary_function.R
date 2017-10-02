#summary function

library(data.table)
library(stringr)
library(plyr)
library(dplyr)
library(dtplyr)
library(ggplot2)
library(bit64)
library(foreach)
library(doParallel)
library(reshape2)
library(xlsx)


wd <- "C:/Users/39492/Desktop/HEM S2D R"
setwd(wd)

control.file="test0.txt"


# read.control file reads the settings for the current S2D run

read.control.file = function(control.file) {
  if (is.null(control.file)) control.file <- "S2D_control_file.txt"
  lc <- str_length(control.file)
  if(!substr(control.file,lc-3,lc)=='.txt') control.file <- paste0(control.file,".txt")
  x <- fread(paste0("input/",control.file),header=FALSE)
  setnames(x,c("key","setting"))
  x$key          <- tolower(x$key)
  chem.list      <- list(c(x$setting[x$key=="chem"]))
  puc.list       <- list(c(x$setting[x$key=="puc"]))
  fug.file       <- x$setting[x$key=="fug.file"] 
  chem.file      <- x$setting[x$key=="chem.file"]
  chem.frac.file <- x$setting[x$key=="chem.frac.file"]
  puc.type.file  <- x$setting[x$key=="puc.type.file"]
  compart.file   <- x$setting[x$key=="compart.file"]
  puc.met.file   <- x$setting[x$key=="puc.met.file"]
  skin.area.file <- x$setting[x$key=="skin.area.file"]
  removal.file   <- x$setting[x$key=="removal.file"]
  vent.file      <- x$setting[x$key=="vent.file"]
  diary.prefix   <- x$setting[x$key=="diary.prefix"]
  run.name       <- x$setting[x$key=="run.name"]
  house.seed     <- x$setting[x$key=="house.seed"]
  puc.seed       <- x$setting[x$key=="puc.seed"]
  chem.seed      <- x$setting[x$key=="chem.seed"]
  init.seed      <- x$setting[x$key=="init.seed"]
  sum.type       <- x$setting[x$key=="sum.type"]
  if (length(init.seed)>0) { 
    house.seed   <- init.seed
    puc.seed     <- init.seed
    chem.seed    <- init.seed
  } else init.seed <- 0
  mode(house.seed)  <- "integer"
  mode(puc.seed)    <- "integer"
  mode(chem.seed)   <- "integer"
  first.house       <- x$setting[x$key=="first.house"]
  if (length(first.house)==0) first.house <- 1
  mode(first.house) <- "integer"
  last.house        <- x$setting[x$key=="last.house"]
  if (length(last.house)==0) last.house <- NA
  mode(last.house)  <- "integer"
  n.houses          <- x$setting[x$key=="n.houses"]
  if (length(n.houses)==0)   n.houses   <- NA
  mode(n.houses)    <- "integer"
  if (is.na(last.house) & !is.na(n.houses)) last.house <- first.house+n.houses-1
  if (is.na(n.houses) & !is.na(last.house)) n.houses   <- last.house-first.house+1
  comp.method       <- x$setting[x$key=="comp.method"]
  if (length(comp.method)==0) comp.method <- 1
  mode(comp.method) <- "integer" 
  puc.offset        <- x$setting[x$key=="puc.offset"]
  if (length(puc.offset)==0) puc.offset <- 0
  mode(puc.offset)  <- "integer"
  prog              <- substr(x$setting[x$key=="show.progress"],1,1)
  if (length(prog)==0)  prog <- "n"                     # default is no progress messages
  parallel          <- substr(x$setting[x$key=="parallel"],1,1)
  if (length(parallel)==0) parallel <- "n"              # default is not parallel
  save.r.objects    <- substr(x$setting[x$key=="save.r.objects"],1,1)
  if (length(save.r.objects)==0) save.r.objects <- "n"  # default is not to save
  out <- paste0("output/S2D/",run.name)
  g <- as.data.table(list(chem.list,sum.type,puc.list,fug.file,chem.file,chem.frac.file,puc.type.file,compart.file,puc.met.file,
                          skin.area.file,removal.file,vent.file,diary.prefix,run.name,n.houses,init.seed,prog,parallel,
                          puc.offset,first.house,last.house,comp.method,save.r.objects,house.seed,puc.seed,chem.seed,out))
  setnames(g,c("chem.list","sum.type","puc.list","fug.file","chem.file","chem.frac.file","puc.type.file","compart.file",
               "puc.met.file","skin.area.file","removal.file","vent.file","diary.prefix","run.name","n.houses",
               "init.seed","prog","parallel","puc.offset","first.house","last.house","comp.method","save.r.objects",
               "house.seed","puc.seed","chem.seed","out"))
  return(g)
}


read.vent.file = function(vent.file){
  if (is.null(vent.file)) vent.file <- g$vent.file
  x <- fread(paste0("input/",vent.file))
  setnames(x,tolower(names(x)))
  return(x)
}

# trimzero removes leading zeroes from CAS numbers

trimzero = function(x) {
  y <- x
  for (j in 1:length(x)) {
    i <- 1
    while(substr(x[j],i,i)=="0" & substr(x[j],i+1,i+1)!="_") {
      substr(y[j],i,i)<- " "
      i <- i+1
    }  
  }
  return(str_trim(y))
}

read.chem.props = function(chem.file,chem.list) {
  if (is.null(chem.file)) chem.file <- g$chem.file
  # Read chemical properties input file
  dt <- as.data.table(fread(paste0("input/",chem.file),na.strings=c("",". ","."," .","NA")))
  setnames(dt,names(dt),tolower(str_replace_all(names(dt),"_",".")))
  mode(dt$cas) <- "character"
  mode(dt$kp) <- "numeric"
  dt$cas <- trimzero(str_replace_all(dt$cas,"-","_"))
  dt$cas <- ifelse(substr(dt$cas,1,1)=="_",paste0("0",dt$cas),dt$cas)
  if(!exists("x",dt))               dt$x       <- 1:nrow(dt)
  if(is.na(dt[1]$x))                dt[1]$x    <- 1
  y <- lag(dt$x) + 1
  dt$x[is.na(dt$x)] <- y[is.na(dt$x)]
  if (length(chem.list)>0)  dt <- dt[dt$dtxsid %in% chem.list]
  dt <- unique(dt,by="dtxsid",fromLast=TRUE)
  if(nrow(dt)==0) cat("\n  No chemicals left in fugacity calcs \n")
  if(exists("name",dt))             setnames(dt,"name","chem.name")
  if(exists("vp.pa",dt))            setnames(dt,"vp.pa","vapor")
  if(exists("mw",dt))               setnames(dt,"mw","molwt")
  if(exists("water.sol.mg.l",dt))   dt$solub   <- dt$water.sol.mg.l/dt$molwt
  if(exists("log.kow",dt))          dt$kow     <- 10^dt$log.kow
  if(exists("half.sediment.hr",dt)) dt$decay.s <- 24*log(2)/dt$half.sediment.hr 
  if(exists("half.air.hr",dt))      dt$decay.a <- 24*log(2)/dt$half.air.hr 
  return(dt) 
}


# read.puc.types reads the .csv file listing the product type for each PUC

read.puc.types = function(puc.type.file,puc.list) {
  if (is.null(puc.type.file)) puc.type.file <- g$puc.type.file
  x <- fread(paste0("input/",puc.type.file))
  setnames(x,tolower(names(x)))
  if(length(puc.list>0)) {
    x <- x[x$source.id %in% puc.list] 
    for (i in 1:length(puc.list)) {
      if (!puc.list[i] %in% x$source.id) cat("  PUC ",puc.list[i]," not on PUC types file")
    }
  }
  x$code    <- toupper(x$code)
  puc.types <- x[x$code!="XXX"]
  return(puc.types)
}





cf <- read.control.file(control.file)

vent <- read.vent.file(cf$vent.file)

chemp <- read.chem.props(cf$chem.file,cf$chem.list)

puc <- read.puc.types(cf$puc.type.file,cf$puc.list)

#print ("00000000000000000000000")

#print (puc$)

##don't delete double hash
print("---------------------------------")
v <- as.array(vent$sex)
num <- length(v)
half<- num/2
sexStr <- vector()
sexStr <- append(sexStr,v[1])
sexStr <- append(sexStr,v[num])
sexStr <- paste(unlist(sexStr),collapse = '')

annual.info <- data.table(unlist(cf$chem.list),unlist(cf$puc.list),cf$last.house-cf$first.house+1,min(vent$minage),max(vent$maxage),max(vent$maxage)-min(vent$minage),sexStr,chemp$kp,chemp$chemical,chemp$casrn,puc$code,puc$product_type,keep.rownames = TRUE)
annual.tab <- transpose(annual.info)
row.names(annual.tab) <- c("dtxsid","PUC","#households","min_age","max_age","max_age-min_age","genders","Kp","chemicals","CASRN","code","product_name")


write.xlsx((annual.tab),file="C:/Users/39492/Desktop/HEM S2D R/sum.xlsx",sheetName="sheet1",row.names=TRUE, col.names = FALSE)
write.xlsx((annual.tab),file="C:/Users/39492/Desktop/HEM S2D R/sum.xlsx",sheetName="sheet2",row.names=TRUE, col.names = FALSE,append = TRUE)
write.xlsx((annual.tab),file="C:/Users/39492/Desktop/HEM S2D R/sum.xlsx",sheetName="sheet3",row.names=TRUE, col.names = FALSE,append = TRUE)


